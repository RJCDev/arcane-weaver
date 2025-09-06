using System;
using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

// public CommandAttribute(Channels channel = Channels.Reliable, bool runLocal = false, bool requireAuthority = false)


//example: TellMove(Vector2 input)

namespace ArcaneWeaver;

public class Command
{
    // <summary>
    /// Generates a (Pack_MethodName()) that packs the arguments that are sent into an Relay RPC, as well as the packet type (RPC = 1),
    /// and the RPC data.
    /// </summary>
    internal static MethodDefinition GeneratePackAndSendMethod(TypeDefinition component, MethodDefinition rpc, int methodHash)
    {

        // Get From MethodRPC Attribute args
        var rpcAttr = rpc.CustomAttributes.First(x => x.AttributeType.FullName == "ArcaneNetworking.CommandAttribute");
        var channelAttrib = rpcAttr.ConstructorArguments[0].Value;

        // Import TypeReferences
        var clientType = Weaver.GetRef("ArcaneNetworking.Client");
        var messageHandlerType = Weaver.GetRef("ArcaneNetworking.MessageHandler");
        var netComponentType = Weaver.GetRef("ArcaneNetworking.NetworkedComponent");
        var netNodeType = Weaver.GetRef("ArcaneNetworking.NetworkedNode");
        var writerType = Weaver.GetRef("ArcaneNetworking.NetworkWriter");
        var poolType = Weaver.GetRef("ArcaneNetworking.NetworkPool");
        var messageLayerType = Weaver.GetRef("ArcaneNetworking.MessageLayer");
        var channelsEnumType = Weaver.GetRef("ArcaneNetworking.Channels");
        var connectionType = Weaver.GetRef("ArcaneNetworking.NetworkConnection");


        // Get FieldReferences
        var serverConnectionType = Weaver.Assembly.ImportReference(
            clientType.Resolve().Fields.First(m => m.Name == "serverConnection" && m.IsStatic)
        );
        var serverRemoteID = Weaver.Assembly.ImportReference(
            connectionType.Resolve().Methods.First(m => m.Name == "GetRemoteID")
        );

        var netNodeField = Weaver.Assembly.ImportReference(
            netComponentType.Resolve().Fields.First(f => f.Name == "NetworkedNode")
        );
        var netIDField = Weaver.Assembly.ImportReference(
            netNodeField.FieldType.Resolve().Fields.First(f => f.Name == "NetID")
        );
        var messageLayerActiveField = Weaver.Assembly.ImportReference(
            messageLayerType.Resolve().Fields.First(f => f.Name == "Active")
        );

        // Get MethodReferences
        var getComponentIndexMethod = Weaver.Assembly.ImportReference(
            netComponentType.Resolve().Methods.First(m => m.Name == "GetIndex")
        );

        var writeMethod = Weaver.Assembly.ImportReference(
            writerType.Resolve().Methods.First(m => m.Name == "Write" && m.HasGenericParameters && m.Parameters.Count == 1)
        );
        var poolGetWriterMethod = Weaver.Assembly.ImportReference(
            poolType.Resolve().Methods.First(m => m.Name == "GetWriter" && m.IsStatic && m.Parameters.Count == 0)
        );

        // Retrieve single send enqueue method
        var enqueueMethod =  messageHandlerType.Resolve().Methods.First(m => m.Name == "Enqueue" && m.Parameters.Count == 3);

        // Create pack method
        var packMethod = new MethodDefinition(
            "Pack_" + rpc.Name,
            MethodAttributes.Public,
            Weaver.Assembly.TypeSystem.Void
        );

        // Add same params as original RPC
        foreach (var p in rpc.Parameters)
            packMethod.Parameters.Add(new ParameterDefinition(p.Name, ParameterAttributes.None, Weaver.Assembly.ImportReference(p.ParameterType)));

        ////////////// WE ARE WEAVING THE PACK METHOD NOW /////////////////

        var il = packMethod.Body.GetILProcessor(); // Start Method Processing
        packMethod.Body.InitLocals = true;

        /////// WE ARE NOW WRITING TO THE NETWORK WRITER BUFFER /////////

        // Writer orderflow:
        // rpcTypeByte -> methodHash -> callerNetID -> callerComponentIndex -> args (arbitrary)
        
        // Load Writer object
        var writerVar = new VariableDefinition(writerType);
        packMethod.Body.Variables.Add(writerVar);

        // Call our GetWriter() method to retrieve the NetworkWriter from the pool
        il.Emit(OpCodes.Call, poolGetWriterMethod);
        il.Emit(OpCodes.Stloc, writerVar); // Allocate a writer variable

        // Write network message type to writer (RPC = 1)
        var writePacketTypeGeneric = new GenericInstanceMethod(writeMethod);
        writePacketTypeGeneric.GenericArguments.Add(Weaver.Assembly.ImportReference(typeof(byte)));
        il.Emit(OpCodes.Ldloc, writerVar);
        il.Emit(OpCodes.Ldc_I4_S, (sbyte)1);
        il.Emit(OpCodes.Callvirt, writePacketTypeGeneric); // Push Packet byte

        // Write RPC Method ID hash to writer
        var writePacketHashGeneric = new GenericInstanceMethod(writeMethod);
        writePacketHashGeneric.GenericArguments.Add(Weaver.Assembly.ImportReference(typeof(int)));
        il.Emit(OpCodes.Ldloc, writerVar);
        il.Emit(OpCodes.Ldc_I4, methodHash);
        il.Emit(OpCodes.Callvirt, writePacketHashGeneric); // Call generic

        var writeCallerNetIDGeneric = new GenericInstanceMethod(writeMethod);
        writeCallerNetIDGeneric.GenericArguments.Add(Weaver.Assembly.ImportReference(typeof(uint)));
        il.Emit(OpCodes.Ldloc, writerVar);
        il.Emit(OpCodes.Ldarg_0);  // Load "this" (NetworkedComponent)
        il.Emit(OpCodes.Ldfld, netNodeField);  // Load NetworkedNode from the abstract NetworkedComponent
        il.Emit(OpCodes.Ldfld, netIDField);  // Load NetID from NetworkedNode
        il.Emit(OpCodes.Call, writeCallerNetIDGeneric); // Push Caller Indx

        // Write component index
        var writeCompIndexGeneric = new GenericInstanceMethod(writeMethod);
        writeCompIndexGeneric.GenericArguments.Add(Weaver.Assembly.ImportReference(typeof(int)));
        il.Emit(OpCodes.Ldloc, writerVar);
        il.Emit(OpCodes.Ldarg_0);                        // Load "this"
        il.Emit(OpCodes.Call, getComponentIndexMethod);  // Call GetIndex() on the component
        il.Emit(OpCodes.Call, writeCompIndexGeneric); // Push Caller Indx

        // Write each arg with writer.Write<T>()
        for (int i = 0; i < packMethod.Parameters.Count; i++)
        {
            var param = packMethod.Parameters[i];

            var paramWriteGeneric = new GenericInstanceMethod(writeMethod);
            paramWriteGeneric.GenericArguments.Add(Weaver.Assembly.ImportReference(packMethod.Parameters[i].ParameterType));
            il.Emit(OpCodes.Ldloc, writerVar);
            il.Emit(OpCodes.Ldarg, i + 1);
            il.Emit(OpCodes.Callvirt, paramWriteGeneric);
        }

        // Load channel
        int channelVal = (int)channelAttrib;

        il.Emit(OpCodes.Ldc_I4, channelVal); // Push Channels enum
        il.Emit(OpCodes.Ldloc, writerVar); // Push Writer
        il.Emit(OpCodes.Ldsfld, serverConnectionType); // Get server Connection

        il.Emit(OpCodes.Call, enqueueMethod); // Call MessageHandler.Enqueue(Channels channel, NetworkWriter writer, NetworkConnectionconnection)

        il.Emit(OpCodes.Ret);

        component.Methods.Add(packMethod);

        return packMethod;
    }

    /// <summary>
    /// Generates a (Unpack_MethodName()) that unpacks the incoming data with a custom invoker that reads each argument
    /// </summary>
    internal static MethodDefinition GenerateUpackAndInvokeHandler(TypeDefinition component, MethodDefinition rpc)
    {
        var arraySegByteType = Weaver.Assembly.ImportReference(typeof(byte[]));

        var ncType = Weaver.GetRef("ArcaneNetworking.NetworkedComponent");
        var readerType = Weaver.GetRef("ArcaneNetworking.NetworkReader");
        var poolType = Weaver.GetRef("ArcaneNetworking.NetworkPool"); ;

        // NetworkReader.Read<T>(out T read, Type concreteType = default)
        var readGenericDef = Weaver.Assembly.ImportReference(
            readerType.Resolve().Methods.First(m =>
            m.Name == "Read" &&
            m.HasGenericParameters &&
            m.Parameters.Count == 2 &&
            m.Parameters[0].ParameterType is ByReferenceType br && br.ElementType is GenericParameter &&
            m.Parameters[1].ParameterType.FullName == "System.Type"));

        // Method signature
        var unpackMethod = new MethodDefinition(
            "Unpack_" + rpc.Name,
            MethodAttributes.Public | MethodAttributes.Static,
            Weaver.Assembly.TypeSystem.Void);

        unpackMethod.Parameters.Add(new ParameterDefinition("reader", ParameterAttributes.None, readerType));
        unpackMethod.Parameters.Add(new ParameterDefinition("target", ParameterAttributes.None, ncType));

        // Locals
        unpackMethod.Body.InitLocals = true;

        var il = unpackMethod.Body.GetILProcessor();

        var readerVar = new VariableDefinition(readerType);
        unpackMethod.Body.Variables.Add(readerVar);

        ////////////// WE ARE WEAVING THE UNPACK METHOD NOW ////////////////

        /// Packet Type and methodhash was read before IL Weave

        // load reader from method
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Stloc, readerVar);

        // One local per RPC parameter
        var paramLocals = new List<VariableDefinition>(rpc.Parameters.Count);
        foreach (var p in rpc.Parameters)
        {
            // ensure type is imported into this module
            var pt = Weaver.Assembly.ImportReference(p.ParameterType);
            var v = new VariableDefinition(pt);
            unpackMethod.Body.Variables.Add(v);
            paramLocals.Add(v);
        }

        // For each param: local_i = reader.Read<Ti>() 
        for (int i = 0; i < paramLocals.Count; i++)
        {
            var ti = Weaver.Assembly.ImportReference(paramLocals[i].VariableType);
            var readTi = new GenericInstanceMethod(readGenericDef);
            readTi.GenericArguments.Add(ti);

            il.Emit(OpCodes.Ldloc, readerVar);   // this
            il.Emit(OpCodes.Ldloca, paramLocals[i]); // out local
            il.Emit(OpCodes.Ldnull);             // Type concreteType = null
            il.Emit(OpCodes.Callvirt, readTi);   // Call Read<T>(out T, Type)
            il.Emit(OpCodes.Pop); // Discard the bool
        }

        // Callvirt on the concrete component:
        // ((<DeclaringType>)target).Rpc(paramLocals...)
        var declaringCompType = Weaver.Assembly.ImportReference(rpc.DeclaringType);
        var originalRPCRef = Weaver.Assembly.ImportReference(rpc); // instance method

        il.Emit(OpCodes.Ldarg_1); // target (NetworkedComponent)
        il.Emit(OpCodes.Castclass, declaringCompType); // cast to the concrete type
        
        for (int i = 0; i < paramLocals.Count; i++) // Load to original rpc
            il.Emit(OpCodes.Ldloc, paramLocals[i]);

        il.Emit(OpCodes.Callvirt, originalRPCRef);

        il.Emit(OpCodes.Ret);

        // Add method to the declaring type of the RPC (or a dispatcher type if you prefer)
        component.Methods.Add(unpackMethod);

        return unpackMethod;
    }

    // <summary>
    /// Injects instructions behind an RPC method that are determined by the attribute applied above it
    /// </summary>
    internal static void InjectPackMethod(MethodDefinition rpc, MethodDefinition packMethod)
    {
        var networkManagerType = Weaver.GetRef("ArcaneNetworking.NetworkManager");
        var arrayLengthGetter = Weaver.Assembly.ImportReference(typeof(Array).GetProperty("Length").GetMethod);

        var amIClientField = Weaver.Assembly.ImportReference(
            networkManagerType.Resolve().Fields.First(f => f.Name == "AmIClient" && f.IsStatic)
        );

        bool requireAuthority = (bool)rpc.CustomAttributes
        .First(x => x.AttributeType.FullName == "ArcaneNetworking.CommandAttribute")
        .ConstructorArguments[1].Value;

        var il = rpc.Body.GetILProcessor();
        var first = rpc.Body.Instructions.First();
        var checkOwner = il.Create(OpCodes.Nop);

        il.InsertBefore(first, il.Create(OpCodes.Ldarg_0)); // This (Networked Component)

        for (int i = 1; i < rpc.Parameters.Count + 1; i++) // Load args from this method, original rpc method had one less paramter, so we can safely do Count + 1
            il.InsertBefore(first, il.Create(OpCodes.Ldarg, i));

        il.InsertBefore(first, il.Create(OpCodes.Callvirt, packMethod)); // Call the pack method
        
        // Never run locally
        var ret = il.Create(OpCodes.Ret);
        il.InsertBefore(first, il.Create(OpCodes.Ldsfld, amIClientField)); // Check if we are client
        il.InsertBefore(first, il.Create(OpCodes.Brtrue, ret)); // We are the client, return
        il.Append(ret);

        
        
    }

}
