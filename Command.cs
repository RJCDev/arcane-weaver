using System;
using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

// public CommandAttribute(Channels channel = Channels.Reliable, bool runLocal = false, bool requireAuthority = false)

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
        var rpcAttr = rpc.CustomAttributes.First(x => x.AttributeType.FullName == "ArcaneNetworking.MethodRPCAttribute");
        var channelAttrib = rpcAttr.ConstructorArguments[0].Value;
        var sendTimeAttrib = rpcAttr.ConstructorArguments[1].Value;

        // Import TypeReferences
        var rpcPacketType = Weaver.GetRef("ArcaneNetworking.RPCPacket");
        var netComponentType = Weaver.GetRef("ArcaneNetworking.NetworkedComponent");
        var netNodeType = Weaver.GetRef("ArcaneNetworking.NetworkedNode");
        var writerType = Weaver.GetRef("ArcaneNetworking.NetworkWriter");
        var poolType = Weaver.GetRef("ArcaneNetworking.NetworkPool");
        var messageLayerType = Weaver.GetRef("ArcaneNetworking.MessageLayer");
        var channelsEnumType = Weaver.GetRef("ArcaneNetworking.Channels");

        // Get FieldReferences
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
        var getWriterBufferMethod = Weaver.Assembly.ImportReference(
            writerType.Resolve().Methods.First(m => m.Name == "ToArraySegment")
        );
        var writeMethod = Weaver.Assembly.ImportReference(
            writerType.Resolve().Methods.First(m => m.Name == "Write" && m.HasGenericParameters && m.Parameters.Count == 1)
        );


        // Import SendToConnections properly
        var sendMethodRef = Weaver.Assembly.ImportReference(
            messageLayerType.Resolve().Methods.First(m =>
                m.Name == "SendToConnections" &&
                m.Parameters.Count == 3 &&
                m.Parameters[0].ParameterType.FullName == Weaver.Assembly.ImportReference(typeof(ArraySegment<byte>)).FullName &&
                m.Parameters[1].ParameterType.FullName == channelsEnumType.FullName &&
                m.Parameters[2].ParameterType.FullName == Weaver.Assembly.ImportReference(typeof(uint[])).FullName
            )
        );

        // Import static pool methods
        var poolGetWriterMethod = Weaver.Assembly.ImportReference(
            poolType.Resolve().Methods.First(m => m.Name == "GetWriter" && m.IsStatic && m.Parameters.Count == 0)
        );
        var recycleWriterMethod = Weaver.Assembly.ImportReference(
            poolType.Resolve().Methods.First(m => m.Name == "Recycle" && m.IsStatic && m.Parameters.Count == 1)
        );

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

        ///// WE ARE CREATING AN RPC PACKET OBJECT ///////
        ///
        // Store it into a local (so we can use it)
        // RPCPacket packet = 
        var rpcVar = new VariableDefinition(rpcPacketType);
        packMethod.Body.Variables.Add(rpcVar);

        il.Emit(OpCodes.Ldloca, rpcVar);
        il.Emit(OpCodes.Initobj, rpcPacketType);

        // Get fields and set values for RPCPacket definition
        var rpcCallerNetIDField = Weaver.Assembly.ImportReference(rpcPacketType.Resolve().Fields.First(f => f.Name == "CallerNetID"));
        var rpcCallerCompIndexField = Weaver.Assembly.ImportReference(rpcPacketType.Resolve().Fields.First(f => f.Name == "CallerCompIndex"));

        // NetID
        il.Emit(OpCodes.Ldloca, rpcVar); // Load the address of the local struct (RPCPacket) onto the stack

        il.Emit(OpCodes.Ldarg_0);  // Load "this" (NetworkedComponent)
        il.Emit(OpCodes.Ldfld, netNodeField);  // Load NetworkedNode from the abstract NetworkedComponent
        il.Emit(OpCodes.Ldfld, netIDField);  // Load NetID from NetworkedNode
        il.Emit(OpCodes.Stfld, rpcCallerNetIDField);  // Store the NetID into the RPCPacket struct field

        // CallerCompIndex 
        il.Emit(OpCodes.Ldloca, rpcVar); // Load the address of the local struct (RPCPacket) onto the stack

        il.Emit(OpCodes.Ldarg_0);                        // Load "this"
        il.Emit(OpCodes.Call, getComponentIndexMethod);  // Call GetIndex() on the component
        il.Emit(OpCodes.Stfld, rpcCallerCompIndexField); // Store result into CallerCompIndex

        /////// WE ARE NOW WRITING TO THE NETWORK WRITER BUFFER /////////

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

        // Write packet struct to writer
        var writePacketGeneric = new GenericInstanceMethod(writeMethod);
        writePacketGeneric.GenericArguments.Add(rpcPacketType.Resolve());
        il.Emit(OpCodes.Ldloc, writerVar);
        il.Emit(OpCodes.Ldloc, rpcVar); // Load packet (packet is struct)
        il.Emit(OpCodes.Callvirt, writePacketGeneric); // Call generic

        // Write each arg with writer.Write (skip the first one, that is for the connections we are sending to)
        for (int i = 1; i < packMethod.Parameters.Count; i++)
        {
            var param = packMethod.Parameters[i];

            var paramWriteGeneric = new GenericInstanceMethod(writeMethod);
            paramWriteGeneric.GenericArguments.Add(Weaver.Assembly.ImportReference(packMethod.Parameters[i].ParameterType));
            il.Emit(OpCodes.Ldloc, writerVar);
            il.Emit(OpCodes.Ldarg, i + 1);
            il.Emit(OpCodes.Callvirt, paramWriteGeneric);
        }

        // Send over the network
        // Load MessageLayer.Active
        il.Emit(OpCodes.Ldsfld, messageLayerActiveField); // push Active onto stack

        // Load writer array
        il.Emit(OpCodes.Ldloc, writerVar);     // Push writer
        il.Emit(OpCodes.Call, getWriterBufferMethod); // Call writer.ToArraySegment()

        // Load channel
        int channelVal = (int)channelAttrib;
        il.Emit(OpCodes.Ldc_I4, channelVal);  // Push Channels enum

        // Load the connectionsToSend array when calling the method
        il.Emit(OpCodes.Ldarg, 1);

        // Call the instance method
        il.Emit(OpCodes.Callvirt, sendMethodRef);  // Call Active.SendToConnections(...)

        // Recycle NetworkWriter
        il.Emit(OpCodes.Ldloc, writerVar);
        il.Emit(OpCodes.Call, recycleWriterMethod);

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
        var rpcPacketType = Weaver.GetRef("ArcaneNetworking.RPCPacket");

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
        var packetVar = new VariableDefinition(rpcPacketType.Resolve()); // struct local
        unpackMethod.Body.Variables.Add(readerVar);
        unpackMethod.Body.Variables.Add(packetVar);

        ////////////// WE ARE WEAVING THE UNPACK METHOD NOW ////////////////

        /// Packet Type and methodhash was read before IL Weave

        // load reader from method
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Stloc, readerVar);

        // One local per RPC parameter
        var paramLocals = new List<VariableDefinition>(rpc.Parameters.Count);
        foreach (var p in rpc.Parameters) // 0 is uint[] connectionsToSendTo
        {
            // ensure type is imported into this module
            var pt = Weaver.Assembly.ImportReference(p.ParameterType);
            var v = new VariableDefinition(pt);
            unpackMethod.Body.Variables.Add(v);
            paramLocals.Add(v);
        }

        // For each param: local_i = reader.Read<Ti>() (Skip one to avoid the connectionIdsToSend uint[])
        for (int i = 1; i < paramLocals.Count; i++)
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

        il.Emit(OpCodes.Ldnull);
        for (int i = 1; i < paramLocals.Count; i++) // Load to original rpc
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
        var arrayLengthGetter = Weaver.Assembly.ImportReference(typeof(Array).GetProperty("Length").GetMethod);

        bool runLocal = (bool)rpc.CustomAttributes
        .First(x => x.AttributeType.FullName == "ArcaneNetworking.MethodRPCAttribute")
        .ConstructorArguments[1].Value;

        var il = rpc.Body.GetILProcessor();
        var first = rpc.Body.Instructions.First();

        // Define (if) branch targets (Check if param 1 is null, uint[])
        var nullSkip = il.Create(OpCodes.Nop);
        var checkLength = il.Create(OpCodes.Nop);

        // Define condition
        il.InsertBefore(first, il.Create(OpCodes.Ldarg_1)); // connectionsToSendTo[]
        il.InsertBefore(first, il.Create(OpCodes.Ldnull));
        il.InsertBefore(first, il.Create(OpCodes.Ceq)); // Check equal

        il.InsertBefore(first, il.Create(OpCodes.Brtrue, nullSkip)); // Check if it is null, if true, skip!

        // Now we check if equal to 1
        il.InsertBefore(first, il.Create(OpCodes.Ldarg_1)); // connectionsToSendTo[]
        il.InsertBefore(first, il.Create(OpCodes.Callvirt, arrayLengthGetter)); // Load count
        il.InsertBefore(first, il.Create(OpCodes.Ldc_I4_0));
        il.InsertBefore(first, il.Create(OpCodes.Ceq)); // Check equal

        il.InsertBefore(first, il.Create(OpCodes.Brtrue, checkLength)); // Check if connsToSendTo.Count == 0, if true, skip!


        il.InsertBefore(first, il.Create(OpCodes.Ldarg_0)); // This (Networked Component)

        for (int i = 1; i < rpc.Parameters.Count + 1; i++) // Load args from this method, original rpc method had one less paramter, so we can safely do Count + 1
            il.InsertBefore(first, il.Create(OpCodes.Ldarg, i));

        il.InsertBefore(first, il.Create(OpCodes.Callvirt, packMethod)); // Call the pack method

        // If we shouldn't run locally, return after the pack
        if (!runLocal)
        {
            il.InsertBefore(first, il.Create(OpCodes.Ret));
        }
        // Before End Of if
        il.InsertBefore(first, checkLength); 

        // End of null check
        il.InsertBefore(first, nullSkip);

 
  

    }

}
