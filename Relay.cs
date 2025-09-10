using System;
using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

// public RelayAttribute(Channels channel = Channels.Reliable)


//example: RelayExplosion(Vector3 position, params uint[] sendTo)

namespace ArcaneWeaver;

public class Relay
{
     /// <summary>
    /// Moves the rpc method body to its own method so we can call it from the original
    /// </summary>
    /// <returns></returns>
    internal static MethodDefinition GenerateInternalCall(TypeDefinition component, MethodDefinition rpc)
    {
        // Create internal method
        var internalCall = new MethodDefinition(
            "Internal_Command_" + rpc.Name,
            MethodAttributes.Public,
            Weaver.Assembly.TypeSystem.Void
        );

        // Add same params as original RPC
        foreach (var p in rpc.Parameters)
            internalCall.Parameters.Add(new ParameterDefinition(p.Name, ParameterAttributes.None, Weaver.Assembly.ImportReference(p.ParameterType)));

        var ilSource = rpc.Body.Instructions;
        var ilTarget = internalCall.Body.Instructions;

        internalCall.Body = rpc.Body;
        rpc.Body = new MethodBody(rpc);
        rpc.Body.GetILProcessor().Emit(OpCodes.Ret);

        // Add to component
        component.Resolve().Methods.Add(internalCall);
        return internalCall;

    }
    // <summary>
    /// Generates a (Pack_MethodName()) that packs the arguments that are sent into an Relay RPC, as well as the packet type (RPC = 1),
    /// and the RPC data.
    /// </summary>
    internal static MethodDefinition GenerateSendMethod(TypeDefinition component, MethodDefinition rpc, int methodHash)
    {

        // Get From MethodRPC Attribute args
        var rpcAttr = rpc.CustomAttributes.First(x => x.AttributeType.FullName == "ArcaneNetworking.RelayAttribute");
        var channelAttrib = rpcAttr.ConstructorArguments[0].Value;

        var arrayType = Weaver.Assembly.ImportReference(typeof(Array));

        // Import TypeReferences
        var serverType = Weaver.GetRef("ArcaneNetworking.Server");
        var netComponentType = Weaver.GetRef("ArcaneNetworking.NetworkedComponent");
        var netNodeType = Weaver.GetRef("ArcaneNetworking.NetworkedNode");
        var writerType = Weaver.GetRef("ArcaneNetworking.NetworkWriter");
        var poolType = Weaver.GetRef("ArcaneNetworking.NetworkPool");
        var messageLayerType = Weaver.GetRef("ArcaneNetworking.MessageLayer");
        var channelsEnumType = Weaver.GetRef("ArcaneNetworking.Channels");
        var connectionType = Weaver.GetRef("ArcaneNetworking.NetworkConnection");

        // Do we have targets ? or just one (^1 is last element)
        bool allTargets = false;
        bool multiTarget = false;
        bool singleTarget = false;
        if (rpc.Parameters.Count > 0)
        {
            multiTarget = Weaver.Assembly.ImportReference(rpc.Parameters[^1].ParameterType).FullName == Weaver.Assembly.ImportReference(connectionType.MakeArrayType()).FullName;
            singleTarget = rpc.Parameters[^1].ParameterType == connectionType;
            allTargets = !multiTarget && !singleTarget;
        }
        else allTargets = true;


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

        var writeMethod = Weaver.Assembly.ImportReference(
            writerType.Resolve().Methods.First(m => m.Name == "Write" && m.HasGenericParameters && m.Parameters.Count == 1)
        );
        var poolGetWriterMethod = Weaver.Assembly.ImportReference(
            poolType.Resolve().Methods.First(m => m.Name == "GetWriter" && m.IsStatic && m.Parameters.Count == 0)
        );
        var getAllConnectionsMethod = Weaver.Assembly.ImportReference(
            serverType.Resolve().Methods.First(m => m.Name == "GetAllConnections" && m.IsStatic && m.Parameters.Count == 0)
        );
        var sendConnectionRawMethod = Weaver.Assembly.ImportReference(
            connectionType.Resolve().Methods.First(m => m.Name == "SendRaw" && m.Parameters.Count == 2)
        );

        var arrayLengthProp = Weaver.Assembly.ImportReference(
            arrayType.Resolve().Properties.First(m => m.Name == "Length").GetMethod
        );
        var arrayGetValueMethod = Weaver.Assembly.ImportReference(
            arrayType.Resolve().Methods.First(m => m.Name == "GetValue" && m.Parameters.Count == 1)
        );

        // Create pack method
        var packMethod = new MethodDefinition(
            "Pack_Relay_" + rpc.Name,
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

        // For (int i = 0; i < Server.GetAllConnections.Length(); i++)

        var indexVar = new VariableDefinition(Weaver.Assembly.TypeSystem.Int32);
        packMethod.Body.Variables.Add(indexVar);

        Instruction loopCheck = il.Create(OpCodes.Nop);
        Instruction loopBody = il.Create(OpCodes.Nop);
        Instruction loopEnd  = il.Create(OpCodes.Nop);

        // int i = 0
        il.Emit(OpCodes.Ldc_I4_0);
        il.Emit(OpCodes.Stloc, indexVar); // your i variable

        // jump to condition check
        il.Emit(OpCodes.Br, loopCheck);

        // --- loop body ---
        il.Append(loopBody);

        
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

        // Write each arg with writer.Write<T>() (skip the last one, that is for the connections we are sending to)
        for (int i = 0; i < packMethod.Parameters.Count; i++)
        {
            var param = packMethod.Parameters[i];

            var paramWriteGeneric = new GenericInstanceMethod(writeMethod);
            paramWriteGeneric.GenericArguments.Add(Weaver.Assembly.ImportReference(packMethod.Parameters[i].ParameterType));
            il.Emit(OpCodes.Ldloc, writerVar);
            il.Emit(OpCodes.Ldarg, i + 1);
            il.Emit(OpCodes.Callvirt, paramWriteGeneric);
        }


        // Writer orderflow:
        // rpcTypeByte -> methodHash -> callerNetID -> callerComponentIndex -> args (arbitrary)

        // Load array
        if (allTargets)
            il.Emit(OpCodes.Call, getAllConnectionsMethod);
        else
            il.Emit(OpCodes.Ldarg, rpc.Parameters.Count - 1);
    

        // Load i
        il.Emit(OpCodes.Ldloc, indexVar);
        il.Emit(OpCodes.Ldelem_Ref); // instead of arrayGetValueMethod

        // load writer + channel
        il.Emit(OpCodes.Ldloc, writerVar);
        il.Emit(OpCodes.Ldc_I4, (int)channelAttrib);
        il.Emit(OpCodes.Callvirt, sendConnectionRawMethod);

        // i++
        il.Emit(OpCodes.Ldloc, indexVar);
        il.Emit(OpCodes.Ldc_I4_1);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Stloc, indexVar);

        // --- loop check ---
        il.Append(loopCheck);

        // Load i
        il.Emit(OpCodes.Ldloc, indexVar);

        // Load array.Length
        if (allTargets)
            il.Emit(OpCodes.Call, getAllConnectionsMethod);
        else
            il.Emit(OpCodes.Ldarg, rpc.Parameters.Count - 1);

        il.Emit(OpCodes.Ldlen);         // get array length
        il.Emit(OpCodes.Conv_I4);       // convert native int -> int32

        // if (i < array.Length) goto loopBody
        il.Emit(OpCodes.Blt, loopBody);

        // --- loop end ---
        il.Append(loopEnd);

        il.Emit(OpCodes.Ret);

        component.Methods.Add(packMethod);

        return packMethod;
    }

    /// <summary>
    /// Generates a (Unpack_MethodName()) that unpacks the incoming data with a custom invoker that reads each argument
    /// </summary>
    internal static MethodDefinition GenerateReceiverInvokerHandler(TypeDefinition component, MethodDefinition rpc, MethodDefinition internalrpc)
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
            "Unpack_Relay_" + rpc.Name,
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
        var declaringCompType = Weaver.Assembly.ImportReference(internalrpc.DeclaringType);

        il.Emit(OpCodes.Ldarg_1); // target (NetworkedComponent)
        il.Emit(OpCodes.Castclass, declaringCompType); // cast to the concrete type
        
        for (int i = 0; i < paramLocals.Count; i++) // Load to original rpc
            il.Emit(OpCodes.Ldloc, paramLocals[i]);

        il.Emit(OpCodes.Callvirt, internalrpc);

        il.Emit(OpCodes.Ret);

        // Add method to the declaring type of the RPC (or a dispatcher type if you prefer)
        component.Methods.Add(unpackMethod);

        return unpackMethod;
    }
    // <summary>
    /// Injects instructions behind an RPC method that are determined by the attribute applied above it
    /// </summary>
    internal static void InjectSendMethod(MethodDefinition rpc, MethodDefinition internalRPC, MethodDefinition packMethod)
    {
        var networkManagerType = Weaver.GetRef("ArcaneNetworking.NetworkManager");


        var amIServerField = Weaver.Assembly.ImportReference(
            networkManagerType.Resolve().Fields.First(f => f.Name == "AmIServer" && f.IsStatic)
        );
        var amIClientField = Weaver.Assembly.ImportReference(
           networkManagerType.Resolve().Fields.First(f => f.Name == "AmIClient" && f.IsStatic)
        );

        var il = rpc.Body.GetILProcessor();
        var first = rpc.Body.Instructions.First();

        var checkClientEnd = il.Create(OpCodes.Nop);
        var checkServerEnd = il.Create(OpCodes.Nop);
        var ret = il.Create(OpCodes.Ret);

        // if (AmIClient) { internalRPC(...); return; }
        il.InsertBefore(first, il.Create(OpCodes.Ldsfld, amIClientField));
        il.InsertBefore(first, il.Create(OpCodes.Brfalse, checkClientEnd));
        il.InsertBefore(first, il.Create(OpCodes.Ldarg_0));

        for (int i = 0; i < rpc.Parameters.Count; i++)
            il.InsertBefore(first, il.Create(OpCodes.Ldarg, i + 1));

        il.InsertBefore(first, il.Create(OpCodes.Callvirt, internalRPC));
        il.InsertBefore(first, checkClientEnd);

        // else if (AmIServer) { packMethod(...); }
        il.InsertBefore(first, il.Create(OpCodes.Ldsfld, amIServerField));
        il.InsertBefore(first, il.Create(OpCodes.Brfalse, checkServerEnd));
        il.InsertBefore(first, il.Create(OpCodes.Ldarg_0));

        for (int i = 0; i < rpc.Parameters.Count; i++)
            il.InsertBefore(first, il.Create(OpCodes.Ldarg, i + 1));

        il.InsertBefore(first, il.Create(OpCodes.Callvirt, packMethod));
        il.InsertBefore(first, checkServerEnd);

        // final return
        il.InsertBefore(first, ret);

        
    }

}
