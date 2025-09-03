using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

namespace ArcaneWeaver;

public static class Weaver
{
    static ModuleDefinition? ModuleDefinition;

    public static int Main(string[] args)
    {
        if (args.Length == 2 && args[1].EndsWith(".dll"))
        {
            string targetPath = args[1];
            string tempPath = Path.Combine(Path.GetTempPath(), Path.GetFileName(targetPath));

            try
            {

                // Open temp DLL for read/write
                using (var fs = new FileStream(targetPath, System.IO.FileMode.Open, System.IO.FileAccess.ReadWrite, System.IO.FileShare.ReadWrite))
                {
                    ModuleDefinition = ModuleDefinition.ReadModule(fs);

                    foreach (var type in ModuleDefinition.Types.ToList().ToList())
                    {
                        // Packet hashing
                        if (type.Interfaces.Any(i => i.InterfaceType.FullName == "ArcaneNetworking.Packet"))
                            InjectPacketHashRegister(type);

                        // RPC method weaving
                        if (type.BaseType?.FullName == "ArcaneNetworking.NetworkedComponent")
                        {
                            foreach (var originalRPC in type.Methods.ToList())
                            {
                                if (!originalRPC.CustomAttributes.Any(a => a.AttributeType.Name == "MethodRPCAttribute"))
                                    continue;

                                var invokeHandler = GenerateRPCUpackAndInvokeHandler(type, originalRPC);

                                CecilDebug._createdMethods.Add(invokeHandler); // Debug

                                int methodHash = InjectMethodRPCHashRegister(invokeHandler);
                                var packMethod = GeneratePackMethod(type, originalRPC, methodHash);

                                CecilDebug._createdMethods.Add(packMethod); // Debug

                                InjectPackMethod(originalRPC, packMethod);
                            }
                        }

                    }

                    ModuleDefinition.Write(fs);
                    ModuleDefinition.Dispose();
                }


                ////////////////////// DEBUG /////////////////////
                foreach (var method in CecilDebug._createdMethods)
                    CecilDebug.DumpMethodIL(method);
                ////////////////////// DEBUG /////////////////////

                
                Console.WriteLine("[Weaver] Successfully weaved: " + targetPath);
                return 0;
            }
            catch (Exception e)
            {
                Console.WriteLine("[Weaver] Failed to weave: " + e);
                return 1;
            }
        }

        Console.WriteLine("[Weaver] Invalid arguments.");
        return 1;
    }

    static int StableHash(string hashString)
    {
        var hash = MD5.HashData(System.Text.Encoding.UTF8.GetBytes(hashString));
        return BitConverter.ToInt32(hash, 0);
    }

    static TypeReference GetTypeReferenceByFullName(string FullName)
    {
        // module = your target ModuleDefinition
        // Find the type inside the loaded assembly
        var registryTypeDef = ModuleDefinition.Types.FirstOrDefault(t => t.FullName == FullName);

        if (registryTypeDef == null)
            throw new Exception("Could not find Type: " + FullName + " in Project Acssembly!");

        // Import a reference to it
        return ModuleDefinition.ImportReference(registryTypeDef);
    }

    /// <summary>
    /// Injects a register method in ArcaneNetworking static constructor that registers packet IDs
    /// </summary>
    static int InjectPacketHashRegister(TypeDefinition packetType)
    {
        // Compute a stable hash for this method
        int hash = StableHash(packetType.FullName);

        // Get ArcaneNetworking type and its static constructor
        var arcaneType = ModuleDefinition.Types.First(t => t.Name == "ArcaneNetworking");
        var cctor = arcaneType.GetStaticConstructor();
        var il = cctor.Body.GetILProcessor();
        var ret = cctor.Body.Instructions.Last(i => i.OpCode == OpCodes.Ret);

        // Import reference to RegisterRPC(int, RPCUnpackDelegate)
        var registerMethod = arcaneType.Methods.First(m =>
            m.Name == "RegisterPacket" &&
            m.Parameters.Count == 2 &&
            m.Parameters[0].ParameterType.MetadataType == MetadataType.Int32 &&
            m.Parameters[1].ParameterType.Name == "Type"
        );
        var registerRef = ModuleDefinition.ImportReference(registerMethod);
        
        var getTypeFromHandle = ModuleDefinition.ImportReference(
        typeof(Type).GetMethod("GetTypeFromHandle", [typeof(RuntimeTypeHandle)])
        );

        // Inject IL: RegisterPacket(int hash, Type type)
        il.InsertBefore(ret, il.Create(OpCodes.Ldc_I4, hash));             // push hash
        il.InsertBefore(ret, il.Create(OpCodes.Ldtoken, ModuleDefinition.ImportReference(packetType)));     // push method pointer
        il.InsertBefore(ret, il.Create(OpCodes.Call, getTypeFromHandle));
        il.InsertBefore(ret, il.Create(OpCodes.Call, registerRef));       // call RegisterPacket(int, Type)

        return hash;
    }

    // <summary>
    /// Injects a register method in ArcaneNetworking static constructor that registers RPC method
    /// </summary>
   private static int InjectMethodRPCHashRegister(MethodDefinition unpackInvoke)
    {
        // Compute a stable hash for this method
        int hash = StableHash(unpackInvoke.FullName);

        // Get ArcaneNetworking type and its static constructor
        var arcaneType = ModuleDefinition.Types.First(t => t.Name == "ArcaneNetworking");
        var cctor = arcaneType.GetStaticConstructor();
        var il = cctor.Body.GetILProcessor();
        var ret = cctor.Body.Instructions.Last(i => i.OpCode == OpCodes.Ret);

        // Import reference to RegisterRPC(int, RPCUnpackDelegate)
        var registerMethod = arcaneType.Methods.First(m =>
            m.Name == "RegisterRPC" &&
            m.Parameters.Count == 2 &&
            m.Parameters[0].ParameterType.MetadataType == MetadataType.Int32 &&
            m.Parameters[1].ParameterType.Name == "RPCUnpackDelegate"
        );
        var registerRef = ModuleDefinition.ImportReference(registerMethod);

        // Import delegate constructor: RPCUnpackDelegate(object target, IntPtr methodPtr)
        var delegateType = ModuleDefinition.Types.First(t => t.Name == "RPCUnpackDelegate");
        var delegateCtor = delegateType.Methods.First(m => m.IsConstructor && m.Parameters.Count == 2);
        var delegateCtorRef = ModuleDefinition.ImportReference(delegateCtor);

        // Inject IL: RegisterRPC(hash, new RPCUnpackDelegate(null, &Method))
        il.InsertBefore(ret, il.Create(OpCodes.Ldc_I4, hash));             // push hash
        il.InsertBefore(ret, il.Create(OpCodes.Ldnull));                  // target = null (static method)
        il.InsertBefore(ret, il.Create(OpCodes.Ldftn, unpackInvoke));     // push method pointer
        il.InsertBefore(ret, il.Create(OpCodes.Newobj, delegateCtorRef)); // new RPCUnpackDelegate
        il.InsertBefore(ret, il.Create(OpCodes.Call, registerRef));       // call RegisterRPC(int, delegate)

        return hash;
    }

    // <summary>
    /// Injects instructions behind an RPC method 
    /// </summary>
    static void InjectPackMethod(MethodDefinition rpc, MethodDefinition packMethod)
    {
        bool isServerCommand = (bool)rpc.CustomAttributes
        .First(x => x.AttributeType.FullName == "ArcaneNetworking.MethodRPCAttribute")
        .ConstructorArguments[1].Value;

        var il = rpc.Body.GetILProcessor();
        var first = rpc.Body.Instructions.First();

        // Define (if) branch targets (Check if param 1 is null, uint[])
        var skipBlock = il.Create(OpCodes.Nop);

        // Define condition
        il.InsertBefore(first, il.Create(OpCodes.Ldarg_1)); // connectionsToSendTo[]
        il.InsertBefore(first, il.Create(OpCodes.Ldnull));
        il.InsertBefore(first, il.Create(OpCodes.Ceq)); // Check equal, 1 if equal
        il.InsertBefore(first, il.Create(OpCodes.Brtrue, skipBlock)); // Check if it is null, if true, skip!

        il.InsertBefore(first, il.Create(OpCodes.Ldarg_0)); // This (Networked Component)

        for (int i = 1; i < rpc.Parameters.Count + 1; i++)
            il.InsertBefore(first, il.Create(OpCodes.Ldarg, i));

        il.InsertBefore(first, il.Create(OpCodes.Callvirt, packMethod));

        if (isServerCommand) il.InsertBefore(first, il.Create(OpCodes.Ret));

        // End of (if)
        il.InsertBefore(first, skipBlock);

    }

    // <summary>
    /// Generates a (packMethod) that pack the arguments that are sent into an RPC, as well as the packet type (RPC = 1),
    /// and the RPC data.
    /// </summary>
    static MethodDefinition GeneratePackMethod(TypeDefinition component, MethodDefinition rpc, int methodHash)
    {
        // Get From MethodRPC Attribute args
        var rpcAttr = rpc.CustomAttributes.First(x => x.AttributeType.FullName == "ArcaneNetworking.MethodRPCAttribute");
        var channelAttrib = rpcAttr.ConstructorArguments[0].Value;
        var sendTimeAttrib = rpcAttr.ConstructorArguments[1].Value;

        // Import TypeReferences
        var rpcPacketType = GetTypeReferenceByFullName("ArcaneNetworking.RPCPacket");
        var netComponentType = GetTypeReferenceByFullName("ArcaneNetworking.NetworkedComponent");
        var netNodeType = GetTypeReferenceByFullName("ArcaneNetworking.NetworkedNode");
        var writerType = GetTypeReferenceByFullName("ArcaneNetworking.NetworkWriter");
        var poolType = GetTypeReferenceByFullName("ArcaneNetworking.NetworkPool");
        var messageLayerType = GetTypeReferenceByFullName("ArcaneNetworking.MessageLayer");
        var channelsEnumType = GetTypeReferenceByFullName("ArcaneNetworking.Channels");

        // Get FieldReferences
        var netNodeField = ModuleDefinition.ImportReference(
            netComponentType.Resolve().Fields.First(f => f.Name == "NetworkedNode")
        );
        var netIDField = ModuleDefinition.ImportReference(
            netNodeField.FieldType.Resolve().Fields.First(f => f.Name == "NetID")
        );
        var messageLayerActiveField = ModuleDefinition.ImportReference(
            messageLayerType.Resolve().Fields.First(f => f.Name == "Active")
        );

        // Get MethodReferences
        var getComponentIndexMethod = ModuleDefinition.ImportReference(
            netComponentType.Resolve().Methods.First(m => m.Name == "GetIndex")
        );
        var getWriterBufferMethod = ModuleDefinition.ImportReference(
            writerType.Resolve().Methods.First(m => m.Name == "ToArraySegment")
        );
        var writeMethod = ModuleDefinition.ImportReference(
            writerType.Resolve().Methods.First(m => m.Name == "Write" && m.HasGenericParameters && m.Parameters.Count == 1)
        );


        // Import SendToConnections properly
        var sendMethodRef = ModuleDefinition.ImportReference(
            messageLayerType.Resolve().Methods.First(m =>
                m.Name == "SendToConnections" &&
                m.Parameters.Count == 3 &&
                m.Parameters[0].ParameterType.FullName == ModuleDefinition.ImportReference(typeof(ArraySegment<byte>)).FullName &&
                m.Parameters[1].ParameterType.FullName == channelsEnumType.FullName &&
                m.Parameters[2].ParameterType.FullName == ModuleDefinition.ImportReference(typeof(uint[])).FullName
            )
        );

        // Import static pool methods
        var poolGetWriterMethod = ModuleDefinition.ImportReference(
            poolType.Resolve().Methods.First(m => m.Name == "GetWriter" && m.IsStatic && m.Parameters.Count == 0)
        );
        var recycleWriterMethod = ModuleDefinition.ImportReference(
            poolType.Resolve().Methods.First(m => m.Name == "Recycle" && m.IsStatic && m.Parameters.Count == 1)
        );

        // Create pack method
        var packMethod = new MethodDefinition(
            "Pack_" + rpc.Name,
            Mono.Cecil.MethodAttributes.Public,
            ModuleDefinition.TypeSystem.Void
        );

        // Add same params as original RPC
        foreach (var p in rpc.Parameters)
            packMethod.Parameters.Add(new ParameterDefinition(p.Name, Mono.Cecil.ParameterAttributes.None, ModuleDefinition.ImportReference(p.ParameterType)));

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
        var rpcCallerNetIDField = ModuleDefinition.ImportReference(rpcPacketType.Resolve().Fields.First(f => f.Name == "CallerNetID"));
        var rpcCallerCompIndexField = ModuleDefinition.ImportReference(rpcPacketType.Resolve().Fields.First(f => f.Name == "CallerCompIndex"));

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
        writePacketTypeGeneric.GenericArguments.Add(ModuleDefinition.ImportReference(typeof(byte)));
        il.Emit(OpCodes.Ldloc, writerVar);
        il.Emit(OpCodes.Ldc_I4_S, (sbyte)1);
        il.Emit(OpCodes.Callvirt, writePacketTypeGeneric); // Push Packet byte

        // Write RPC Method ID hash to writer
        var writePacketHashGeneric = new GenericInstanceMethod(writeMethod);
        writePacketHashGeneric.GenericArguments.Add(ModuleDefinition.ImportReference(typeof(int)));
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
            paramWriteGeneric.GenericArguments.Add(ModuleDefinition.ImportReference(packMethod.Parameters[i].ParameterType));
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
    /// Generates a (unpackMethod) that unpacks the incoming data with a custom invoker that reads each argument
    /// </summary>
    static MethodDefinition GenerateRPCUpackAndInvokeHandler(TypeDefinition component, MethodDefinition rpc)
    {
        var arraySegByteType = ModuleDefinition.ImportReference(typeof(byte[]));
        var ncType = GetTypeReferenceByFullName("ArcaneNetworking.NetworkedComponent");
        var readerType = GetTypeReferenceByFullName("ArcaneNetworking.NetworkReader");
        var poolType = GetTypeReferenceByFullName("ArcaneNetworking.NetworkPool"); ;
        var rpcPacketType = GetTypeReferenceByFullName("ArcaneNetworking.RPCPacket");

        // NetworkReader.Read<T>(out T read, Type concreteType = default)
        var readGenericDef = ModuleDefinition.ImportReference(
            readerType.Resolve().Methods.First(m =>
            m.Name == "Read" &&
            m.HasGenericParameters &&
            m.Parameters.Count == 2 &&
            m.Parameters[0].ParameterType is ByReferenceType br && br.ElementType is GenericParameter &&
            m.Parameters[1].ParameterType.FullName == "System.Type"));

        // Method signature
        var unpackMethod = new MethodDefinition(
            "Unpack_" + rpc.Name,
            Mono.Cecil.MethodAttributes.Public | Mono.Cecil.MethodAttributes.Static,
            ModuleDefinition.TypeSystem.Void);

        unpackMethod.Parameters.Add(new ParameterDefinition("reader", Mono.Cecil.ParameterAttributes.None, readerType));
        unpackMethod.Parameters.Add(new ParameterDefinition("target", Mono.Cecil.ParameterAttributes.None, ncType));

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
            var pt = ModuleDefinition.ImportReference(p.ParameterType);
            var v = new VariableDefinition(pt);
            unpackMethod.Body.Variables.Add(v);
            paramLocals.Add(v);
        }

        // For each param: local_i = reader.Read<Ti>() (Skip one to avoid the connectionIdsToSend uint[])
        for (int i = 1; i < paramLocals.Count; i++)
        {
            var ti = ModuleDefinition.ImportReference(paramLocals[i].VariableType);
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
        var declaringCompType = ModuleDefinition.ImportReference(rpc.DeclaringType);
        var originalRPCRef = ModuleDefinition.ImportReference(rpc); // instance method

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


}