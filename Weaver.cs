using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Threading.Tasks.Dataflow;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

namespace ArcaneWeaver;

public static class Weaver
{
    internal static ModuleDefinition? Assembly;

    public static int Main(string[] args)
    {
        if (args.Length == 2 && args[1].EndsWith(".dll"))
        {
            string targetPath = args[1];
            string tempPath = Path.Combine(Path.GetTempPath(), Path.GetFileName(targetPath));

            try
            {
                // Open temp DLL for read/write
                using (var fs = new FileStream(targetPath, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite))
                {
                    var readerParams = new ReaderParameters { ReadSymbols = true, SymbolReaderProvider = new PortablePdbReaderProvider() };
                    Assembly = ModuleDefinition.ReadModule(fs, readerParams);

                    foreach (var component in Assembly.Types.ToList())
                    {
                        // Packet hashing
                        if (component.Interfaces.Any(i => i.InterfaceType.FullName == "ArcaneNetworking.Packet"))
                            InjectPacketHashRegister(component);

                        // RPC method weaving
                        if (IsNetComponent(component))
                        {
                            foreach (var originalRPC in component.Methods.ToList())
                            {
                                // // // // // COMMAND RPCS \\ \\ \\ \\ \\
                                if (originalRPC.CustomAttributes.Any(a => a.AttributeType.Name == "CommandAttribute"))
                                {
                                    // Compute a stable hash for this method
                                    int hash = StableHash(originalRPC.FullName);

                                    var commandInternal = Command.GenerateInternalCall(component, originalRPC); // The internal command that will get called on invokes
                                    var commandSendMethod = Command.GenerateSendMethod(component, originalRPC, hash); // The send method that will send the params
                                    Command.InjectSendMethod(originalRPC, commandInternal, commandSendMethod); // Injects the send method into the original rpc
                                    var commandUnpackInvoker = Command.GenerateReceiverInvokeHandler(component, originalRPC, commandInternal); // The unpacker that will read the params

                                    InjectMethodRPCHashRegister(commandUnpackInvoker, hash); // Injects add method to ArcaneNetworking static class to add this method invoker

                                    CecilDebug._createdMethods.Add(commandUnpackInvoker); // Debug
                                    CecilDebug._createdMethods.Add(commandSendMethod); // Debug

                                    continue;
                                }

                                // // // // // RELAY RPCS \\ \\ \\ \\ \\
                                if (originalRPC.CustomAttributes.Any(a => a.AttributeType.Name == "RelayAttribute"))
                                {
                                    // Compute a stable hash for this method
                                    int hash = StableHash(originalRPC.FullName);

                                    var relayInternal = Relay.GenerateInternalCall(component, originalRPC); // The internal command that will get called on invokes
                                    var relayPackInvoker = Relay.GenerateSendMethod(component, originalRPC, hash); // The send method that will send the params
                                    Relay.InjectSendMethod(originalRPC, relayInternal, relayPackInvoker); // Injects the send method into the original rpc
                                    var relayUnpackInvoker = Relay.GenerateReceiverInvokerHandler(component, originalRPC, relayInternal); // The unpacker that will read the params

                                    InjectMethodRPCHashRegister(relayUnpackInvoker, hash); // Injects add method to ArcaneNetworking static class to add this method invoker

                                    CecilDebug._createdMethods.Add(relayUnpackInvoker); // Debug
                                    CecilDebug._createdMethods.Add(relayPackInvoker); // Debug
                                    
                                    continue;
                                }
                            }
                        }
                    }

                    Assembly.Write(fs, new WriterParameters()
                    {
                        WriteSymbols = true,

                        SymbolWriterProvider = new PortablePdbWriterProvider()
                    } );
                    
                    Assembly.Dispose();
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

    internal static int StableHash(string hashString)
    {
        var hash = MD5.HashData(System.Text.Encoding.UTF8.GetBytes(hashString));
        return BitConverter.ToInt32(hash, 0);
    }

    internal static TypeReference GetRef(string FullName)
    {
        // module = your target ModuleDefinition
        // Find the type inside the loaded assembly
        var registryTypeDef = Assembly.Types.FirstOrDefault(t => t.FullName == FullName);

        if (registryTypeDef == null)
            throw new Exception("Could not find Type: " + FullName + " in Project Acssembly!");

        // Import a reference to it
        return Assembly.ImportReference(registryTypeDef);
    }

    /// <summary>
    /// Injects a register method in ArcaneNetworking static constructor that registers packet IDs
    /// </summary>
    static int InjectPacketHashRegister(TypeDefinition packetType)
    {
        // Compute a stable hash for this method
        int hash = StableHash(packetType.FullName);

        // Get ArcaneNetworking type and its static constructor
        var arcaneType = Assembly.Types.First(t => t.Name == "ArcaneNetworking");
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
        var registerRef = Assembly.ImportReference(registerMethod);
        
        var getTypeFromHandle = Assembly.ImportReference(
        typeof(Type).GetMethod("GetTypeFromHandle", [typeof(RuntimeTypeHandle)])
        );

        // Inject IL: RegisterPacket(int hash, Type type)
        il.InsertBefore(ret, il.Create(OpCodes.Ldc_I4, hash));             // push hash
        il.InsertBefore(ret, il.Create(OpCodes.Ldtoken, Assembly.ImportReference(packetType)));     // push method pointer
        il.InsertBefore(ret, il.Create(OpCodes.Call, getTypeFromHandle));
        il.InsertBefore(ret, il.Create(OpCodes.Call, registerRef));       // call RegisterPacket(int, Type)

        return hash;
    }

    // <summary>
    /// Injects a register method in ArcaneNetworking static constructor that registers RPC method
    /// </summary>
    static void InjectMethodRPCHashRegister(MethodDefinition unpackInvoke, int hash)
    {
        // Get ArcaneNetworking type and its static constructor
        var arcaneType = Assembly.Types.First(t => t.Name == "ArcaneNetworking");
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
        var registerRef = Assembly.ImportReference(registerMethod);

        // Import delegate constructor: RPCUnpackDelegate(object target, IntPtr methodPtr)
        var delegateType = Assembly.Types.First(t => t.Name == "RPCUnpackDelegate");
        var delegateCtor = delegateType.Methods.First(m => m.IsConstructor && m.Parameters.Count == 2);
        var delegateCtorRef = Assembly.ImportReference(delegateCtor);

        // Inject IL: RegisterRPC(hash, new RPCUnpackDelegate(null, &Method))
        il.InsertBefore(ret, il.Create(OpCodes.Ldc_I4, hash));             // push hash
        il.InsertBefore(ret, il.Create(OpCodes.Ldnull));                  // target = null (static method)
        il.InsertBefore(ret, il.Create(OpCodes.Ldftn, unpackInvoke));     // push method pointer
        il.InsertBefore(ret, il.Create(OpCodes.Newobj, delegateCtorRef)); // new RPCUnpackDelegate
        il.InsertBefore(ret, il.Create(OpCodes.Call, registerRef));       // call RegisterRPC(int, delegate)
    }

    static bool IsNetComponent(TypeDefinition type)
    {
        while (type != null)
        {
            if (type.FullName == "ArcaneNetworking.NetworkedComponent")
                return true;

            if (type.BaseType == null)
                return false;

            try
            {
                type = type.BaseType.Resolve();
            }
            catch
            {
                return false; // If we can't resolve the base type
            }
            
        }

        return false;
    }

}