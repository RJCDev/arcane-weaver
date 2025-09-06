using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
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
                    Assembly = ModuleDefinition.ReadModule(fs);

                    foreach (var type in Assembly.Types.ToList().ToList())
                    {
                        // Packet hashing
                        if (type.Interfaces.Any(i => i.InterfaceType.FullName == "ArcaneNetworking.Packet"))
                            InjectPacketHashRegister(type);

                        // RPC method weaving
                        if (type.BaseType?.FullName == "ArcaneNetworking.NetworkedComponent")
                        {
                            foreach (var originalRPC in type.Methods.ToList())
                            {
                                // // // // // COMMAND RPCS \\ \\ \\ \\ \\
                                if (originalRPC.CustomAttributes.Any(a => a.AttributeType.Name == "CommandAttribute"))
                                {
                                    var commandUnpackInvoker = Command.GenerateUpackAndInvokeHandler(type, originalRPC);
                                    var commandPackInvoker = Command.GeneratePackAndSendMethod(type, originalRPC, InjectMethodRPCHashRegister(commandUnpackInvoker));

                                    Command.InjectPackMethod(originalRPC, commandPackInvoker);

                                    CecilDebug._createdMethods.Add(commandUnpackInvoker); // Debug
                                    CecilDebug._createdMethods.Add(commandPackInvoker); // Debug

                                    continue;
                                }

                                // // // // // RELAY RPCS \\ \\ \\ \\ \\
                                if (originalRPC.CustomAttributes.Any(a => a.AttributeType.Name == "RelayAttribute"))
                                {
                                    var relayUnpackInvoker = Relay.GenerateUpackAndInvokeHandler(type, originalRPC);
                                    var relayPackInvoker = Relay.GeneratePackAndSendMethod(type, originalRPC, InjectMethodRPCHashRegister(relayUnpackInvoker));

                                    Relay.InjectPackMethod(originalRPC, relayPackInvoker);

                                    CecilDebug._createdMethods.Add(relayUnpackInvoker); // Debug
                                    CecilDebug._createdMethods.Add(relayPackInvoker); // Debug
                                    
                                    continue;
                                }
                            }
                        }
                    }

                    Assembly.Write(fs);
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
    static int InjectMethodRPCHashRegister(MethodDefinition unpackInvoke)
    {
        // Compute a stable hash for this method
        int hash = StableHash(unpackInvoke.FullName);

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

        return hash;
    }

   

}