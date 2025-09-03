using System;
using System.Collections.Generic;
using Mono.Cecil;

public static class CecilDebug
{
    public static List<MethodDefinition> _createdMethods = new();

    public static void DumpMethodIL(MethodDefinition method)
    {
        Console.WriteLine($"Method: {method.FullName}");

        if (!method.HasBody)
        {
            Console.WriteLine("   (no body)");
            return;
        }

        int index = 0;
        foreach (var instr in method.Body.Instructions)
        {
            string operand = instr.Operand != null ? OperandToString(instr.Operand) : "";
            Console.WriteLine($"   IL_{index:D3}: {instr.OpCode,-12} {operand}");
            index++;
        }
    }

    private static string OperandToString(object operand)
    {
        switch (operand)
        {
            case MethodReference m: return m.FullName;
            case FieldReference f:  return f.FullName;
            case TypeReference t:   return t.FullName;
            case string s:          return $"\"{s}\"";
            default:                return operand.ToString();
        }
    }
}