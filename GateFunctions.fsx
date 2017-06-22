#if INTERACTIVE
#r @"/home/alexgo/Liquid/bin/Liquid1.dll"
#else
namespace Microsoft.Research.Liquid // Tell the compiler our namespace
#endif

open Microsoft.Research.Liquid      // Get necessary Liquid libraries

module GateFunctions = 

    // The Operation should always inherit a defined gate to avoid rendering problems
    // What does "inherit a defined gate" mean: 
    // let operation (qs:Qubits) = let gate = new Gate( ... ); gate.Run qs
    // or also a build gate 
    // let operation (qs:Qubits) = let gate = Gate.Build( ... ); gate.Run qs
    type GateFunction(operation:Qubits->Unit, indicesOfQubitsForFunctionInput: int list, indicesOfQubitsForFunctionOutput: int list, ?indicesOfAncillaQubits: int list) =
        member this.Operation = operation
        member this.IndicesOfQubitsForFunctionInput = indicesOfQubitsForFunctionInput
        member this.IndicesOfQubitsForFunctionOutput = indicesOfQubitsForFunctionOutput
        member this.IndicesOfAncillaQubits = 
            match indicesOfAncillaQubits with
                | Some x -> x
                | None -> []
        member this.NumberOfQubitsForFunctionInput() = this.IndicesOfQubitsForFunctionInput.Length       
        member this.NumberOfQubitsForFunctionOutput() = this.IndicesOfQubitsForFunctionOutput.Length
        member this.NumberOfAncillaQubits() = this.IndicesOfAncillaQubits.Length
        member this.NumberOfQubitsForGate() = this.NumberOfAncillaQubits() + this.NumberOfQubitsForFunctionInput()
        member this.IndicesOfQubitsForGate() = this.IndicesOfAncillaQubits @ this.IndicesOfQubitsForFunctionInput

    type DecisionGateFunction(operation:Qubits->Unit, indicesOfQubitsForFunctionInput: int list, indexOfQubitForFunctionOutput: int, ?indicesOfAncillaQubits: int list) =
        inherit GateFunction(operation, indicesOfQubitsForFunctionInput, [indexOfQubitForFunctionOutput], ?indicesOfAncillaQubits=indicesOfAncillaQubits)
        member this.IndexOfQubitForFunctionOutput = this.IndicesOfQubitsForFunctionOutput.[0]
