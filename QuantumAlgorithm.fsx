#if INTERACTIVE
#r @"/home/alexgo/Liquid/bin/Liquid1.dll"
#else
namespace Microsoft.Research.Liquid // Tell the compiler our namespace
#endif

open Microsoft.Research.Liquid      // Get necessary Liquid libraries
open Util

module QuantumAlgorithm =

    type T = {Name: string; NumberOfRequiredQubits: int; RunAlgorithm: Qubits->Unit; GetResult: Qubits -> int}

    let initQubits quantumAlgorithm =
        Ket(quantumAlgorithm.NumberOfRequiredQubits).Qubits

    let run quantumAlgorithm (qs:Qubits) =
        quantumAlgorithm.RunAlgorithm qs

    let getResult quantumAlgorithm (qs:Qubits) =
        quantumAlgorithm.GetResult qs

    let prepareAndRunAndGetResult quantumAlgorithm = 
        let qs = initQubits quantumAlgorithm 
        run quantumAlgorithm qs
        getResult quantumAlgorithm qs 

    let compile quantumAlgorithm =
        Circuit.Compile (run quantumAlgorithm) (initQubits quantumAlgorithm)

    let render quantumAlgorithm detail =
        (compile quantumAlgorithm).Fold().RenderHT(quantumAlgorithm.Name, ?detail=detail)

    let dump quantumAlgorithm =
        (compile quantumAlgorithm).Dump(showInd)

    let renderAndDump quantumAlgorithm = 
        let circuit = compile quantumAlgorithm 
        circuit.RenderHT(quantumAlgorithm.Name)
        circuit.Dump(showInd)
