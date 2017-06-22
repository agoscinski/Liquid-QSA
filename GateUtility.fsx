#if INTERACTIVE
#r @"/home/alexgo/Liquid/bin/Liquid1.dll"
#else
namespace Microsoft.Research.Liquid // Tell the compiler our namespace
#endif

open Microsoft.Research.Liquid      // Get necessary Liquid libraries
open Operations

module GateUtility = 

    // Just to show how to draw safely, unsafe drawing is wrong, liquid automatically draws safe
    // My assumption was wrong, just keep this for maybe later usage

    let safeDrawMultiGateString numberOfRequiredQubitsForGate gateName= 
        List.map (fun i -> sprintf "\\qwx[#%i]\\go[#%i]\\gate{%s}" i i gateName) [1 .. numberOfRequiredQubitsForGate-1] 
        |> (@) [sprintf "\\gate{%s}" gateName] 
        |> String.concat ""

    let GateWithUnsafeDraw (qs:Qubits) =
        let gate =
            new Gate(
                Qubits = 2,
                Name = "UnsafeDraw",
                Help = "Gate to visualize the problem with multigate draw",
                Draw = "\\multigate{#1}{UnsafeDraw}",
                Op = WrapOp(fun (qs:Qubits) -> CNOT qs)
            )
        gate.Run qs

    let GateWithSafeDraw (qs:Qubits) =
        let gate =
            new Gate(
                Qubits = 2,
                Name = "SafeDraw",
                Help = "Gate to visualize the problem with multigate draw",
                Draw = (safeDrawMultiGateString 2 "SafeDraw"),
                Op = WrapOp(fun (qs:Qubits) -> CNOT qs)
            )
        gate.Run qs

    let showUnsafeDrawProblem () =
        let qs = Ket(3).Qubits
        let op (qs:Qubits) =
            GateWithUnsafeDraw !!(qs, [0;2])
            GateWithSafeDraw !!(qs, [0;2])
        let circ = Circuit.Compile op qs
        circ.RenderHT("UnsafeDraw", detail=0)

#if INTERACTIVE
//GateUtility.showUnsafeDrawProblem()
#endif
