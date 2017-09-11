#if INTERACTIVE
#r @"/home/alexgo/Liquid/bin/Liquid1.dll"
#load "Utility.fsx"
#load "GateFunctions.fsx"
#load "SearchGates.fsx"
#load "QuantumAlgorithm.fsx"
#else
namespace Microsoft.Research.Liquid // Tell the compiler our namespace
#endif
open Utility
open GateFunctions
open SearchGates
open QuantumAlgorithm

open Microsoft.Research.Liquid      // Get necessary Liquid libraries
open Operations                     // Basic gates and operations

module SearchAlgorithms =

    let PhaseFlipX (x:int) (qs:Qubits) =
        let gateIdentifier = "PhaseFlipX{X="+x.ToString()+";Length="+qs.Length.ToString()+"}"
        let gateName = "FP"+ x.ToString()
        let gate = 
            Gate.Build(gateIdentifier, fun () ->
                new Gate(
                    Name    = gateName,
                    Help    = sprintf "Flip phase ofstate |%i>" x,
                    Mat     = (
                        let gateSize = 1 <<< qs.Length
                        let mutable matrixEntriesList = [(x,x,1.,0.)]
                        let iteratableList = List.filter(fun y -> y <> x)  [0 .. (gateSize - 1)]
                        for i in iteratableList do
                            matrixEntriesList <- matrixEntriesList @ [(i,i,-1.0,0.0)]
                        CSMat(gateSize, matrixEntriesList)),
                    Draw    = "\\multigate{#"+(qs.Length-1).ToString()+"}{" + gateName + "}"
                    )
               )
        gate.Run qs

    let oracle (decisionGateFunction: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
        let numberOfQubitsForGate = decisionGateFunction.NumberOfQubitsForGate() + 1
        if qs.Length < numberOfQubitsForGate 
            then failwith (sprintf "Oracle needs at least %i qubits, but only %i are given" numberOfQubitsForGate (qs.Length))
        let indexOfQubitForFlip = decisionGateFunction.NumberOfQubitsForGate()
        decisionGateFunction.Operation !!(qs, decisionGateFunction.IndicesOfQubitsForGate())
        CNOT [qs.[decisionGateFunction.IndexOfQubitForFunctionOutput]; qs.[indexOfQubitForFlip]]
        decisionGateFunction.Operation !!(qs, decisionGateFunction.IndicesOfQubitsForGate())

    let Oracle (decisionGateFunction: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
        let numberOfQubitsForGate = decisionGateFunction.NumberOfQubitsForGate() + 1
        let operationGate = !< decisionGateFunction.Operation qs
        let name = "Oracle_" + operationGate.Name
        let gate = 
            Gate.Build(name, fun () -> 
                new Gate(
                    Qubits = numberOfQubitsForGate,
                    Name = name,
                    Help = "Oracle of "+ operationGate.Help,
                    Draw = sprintf "\\multigate{#%i}{%s}" (numberOfQubitsForGate-1) name,
                    Op =  WrapOp (oracle decisionGateFunction)
                ))
        gate.Run qs

    let flipAboutSuperpositionVector (qs:Qubits) = 
        H >< qs 
        PhaseFlipX 0 qs
        H >< qs

    let FlipAboutSuperpositionVector (qs:Qubits) = 
        let numberOfQubitsForGate = qs.Length
        let name = "2|\\phi><\\phi|+I"
        let gateIdentifier = "FlipAboutSuperPositionVector_" + numberOfQubitsForGate.ToString()
        let gate = 
            Gate.Build(gateIdentifier , fun () -> 
                new Gate(
                    Qubits = numberOfQubitsForGate,
                    Name = name,
                    Help = "Flips the vector over the superposition vector",
                    Draw = sprintf "\\multigate{#%i}{%s}" (numberOfQubitsForGate-1) name,
                    Op =  WrapOp (flipAboutSuperpositionVector)
                )
            )
        gate.Run qs

    let groverIteration (decisionGateFunction: GateFunctions.DecisionGateFunction) (qs:Qubits) =
        let numberOfQubitsForGate = decisionGateFunction.NumberOfQubitsForGate() + 1
        if qs.Length < numberOfQubitsForGate 
            then failwith (sprintf "GroverIteration needs at least %i qubits, but only %i are given" numberOfQubitsForGate (qs.Length))
        Oracle decisionGateFunction qs
        FlipAboutSuperpositionVector !!(qs, decisionGateFunction.IndicesOfQubitsForFunctionInput)

    let GroverIteration (decisionGateFunction: GateFunctions.DecisionGateFunction) (qs:Qubits) =
        let numberOfQubitsForGate = decisionGateFunction.NumberOfQubitsForGate() + 1
        let operationGate = !< decisionGateFunction.Operation qs
        let name = "GroverIteration_" + operationGate.Name
        let gate = 
            Gate.Build(name, fun () -> 
                new Gate(
                    Qubits = numberOfQubitsForGate,
                    Name = name,
                    Help = "Grover iteration of "+ operationGate.Help,
                    Draw = sprintf "\\multigate{#%i}{%s}" (numberOfQubitsForGate-1) name,
                    Op =  WrapOp (groverIteration decisionGateFunction)
                ))
        gate.Run qs

    [<AutoOpen>]
    module GroverSearch =
        
        // See Nielson equation 6.17
        // This is just an estimation which is not quite accurate
        let inline calculateNumberOfGroverInterationsEstimation N M = 
            int(ceil(System.Math.PI * sqrt((float N)/ (float M))/4.))

        // Nielsen equation 6.14
        let inline calculateTheta N M =
            asin ((2. * sqrt ((float M)*((float N)- (float M)))) / (float N))
        
        // Nielson 6.12 the scalar of the beta part
        let inline probabilityOfSuccessAfterKIterations K N M =
            let theta = calculateTheta N M
            (sin ((2.*(float K)+1.)*theta/2.)) ** 2.
        
        let inline numberOfIterations N M = 
            let theta = calculateTheta N M
            int ((System.Math.PI - theta) / (2. * theta))

        let groverIteration (decisionGateFunction: GateFunctions.DecisionGateFunction) (qs:Qubits) =
            let numberOfQubitsForGate = decisionGateFunction.NumberOfQubitsForGate() + 1

            Oracle decisionGateFunction qs
            FlipAboutSuperpositionVector !!(qs, decisionGateFunction.IndicesOfQubitsForFunctionInput)

        let GroverIteration (decisionGateFunction: GateFunctions.DecisionGateFunction) (qs:Qubits) =
            let numberOfQubitsForGate = decisionGateFunction.NumberOfQubitsForGate() + 1
            let operationGate = !< decisionGateFunction.Operation qs
            let name = "GroverIteration_" + operationGate.Name
            let gate = 
                Gate.Build(name, fun () -> 
                    new Gate(
                        Qubits = numberOfQubitsForGate,
                        Name = name,
                        Help = "Grover iteration of "+ operationGate.Help,
                        Draw = sprintf "\\multigate{#%i}{%s}" (numberOfQubitsForGate-1) name,
                        Op =  WrapOp (groverIteration decisionGateFunction)
                    ))
            gate.Run qs

        let numberOfRequiredQubitsForGroverSearch (decisionGateFunction: GateFunctions.DecisionGateFunction) =
            decisionGateFunction.NumberOfQubitsForGate() + 1
        
        // TODO maybe a usage of a prepare main measure sheme? can all algorithm fit this sheme?

        let prepare (decisionGateFunction: GateFunctions.DecisionGateFunction)  (qs:Qubits) =
            let indexOfAncillaQubit = decisionGateFunction.NumberOfQubitsForGate()
            X [qs.[indexOfAncillaQubit]] 
            H >< !!(qs, decisionGateFunction.IndicesOfQubitsForFunctionInput @ [indexOfAncillaQubit])

        let main (decisionGateFunction: GateFunctions.DecisionGateFunction) (numberOfSolutions:int) (qs:Qubits) =
            let numberOfQubitsForGate = numberOfRequiredQubitsForGroverSearch decisionGateFunction
            if qs.Length < numberOfQubitsForGate 
                then failwith (sprintf "GroverSearch needs at least %i qubits, but only %i are given" numberOfQubitsForGate (qs.Length))
            let n = decisionGateFunction.NumberOfQubitsForFunctionInput()
            let N = (1 <<< n) // 2 ** n
            let R = numberOfIterations N numberOfSolutions
            for i in 1 .. R do
                GroverIteration decisionGateFunction qs

        let measure (decisionGateFunction: GateFunctions.DecisionGateFunction) (qs:Qubits) =
            M >< !!(qs, decisionGateFunction.IndicesOfQubitsForFunctionInput)

        let groverSearch (decisionGateFunction: GateFunctions.DecisionGateFunction) (numberOfSolutions:int) (qs:Qubits) =
            let numberOfQubitsForGate = numberOfRequiredQubitsForGroverSearch decisionGateFunction
            if qs.Length < numberOfQubitsForGate 
                then failwith (sprintf "GroverSearch needs at least %i qubits, but only %i are given" numberOfQubitsForGate (qs.Length))
            let indexOfAncillaQubit = decisionGateFunction.NumberOfQubitsForGate()

            X [qs.[indexOfAncillaQubit]] 
            H >< !!(qs, decisionGateFunction.IndicesOfQubitsForFunctionInput @ [indexOfAncillaQubit])

            let n = decisionGateFunction.NumberOfQubitsForFunctionInput()
            let N = (1 <<< n) // 2 ** n
            let R = numberOfIterations N numberOfSolutions
            for i in 1 .. R do
                GroverIteration decisionGateFunction qs


        let GroverSearch (decisionGateFunction: GateFunctions.DecisionGateFunction) (numberOfSolutions:int) (qs:Qubits) = 
            let numberOfQubitsForGate = numberOfRequiredQubitsForGroverSearch decisionGateFunction
            let operationGate = !< decisionGateFunction.Operation qs 
            let uniqueIdentifier = "GroverSearch{DecisionFunction=" + operationGate.Name+"}" // TODO add hash value for uniquenes
            let name = "GroverSearch_"+ operationGate.Name
            let gate = 
                Gate.Build(uniqueIdentifier, fun () -> 
                    new Gate(
                        Qubits = numberOfQubitsForGate,
                        Name = name,
                        Help = "Grover search of "+ operationGate.Help,
                        Draw = sprintf "\\multigate{#%i}{%s}" (numberOfQubitsForGate-1) name,
                        Op =  WrapOp (groverSearch decisionGateFunction numberOfSolutions)
                    ))
            gate.Run qs

        let groverSearchWithMeasurement (decisionGateFunction: GateFunctions.DecisionGateFunction) (numberOfSolutions:int) (qs:Qubits) =
            groverSearch decisionGateFunction numberOfSolutions qs
            M >< !!(qs, decisionGateFunction.IndicesOfQubitsForFunctionInput)

        let GroverSearchWithMeasurement (decisionGateFunction: GateFunctions.DecisionGateFunction) (numberOfSolutions:int) (qs:Qubits) =
            let numberOfQubitsForGate = numberOfRequiredQubitsForGroverSearch decisionGateFunction
            let operationGate = !< decisionGateFunction.Operation qs 
            let uniqueIdentifier = "GroverSearch{DecisionFunction=" + operationGate.Name+"}" // TODO add hash value for uniquenes
            let name = "GroverSearch_"+ operationGate.Name
            let gate = 
                Gate.Build(uniqueIdentifier, fun () -> 
                    new Gate(
                        Qubits = numberOfQubitsForGate,
                        Name = name,
                        Help = "Grover search of "+ operationGate.Help + " with meauserment.",
                        Draw = sprintf "\\multigate{#%i}{%s}" (numberOfQubitsForGate-1) name,
                        Op =  WrapOp (groverSearchWithMeasurement decisionGateFunction numberOfSolutions)
                    ))
            gate.Run qs

        let getResultAfterGroverSearch (decisionGateFunction: GateFunctions.DecisionGateFunction) (qs:Qubits) =
            let solution = Utility.qubitsToInt !!(qs, decisionGateFunction.IndicesOfQubitsForFunctionInput)
            solution

        let create (decisionGateFunction:GateFunctions.DecisionGateFunction) (numberOfSolutions:int) :QuantumAlgorithm.T = 
            {
            Name="GroverSearch";
            NumberOfRequiredQubits = numberOfRequiredQubitsForGroverSearch decisionGateFunction;
            RunAlgorithm=GroverSearchWithMeasurement decisionGateFunction numberOfSolutions;
            GetResult=getResultAfterGroverSearch decisionGateFunction
            }

    module OzhigovSearch =

        let numberOfIterations N = 
            int(round(System.Math.PI * sqrt(float N)/sqrt(8.) ))

        let W_i (f_i: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
            H >< !!(qs, f_i.IndicesOfQubitsForFunctionInput)

        let R0_i (f_i: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
            let xy = List.chunkBySize (f_i.NumberOfQubitsForFunctionInput()/2) f_i.IndicesOfQubitsForFunctionInput
            PhaseFlipX 0 !!(qs, xy.[0])
            PhaseFlipX 0 !!(qs, xy.[1])

        let F_i (f_i: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
            Oracle f_i qs
            
        let P (f_1: GateFunctions.DecisionGateFunction) (qs:Qubits) =
            for i in 0 .. f_1.NumberOfQubitsForFunctionInput()-1 do 
                CNOT [qs.[i+f_1.NumberOfQubitsForFunctionInput()]; qs.[i]]

        let Z_2 (f_2: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
            W_i f_2 qs
            R0_i f_2 qs
            W_i f_2 qs

        let indicesOfInputForZ_i (f_i: GateFunctions.DecisionGateFunction) (f_ip1: GateFunctions.DecisionGateFunction) (offset:int) =
            let remappedf_ip1 = List.take (f_i.NumberOfQubitsForFunctionInput()) f_ip1.IndicesOfQubitsForFunctionInput
                                |> List.map (fun x -> offset + x)
            f_i.IndicesOfQubitsForFunctionInput @ remappedf_ip1

        let Z_i (f_i: GateFunctions.DecisionGateFunction) (f_ip1: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
            let indexOfQubitForPhaseFlipForF1 = f_i.NumberOfQubitsForGate() + f_ip1.NumberOfQubitsForGate()
            let indexOfQubitForPhaseFlipForF2 = f_i.NumberOfQubitsForGate() + f_ip1.NumberOfQubitsForGate() + 1
            let qsf_i = !!(qs, f_i.IndicesOfQubitsForGate() @ [indexOfQubitForPhaseFlipForF1])
            let offset = f_i.NumberOfQubitsForGate()
            let indicesOfInputForZi = indicesOfInputForZ_i f_i f_ip1 offset
            P f_i !!(qs,indicesOfInputForZi)
            F_i f_ip1 !!(qs, (List.map (fun x -> offset + x) (f_ip1.IndicesOfQubitsForGate())) @ [indexOfQubitForPhaseFlipForF2]) 
            F_i f_i qsf_i 
            P f_i !!(qs,indicesOfInputForZi)
            //Z_2 f_i qsf_i 

        //let offset (nuberOfQubitsOff_1:int) (i:int) =
        //    0

        let ozhigovSearch (f_1: GateFunctions.DecisionGateFunction) (f_2: GateFunctions.DecisionGateFunction) (qs:Qubits) =
            let N = (1 <<< f_1.NumberOfQubitsForFunctionInput())
            let numberOfOzhigovIterations = numberOfIterations N
            let indexOfQubitForPhaseFlipForF1 = f_1.NumberOfQubitsForGate() + f_2.NumberOfQubitsForGate()
            let indexOfQubitForPhaseFlipForF2 = f_1.NumberOfQubitsForGate() + f_2.NumberOfQubitsForGate() + 1
            let offset = f_1.NumberOfQubitsForGate()
            let mappedF2IndicesOfQubitsForGate = List.map (fun i -> offset+i) <| f_2.IndicesOfQubitsForGate()
            let mappedF2IndicesOfQubitsForFunctionInput = List.map (fun i -> offset+i) f_2.IndicesOfQubitsForFunctionInput
            X >< !!(qs, [indexOfQubitForPhaseFlipForF1;indexOfQubitForPhaseFlipForF2])
            H >< !!(qs, mappedF2IndicesOfQubitsForFunctionInput @ [indexOfQubitForPhaseFlipForF1;indexOfQubitForPhaseFlipForF2])
            for i in 1 .. numberOfOzhigovIterations do
                Z_i f_1 f_2 qs 
                Z_2 f_2 !!(qs, mappedF2IndicesOfQubitsForGate)
            M >< !!(qs, mappedF2IndicesOfQubitsForFunctionInput )

        let numberOfRequiredQubits (f_1: GateFunctions.DecisionGateFunction) (f_2: GateFunctions.DecisionGateFunction) =
            f_1.NumberOfQubitsForGate() + f_2.NumberOfQubitsForGate() + 2

        let OzhigovSearch (f_1: GateFunctions.DecisionGateFunction) (f_2: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
            let numberOfQubitsForGate = numberOfRequiredQubits f_1 f_2
            if qs.Length < numberOfQubitsForGate 
                then failwith (sprintf "OzhigovSearch needs at least %i qubits, but only %i are given" numberOfQubitsForGate (qs.Length))
            let operationGateF1 = !< f_1.Operation qs 
            let operationGateF2 = !< f_2.Operation qs 
            let uniqueIdentifier = "Ozhigov{F1=" + operationGateF1.Name+";F2=" + operationGateF2.Name+"}"
            let name = String.concat "_" ["OzhigovSearch"; operationGateF1.Name; operationGateF2.Name]
            let gate = 
                Gate.Build(uniqueIdentifier, fun () -> 
                    new Gate(
                        Qubits = numberOfQubitsForGate,
                        Name = name,
                        Help = String.concat " " ["OzhigovSearch of"; operationGateF1.Help; "and"; operationGateF2.Help],
                        Draw = sprintf "\\multigate{#%i}{%s}" (numberOfQubitsForGate-1) name,
                        Op =  WrapOp (ozhigovSearch f_1 f_2)
                    ))
            gate.Run qs

        let getResult (f_1: GateFunctions.DecisionGateFunction) (f_2: GateFunctions.DecisionGateFunction) (qs:Qubits) =
            let offset = f_1.NumberOfQubitsForGate()
            let mappedF2IndicesOfQubitsForFunctionInput = List.map (fun i -> offset+i) f_2.IndicesOfQubitsForFunctionInput
            let solution = Utility.qubitsToInt !!(qs, mappedF2IndicesOfQubitsForFunctionInput )
            solution

        let create (f_1: GateFunctions.DecisionGateFunction) (f_2: GateFunctions.DecisionGateFunction) :QuantumAlgorithm.T = 
            {
            Name="OzhigovSearch";
            NumberOfRequiredQubits = numberOfRequiredQubits f_1 f_2;
            RunAlgorithm=OzhigovSearch f_1 f_2;
            GetResult=getResult f_1 f_2
            }

//    module GroverStructuralSearch = 
//
//        // basically a groverIteration
//        let iteration11 (f_1: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
//            Oracle f_1 !!(qs, f_1.IndicesOfQubitsForGate() @ [ancillaQubitsForF1]
//            FlipAboutSuperpositionVector !!(qs, f_1.IndicesOfQubitsForFunctionInput) 
//
//        let op11 (f_1: GateFunctions.DecisionGateFunction) (numberOfSolutions:int) (qs:Qubits) =
//            GroverSearch.main f_1 numberOfSolutions !!(qs, f_1.IndicesOfQubitsForGate() @ [indexOfAncillaQubit]) 
//
//        let op13 (f_1: GateFunctions.DecisionGateFunction) (numberOfSolutions:int) (qs:Qubits) = 
//            GroverSearch.main f_1 numberOfSolutions !!(qs, f_1.IndicesOfQubitsForGate() @ [indexOfAncillaQubit]) 
//            FlipAboutSuperpositionVector !!(qs, f_1.IndicesOfQubitsForFunctionInput) 
//            GroverSearch.main f_1 numberOfSolutions !!(qs, f_1.IndicesOfQubitsForGate() @ [indexOfAncillaQubit]) 
//        let op8
//            let circuit = Circuit.Compile GroverSearch.main f_2 1 !!(qs, f_2.IndicesOfQubitsForGate() @ [indexOfAncillaQubit]) 
//            circuit.Run 
//            // Adj gate does not work
//            Oracle qs !!(qs, f_2.IndicesOfQubitsForGate() @ [indexOfAncillaQubit]) 
//
//        let op14
//
//        let iteration (f_1: GateFunctions.DecisionGateFunction) (f_2: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
//            Oracle f_1 !!(qs, f_1.IndicesOfQubitsForGate() @ [ancillaQubitsForF1]
//            Oracle f_2 !!(qs, f_1.IndicesOfQubitsForGate() @ [ancillaQubitsForF2]
//            FlipAboutSuperpositionVector !!(qs, f_1.IndicesOfQubitsForFunctionInput) 
//            FlipAboutSuperpositionVector !!(qs, f_2.IndicesOfQubitsForFunctionInput) 
//
//        let GroverStructuralSearch (f_1: GateFunctions.DecisionGateFunction) (f_2: GateFunctions.DecisionGateFunction) (qs:Qubits) = 
//            let searchSpaceSize = (1 <<< f_1.NumberOfQubitsForFunctionInput())
//            let indexOfQubitForPhaseFlipForF1 = f_1.NumberOfQubitsForGate() + f_2.NumberOfQubitsForGate()
//            let indexOfQubitForPhaseFlipForF2 = f_1.NumberOfQubitsForGate() + f_2.NumberOfQubitsForGate() + 1
//            let offset = f_1.NumberOfQubitsForGate()
//            let mappedF2IndicesOfQubitsForGate = List.map (fun i -> offset+i) <| f_2.IndicesOfQubitsForGate()
//            let mappedF2IndicesOfQubitsForFunctionInput = List.map (fun i -> offset+i) f_2.IndicesOfQubitsForFunctionInput
//            X >< !!(qs, [indexOfQubitForPhaseFlipForF1;indexOfQubitForPhaseFlipForF2])
//            H >< !!(qs, mappedF2IndicesOfQubitsForFunctionInput @ [indexOfQubitForPhaseFlipForF1;indexOfQubitForPhaseFlipForF2])
