#if INTERACTIVE
#r @"/home/alexgo/Liquid/bin/Liquid1.dll"
#load "GateFunctions.fsx"
#else
namespace Microsoft.Research.Liquid // Tell the compiler our namespace
#endif
open GateFunctions

open Microsoft.Research.Liquid      // Get necessary Liquid libraries

module SearchGates = 

    [<AutoOpen>]
    module UnstructuredSearchGates = 

        let isEqualToInLastBit(oneInt:int, otherInt:int) =
            if (((oneInt % 2) = 0) && ((otherInt % 2) = 0)) || (((oneInt % 2) = 1) && ((otherInt % 2) = 1)) then true
            else false

        let areEqualOmittingTheLastBit(oneInt:int, otherInt:int) = 
            if isEqualToInLastBit(oneInt, otherInt) then
                oneInt = otherInt
            elif ((oneInt % 2) = 0) then
                (oneInt+1) = otherInt
            else
                oneInt = otherInt+1

        // 1 0 0 0 0 0 0 0
        // 0 0 0 0 1 0 0 0
        // 0 0 s s 0 0 0 0
        // 0 0 s s 0 0 0 0
        //        |-------
        // 0 1 0 0|0 0 0 0
        // 0 0 0 0|0 1 0 0
        // 0 0 0 0|0 0 1 0
        // 0 0 0 0|0 0 0 1
        // The solution block marked with s is the block where the row and and column number
        // equals the solution omitting the last bit (the most right)
        
        let removeSearchBlockIndicesFromList list solution =
            List.filter (fun x -> areEqualOmittingTheLastBit(x,solution) = false) list

        // 1 0 0 0 0 0 0 0
        // 0 0 0 0 1 0 0 0
        // 0 0 x x 0 0 0 0
        // 0 0 x x 0 0 0 0
        //        |-------
        // 0 1 0 0|0 0 0 0
        // 0 0 0 0|0 1 0 0
        // 0 0 0 0|0 0 1 0
        // 0 0 0 0|0 0 0 1
        let solutionBlockMatrixEntries solution =
            let mutable matrixEntries = []
            if ((solution % 2) = 0) then
                matrixEntries <- matrixEntries @ [solution+1,solution,1.,0.]
                matrixEntries <- matrixEntries @ [solution,solution+1,1.,0.]
            else
                matrixEntries <- matrixEntries @ [solution-1,solution-1,1.,0.]
                matrixEntries <- matrixEntries @ [solution,solution,1.,0.]
            matrixEntries


        // x 0 0 0 0 0 0 0
        // 0 0 0 0 x 0 0 0
        // 0 0 s s 0 0 0 0
        // 0 0 s s 0 0 0 0
        //        |-------
        // 0 x 0 0|0 0 0 0
        // 0 0 0 0|0 1 0 0
        // 0 0 0 0|0 0 x 0
        // 0 0 0 0|0 0 0 1
        let upperLeftLowerLeftUpperRightExceptSolutionBlockMatrixEntries solution searchSpaceSize =
            let searchSpaceWithoutSolutionBlock = removeSearchBlockIndicesFromList [0..searchSpaceSize-1] solution
            let mutable matrixEntries = []
            for i in 0 .. (searchSpaceSize-1) do
                if ((i % 2) = 0 && areEqualOmittingTheLastBit(i,solution) = false) then
                    matrixEntries <- matrixEntries @ [i,i,1.,0.]
                elif ((i % 2) = 1 && areEqualOmittingTheLastBit(i,solution) = false) then
                    matrixEntries <- matrixEntries @ [i,(searchSpaceSize-1)+i,1.,0.]
                    matrixEntries <- matrixEntries @ [(searchSpaceSize-1)+i,i,1.,0.]
                elif ((i % 2) = 1 && areEqualOmittingTheLastBit(i,solution) = true) then
                    matrixEntries <- matrixEntries @ [(searchSpaceSize-1)+i,(searchSpaceSize-1)+i,1.,0.]

                        
            matrixEntries

        // TODO function is too big, partial it in smaller modules
        let unstructuredSearchGateMatrix (solution:int) (numberOfQubitsForSearchSpace:int) =
            let searchSpaceSize = 1 <<< numberOfQubitsForSearchSpace
            let mutable matrixEntries = []
            matrixEntries <- matrixEntries @ (solutionBlockMatrixEntries solution)
            matrixEntries <- matrixEntries @ (upperLeftLowerLeftUpperRightExceptSolutionBlockMatrixEntries solution searchSpaceSize)
            
            // lower right part 
            for i in 0..(searchSpaceSize/2)-1 do
                matrixEntries <- matrixEntries @ [(searchSpaceSize+1)+2*i,(searchSpaceSize+1)+2*i,1.,0.]

            matrixEntries


        // TODO now it only supports one solution, make it more flexible, so it supports multiple solution
        // To create a unitary gate a additional qubit is needed for junk solution
        // the solution is saved in qs.[numberOfQubitsForSearchSpace-1]
        let UnstructuredSearchWithSingleSolution (solution:int) (numberOfQubitsForSearchSpace:int) (qs:Qubits)=
            let gate =
                Gate.Build("UnstructuredSearchWithSingleSolution{Solution=" + solution.ToString() + ";Size=" + numberOfQubitsForSearchSpace.ToString()+"}" , fun () -> // Rember always to have a unique name for gates
                    let requiredQubits = numberOfQubitsForSearchSpace+1
                    if (requiredQubits > qs.Length) then
                        failwith "The number of qubits is not enough to execute the gate."
                    let searchSpaceSize = 1 <<< numberOfQubitsForSearchSpace
                    if (solution >= searchSpaceSize) then
                        failwith "The solution is outside of the search space."

                    let gateSize = 1 <<< requiredQubits
                    let name = "USF_" + (solution.ToString()) + "_" + (numberOfQubitsForSearchSpace.ToString())
                    let matrixEntries = unstructuredSearchGateMatrix solution numberOfQubitsForSearchSpace
                    new Gate(
                        Name = name,
                        Help = sprintf "Unstructured search gate for solution %i" solution,
                        Mat = CSMat(gateSize, matrixEntries), 
                        Draw = sprintf "\\multigate{#%i}{%s}" (requiredQubits-1) name
                    )
                )
            gate.Run qs

        let createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution (solution:int) (numberOfQubitsForSearchSpace:int) =
            let searchSpaceSize = 1 <<< numberOfQubitsForSearchSpace
            if (solution >= searchSpaceSize) then
                failwith "The solution is outside of the search space."
            let operation = UnstructuredSearchWithSingleSolution solution numberOfQubitsForSearchSpace
            let indicesOfQubitsForFunctionInput = [1..numberOfQubitsForSearchSpace]
            let indexOfQubitForFunctionOutput = numberOfQubitsForSearchSpace
            let indicesOfAncillaQubits = [0]
            new GateFunctions.DecisionGateFunction(operation, indicesOfQubitsForFunctionInput, indexOfQubitForFunctionOutput, indicesOfAncillaQubits) 
    
