#load "SearchGates.fsx"
open SearchGates

// Testing

// Problems with testing: 
// - it is not clear which function has problems
// - matrix parser would make the code more readable
// TODO use this one tutorial open on my tablet


module SearchGatesTests = 
    
    [<AutoOpen>]
    module UnstructuredSearchWithSingleSolutionTests = 

        let areEqualInContentAndLength oneList otherList = 
            (Set.ofList oneList) = (Set.ofList otherList) && (oneList.Length = otherList.Length)
           
        [<Test>]
        let ``Test areEqualInContentAndLength for [0;1] and [1;0]``() = 
            test <@ areEqualInContentAndLength [0;1] [1;0] @>

        [<Test>]
        let ``Test areEqualInContentAndLength for [0;0] and [1;0]``() = 
            test <@ (areEqualInContentAndLength [0;0] [1;0]) = false @>

        [<Test>]
        let ``Test areEqualOmittingTheLastBit for values 0-7``() =
            test <@ areEqualOmittingTheLastBit(0,1) @>
            test <@ areEqualOmittingTheLastBit(2,3) @>
            test <@ areEqualOmittingTheLastBit(4,5) @>
            test <@ areEqualOmittingTheLastBit(6,7) @>

        [<Test>]
        let ``Test removeSearchBlockIndicesFromList for list [0..4] and solution 0``() =
            let indicesWithoutSearchBlockIndices = removeSearchBlockIndicesFromList [0..4] 0
            let assertedIndicesWithoutSearchBlockIndices = [2..4]
            test <@ areEqualInContentAndLength indicesWithoutSearchBlockIndices assertedIndicesWithoutSearchBlockIndices @>

        [<Test>]
        let ``Test removeSearchBlockIndicesFromList for list [0..4] and solution 1``() =
            let indicesWithoutSearchBlockIndices = removeSearchBlockIndicesFromList [0..4] 1
            let assertedIndicesWithoutSearchBlockIndices = [2..4]
            test <@ areEqualInContentAndLength indicesWithoutSearchBlockIndices assertedIndicesWithoutSearchBlockIndices @>

        // 0 0 0 0 0 0 0 0
        // 0 0 0 0 0 0 0 0
        // 0 0 1 0 0 0 0 0
        // 0 0 0 0 0 0 1 0
        //        |-------
        // 0 0 0 0|1 0 0 0
        // 0 0 0 0|0 0 0 0
        // 0 0 0 1|0 0 0 0
        // 0 0 0 0|0 0 0 0
        [<Test>]
        let ``Test upperLeftLowerLeftUpperRightExceptSolutionBlockMatrixEntries for solution 0 and SearchSpaceSize 4``() =
            let matrixEntries = upperLeftLowerLeftUpperRightExceptSolutionBlockMatrixEntries 0 4
            let assertedMatrixEntries = [2,2,1.,0.; 6,3,1.,0.; 3,6,1.,0.;4,4,1.,0.]
            test <@ areEqualInContentAndLength assertedMatrixEntries matrixEntries @>

        [<Test>]
        let ``Test upperLeftLowerLeftUpperRightExceptSolutionBlockMatrixEntries for solution 1 and SearchSpaceSize 4``() =
            let matrixEntries = upperLeftLowerLeftUpperRightExceptSolutionBlockMatrixEntries 1 4
            let assertedMatrixEntries = [2,2,1.,0.; 6,3,1.,0.; 3,6,1.,0.;4,4,1.,0.]
            test <@ areEqualInContentAndLength assertedMatrixEntries matrixEntries @>
        
        // TODO if you want to use CSMat you have to add a tol paramater and for this you have to understand option types, I currently do not
        //[<Test>]
        //let ``Build unstructured search gate matrix for solution 0 and numberOfQubitsForSearchSpace 1``() =
        //    let unstructuredSearchCSMat = CSMat(4, (unstructuredSearchGateMatrix 0 1))
        //    let assertedUnstructuredSearchCSMat = CSMat(4, [1,0,1.,0.; 0,1,1.,0. ; 2,1,1.,0.; 1,2,1.,0. ; 3,3,1.,0.])
        //    test <@ assertedUnstructuredSearchCSMat.Equals(unstructuredSearchCSMat) @>

        // 0 1 0 0
        // 1 0 0 0
        //    |---
        // 0 0|1 0
        // 0 0|0 1
        [<Test>]
        let ``Build unstructured search gate matrix for solution 0 and numberOfQubitsForSearchSpace 1``() =
            let unstructuredSearchGateMatrix = unstructuredSearchGateMatrix 0 1
            let assertedUnstructuredSearchSingleSolutionGateMatrix = [1,0,1.,0.; 0,1,1.,0. ; 2,2,1.,0.; 3,3,1.,0.]
            test <@ areEqualInContentAndLength assertedUnstructuredSearchSingleSolutionGateMatrix unstructuredSearchGateMatrix @>

        // 1 0 0 0
        // 0 1 0 0
        //    |---
        // 0 0|1 0
        // 0 0|0 1
        [<Test>]
        let ``Build unstructured search gate matrix for solution 1 and numberOfQubitsForSearchSpace 1``() =
            let unstructuredSearchGateMatrix = unstructuredSearchGateMatrix 1 1
            let assertedUnstructuredSearchSingleSolutionGateMatrix = [0,0,1.,0.; 1,1,1.,0. ; 2,2,1.,0. ; 3,3,1.,0.]
            test <@ areEqualInContentAndLength assertedUnstructuredSearchSingleSolutionGateMatrix unstructuredSearchGateMatrix @>

        // 0 1 0 0 0 0 0 0
        // 1 0 0 0 0 0 0 0
        // 0 0 1 0 0 0 0 0
        // 0 0 0 0 0 0 1 0
        //        |-------
        // 0 0 0 0|1 0 0 0
        // 0 0 0 0|0 1 0 0
        // 0 0 1 0|0 0 0 0
        // 0 0 0 0|0 0 0 1
        [<Test>]
        let ``Build unstructured search gate matrix for solution 0 and numberOfQubitsForSearchSpace 2``() =
            let unstructuredSearchGateMatrix = unstructuredSearchGateMatrix 0 2
            let assertedUnstructuredSearchSingleSolutionGateMatrix = 
                [0,1,1.,0.; 1,0,1.,0.;
                2,2,1.,0.; 4,4,1.,0.; 3,6,1.,0.; 6,3,1.0,0.;
                5,5,1.,0.; 7,7,1.,0.]
            test <@ areEqualInContentAndLength assertedUnstructuredSearchSingleSolutionGateMatrix unstructuredSearchGateMatrix @>

        // 1 0 0 0 0 0 0 0
        // 0 1 0 0 0 0 0 0
        // 0 0 1 0 0 0 0 0
        // 0 0 0 0 0 0 1 0
        //        |-------
        // 0 0 0 0|1 0 0 0
        // 0 0 0 0|0 1 0 0
        // 0 0 1 0|0 0 0 0
        // 0 0 0 0|0 0 0 1
        [<Test>]
        let ``Build unstructured search gate matrix for solution 1 and numberOfQubitsForSearchSpace 2``() =
            let unstructuredSearchGateMatrix = unstructuredSearchGateMatrix 1 2
            let assertedUnstructuredSearchSingleSolutionGateMatrix = 
                [0,0,1.,0.; 1,1,1.,0.;
                2,2,1.,0.; 4,4,1.,0.; 3,6,1.,0.; 6,3,1.0,0.;
                5,5,1.,0.; 7,7,1.,0.]
            test <@ areEqualInContentAndLength assertedUnstructuredSearchSingleSolutionGateMatrix unstructuredSearchGateMatrix @>

        // 1 0 0 0 0 0 0 0
        // 0 0 0 0 1 0 0 0
        // 0 0 0 1 0 0 0 0
        // 0 0 1 0 0 0 0 0
        //        |-------
        // 0 1 0 0|0 0 0 0
        // 0 0 0 0|0 1 0 0
        // 0 0 0 0|0 0 1 0
        // 0 0 0 0|0 0 0 1
        [<Test>]
        let ``Build unstructured search gate matrix for solution 2 and numberOfQubitsForSearchSpace 2``() =
            let unstructuredSearchGateMatrix = unstructuredSearchGateMatrix 1 2
            let assertedUnstructuredSearchSingleSolutionGateMatrix = 
                [2,3,1.,0.; 3,2,1.,0.;
                0,0,1.,0.; 4,1,1.,0.; 1,4,1.,0.; 6,6,1.,0.;
                5,5,1.,0.; 7,7,1.,0.]
            test <@ areEqualInContentAndLength assertedUnstructuredSearchSingleSolutionGateMatrix unstructuredSearchGateMatrix @>

        // 1 0 0 0 0 0 0 0
        // 0 0 0 0 1 0 0 0
        // 0 0 1 0 0 0 0 0
        // 0 0 0 1 0 0 0 0
        //        |-------
        // 0 1 0 0|0 0 0 0
        // 0 0 0 0|0 1 0 0
        // 0 0 0 0|0 0 1 0
        // 0 0 0 0|0 0 0 1
        [<Test>]
        let ``Build unstructured search gate matrix for solution 3 and numberOfQubitsForSearchSpace 2``() =
            let unstructuredSearchGateMatrix = unstructuredSearchGateMatrix 1 2
            let assertedUnstructuredSearchSingleSolutionGateMatrix = 
                [2,2,1.,0.; 3,3,1.,0.;
                0,0,1.,0.; 4,1,1.,0.; 1,4,1.,0.; 6,6,1.,0.;
                5,5,1.,0.; 7,7,1.,0.]
            test <@ areEqualInContentAndLength assertedUnstructuredSearchSingleSolutionGateMatrix unstructuredSearchGateMatrix @>

        [<LQD>]
        let testBuildUnstructuredSearchFunction() =
            let qs = Ket(5).Qubits
            let c = Circuit.Compile (UnstructuredSearchSingleSolutionGate 0 2) qs
            c.Dump(showInd)
            c.RenderHT("SearchFunctionGate")

        let runTests() =
            ``Test areEqualInContentAndLength for [0;1] and [1;0]``()
            ``Test areEqualInContentAndLength for [0;0] and [1;0]``()
            ``Test areEqualOmittingTheLastBit for values 0-7``()
            ``Test removeSearchBlockIndicesFromList for list [0..4] and solution 0``()
            ``Test removeSearchBlockIndicesFromList for list [0..4] and solution 1``()
            ``Test upperLeftLowerLeftUpperRightExceptSolutionBlockMatrixEntries for solution 0 and SearchSpaceSize 4``()
            ``Test upperLeftLowerLeftUpperRightExceptSolutionBlockMatrixEntries for solution 1 and SearchSpaceSize 4``()
            ``Build unstructured search gate matrix for solution 0 and numberOfQubitsForSearchSpace 1``()
            ``Build unstructured search gate matrix for solution 1 and numberOfQubitsForSearchSpace 1``()
            ``Build unstructured search gate matrix for solution 0 and numberOfQubitsForSearchSpace 2``()    
            ``Build unstructured search gate matrix for solution 1 and numberOfQubitsForSearchSpace 2``()
            testBuildUnstructuredSearchFunction()
