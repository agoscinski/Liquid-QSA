#if INTERACTIVE
#r @"/home/alexgo/Liquid/bin/Liquid1.dll"
#load "Utility.fsx"
#load "GateFunctions.fsx"
#load "QuantumAlgorithm.fsx"
#load "SearchGates.fsx"
#load "SearchAlgorithms.fsx"
#else
namespace Microsoft.Research.Liquid // Tell the compiler our namespace
#endif
open Utility
open GateFunctions
open SearchGates
open QuantumAlgorithm
open SearchAlgorithms

open Microsoft.Research.Liquid      // Get necessary Liquid libraries
open Util                           // General utilites

module SearchAlgorithmsSamples = 
    
    let benchmarkUnstructuredSearchAlgorithmForSingleSolution (solution:int) (searchAlgorithm:QuantumAlgorithm.T) (numberOfIterations:int) = 
        let mutable solutionFound = 0
        let mutable numberOfCorrectSolutions = 0
        for i in 1 .. numberOfIterations do
            solutionFound <- QuantumAlgorithm.prepareAndRunAndGetResult searchAlgorithm
            if (solutionFound = solution) then
                numberOfCorrectSolutions <- numberOfCorrectSolutions + 1
        show "Solution %i was found %ith time(s) running the algorithm %i times" solution numberOfCorrectSolutions numberOfIterations

    let benchmarkUnstructuredSearchAlgorithmForSingleSolutionsInSearchSpace (numberOfQubitsForSearchSpace:int) (numberOfIterationsPerSolution:int) (createUnstructuredSearchAlgorithmForSingleSolution: GateFunctions.DecisionGateFunction -> QuantumAlgorithm.T) =
        let searchSpaceSize = 1 <<< numberOfQubitsForSearchSpace
        show "Benchmark search algorithm for all solution in search space starts with searchSpaceSize=%i" searchSpaceSize
        let mutable decisionGateFunction = SearchGates.UnstructuredSearchGates.createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution 0 numberOfQubitsForSearchSpace
        let mutable searchAlgorithm = createUnstructuredSearchAlgorithmForSingleSolution decisionGateFunction
        for solution in 0 .. searchSpaceSize-1 do
            decisionGateFunction <- SearchGates.UnstructuredSearchGates.createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution solution numberOfQubitsForSearchSpace
            searchAlgorithm <- createUnstructuredSearchAlgorithmForSingleSolution decisionGateFunction
            benchmarkUnstructuredSearchAlgorithmForSingleSolution solution searchAlgorithm numberOfIterationsPerSolution
        show "Benchmark stopped"

    let ``benchmark unstructured search algorithm for single solution in search space up to a max value`` (minNumberOfQubitsForSearchSpace:int) (maxNumberOfQubitsForSearchSpace:int) (numberOfIterationsPerSolution:int) (createUnstructuredSearchAlgorithmForSingleSolution: GateFunctions.DecisionGateFunction -> QuantumAlgorithm.T) =
        for numberOfQubitsForSearchSpace in minNumberOfQubitsForSearchSpace .. maxNumberOfQubitsForSearchSpace do
            benchmarkUnstructuredSearchAlgorithmForSingleSolutionsInSearchSpace numberOfQubitsForSearchSpace numberOfIterationsPerSolution createUnstructuredSearchAlgorithmForSingleSolution
        
    let renderSearchAlgorithm (searchAlgorithm:QuantumAlgorithm.T) detail= 
        QuantumAlgorithm.render searchAlgorithm detail

    let dumpSearchAlgorithm (searchAlgorithm:QuantumAlgorithm.T) = 
        QuantumAlgorithm.dump searchAlgorithm

    let ``render untructured search algorithm for single solution`` (solution:int) (numberOfQubitsForSearchSpace:int) (createUnstructuredSearchAlgorithmForSingleSolution: GateFunctions.DecisionGateFunction -> QuantumAlgorithm.T) detail =
        let decisionGateFunction = SearchGates.UnstructuredSearchGates.createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution solution numberOfQubitsForSearchSpace
        let searchAlgorithm = createUnstructuredSearchAlgorithmForSingleSolution decisionGateFunction
        renderSearchAlgorithm searchAlgorithm detail
        //dumpSearchAlgorithm  searchAlgorithm

    module GroverSearchSamples =
        
        let createGroverSearchForSingleSolution (decisionGateFunction:GateFunctions.DecisionGateFunction) =
            SearchAlgorithms.GroverSearch.create decisionGateFunction 1

        let ``benchmark grover search for each single solution in search space up to a max value`` (minNumberOfQubitsForSearchSpace:int) (maxNumberOfQubitsForSearchSpace:int)  (numberOfIterationsPerSolution:int)=
            ``benchmark unstructured search algorithm for single solution in search space up to a max value`` minNumberOfQubitsForSearchSpace maxNumberOfQubitsForSearchSpace numberOfIterationsPerSolution createGroverSearchForSingleSolution 

        let renderGroverSearchForSingleSolution (solution:int) (numberOfQubitsForSearchSpace:int) detail =
            ``render untructured search algorithm for single solution`` solution numberOfQubitsForSearchSpace createGroverSearchForSingleSolution detail

    let splitSolutionIntoIteratedSearchInput solution numberOfQubitsForSearchSpace numberOfFunctions: int list = 
        if (numberOfQubitsForSearchSpace % numberOfFunctions) <> 0 then invalidArg "numberOfQubitsForSearchSpace" (sprintf "It must be an divisable by numberOfFunctions=%i, but numberOfQubitsForSearchSpace=%i is not" numberOfFunctions numberOfQubitsForSearchSpace)
        let numberOfQubitsForInputChunk = numberOfQubitsForSearchSpace / numberOfFunctions
        let solutionChunks = 
            Utility.intToBoolList solution numberOfQubitsForSearchSpace 
            |> List.rev 
            |> List.chunkBySize numberOfQubitsForInputChunk 
        let solutions = List.map (fun i -> List.take i solutionChunks |> List.concat |> List.rev |> Utility.boolListToInt ) [1 .. numberOfFunctions]
        solutions
    
    let splitNumberOfQubitsForSearchSpaceIntoIteratedSearchInput numberOfQubitsForSearchSpace numberOfFunctions: int list = 
        if (numberOfQubitsForSearchSpace % numberOfFunctions) <> 0 then invalidArg "numberOfQubitsForSearchSpace" (sprintf "It must be an divisable by numberOfFunctions=%i, but numberOfQubitsForSearchSpace=%i is not" numberOfFunctions numberOfQubitsForSearchSpace)
        let numberOfQubitsForInputChunk = numberOfQubitsForSearchSpace / numberOfFunctions
        let numbersOfQubitsForSearchSpace = List.map (fun i -> numberOfQubitsForInputChunk*i) [1 .. numberOfFunctions]
        numbersOfQubitsForSearchSpace 
        
    let benchmarkIteratedSearchAlgorithmForSingleSolutionsInSearchSpace (numberOfQubitsForSearchSpace:int) (numberOfIterationsPerSolution:int) (createIteratedSearchAlgorithmForSingleSolution: GateFunctions.DecisionGateFunction -> GateFunctions.DecisionGateFunction -> QuantumAlgorithm.T) =
        let searchSpaceSize = 1 <<< numberOfQubitsForSearchSpace
        let mutable f_1 = SearchGates.UnstructuredSearchGates.createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution 0 numberOfQubitsForSearchSpace
        let mutable f_2 = SearchGates.UnstructuredSearchGates.createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution 0 numberOfQubitsForSearchSpace
        let mutable searchAlgorithm = createIteratedSearchAlgorithmForSingleSolution f_1 f_2
        let mutable solutions = []
        let mutable numbersOfQubitsForSearchSpace = []
        show "Benchmark search algorithm %s for all solution in search space starts with searchSpaceSize=%i" (searchAlgorithm.Name) searchSpaceSize
        for solution in [0 .. searchSpaceSize-1] do
            solutions <- splitSolutionIntoIteratedSearchInput solution numberOfQubitsForSearchSpace 2
            numbersOfQubitsForSearchSpace <- splitNumberOfQubitsForSearchSpaceIntoIteratedSearchInput numberOfQubitsForSearchSpace 2 
            f_1 <- SearchGates.UnstructuredSearchGates.createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution solutions.[0] numbersOfQubitsForSearchSpace.[0]
            f_2 <- SearchGates.UnstructuredSearchGates.createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution solutions.[1] numbersOfQubitsForSearchSpace.[1]
            searchAlgorithm <- createIteratedSearchAlgorithmForSingleSolution f_1 f_2
            benchmarkUnstructuredSearchAlgorithmForSingleSolution solution searchAlgorithm numberOfIterationsPerSolution
        show "Benchmark stopped"

    let ``benchmark iterated search algorithm for single solution in search space up to a max value`` (minNumberOfQubitsForSearchSpace:int) (maxNumberOfQubitsForSearchSpace:int) (numberOfIterationsPerSolution:int) (createUnstructuredSearchAlgorithmForSingleSolution: GateFunctions.DecisionGateFunction -> GateFunctions.DecisionGateFunction -> QuantumAlgorithm.T) =
        for numberOfQubitsForSearchSpace in minNumberOfQubitsForSearchSpace .. maxNumberOfQubitsForSearchSpace do
            if (numberOfQubitsForSearchSpace % 2 = 0) && (numberOfQubitsForSearchSpace > 0) 
                then benchmarkIteratedSearchAlgorithmForSingleSolutionsInSearchSpace numberOfQubitsForSearchSpace numberOfIterationsPerSolution createUnstructuredSearchAlgorithmForSingleSolution
        
    module OzhigovSearchSamples =
        
        let ``benchmark Ozhigov search`` (minNumberOfQubitsForSearchSpace:int) (maxNumberOfQubitsForSearchSpace:int) (numberOfIterationsPerSolution:int) =
            ``benchmark iterated search algorithm for single solution in search space up to a max value`` minNumberOfQubitsForSearchSpace maxNumberOfQubitsForSearchSpace numberOfIterationsPerSolution SearchAlgorithms.OzhigovSearch.create

        let render (solution:int) (numberOfQubitsForSearchSpace:int) detail =
            let solutions = splitSolutionIntoIteratedSearchInput  solution numberOfQubitsForSearchSpace 2            
            let numbersOfQubitsForSearchSpace = splitNumberOfQubitsForSearchSpaceIntoIteratedSearchInput numberOfQubitsForSearchSpace 2
            let f_1 = SearchGates.UnstructuredSearchGates.createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution (solutions.[0]) (numbersOfQubitsForSearchSpace.[0])
            let f_2 = SearchGates.UnstructuredSearchGates.createDecisionGateFunctionWithUnstructuredSearchWithSingleSolution (solutions.[1]) (numbersOfQubitsForSearchSpace.[1])
            renderSearchAlgorithm (SearchAlgorithms.OzhigovSearch.create f_1 f_2) detail



#if INTERACTIVE
//do SearchAlgorithmsSamples.GroverSearchSamples.``benchmark grover search for each single solution in search space up to a max value`` 1 6 100 
// Change detail level 0 most undetailed, 3 most detailed
//do SearchAlgorithmsSamples.GroverSearchSamples.renderGroverSearchForSingleSolution 1 4 (Some 3)
//do SearchAlgorithmsSamples.OzhigovSearchSamples.render 1 4 (Some 3)
do SearchAlgorithmsSamples.OzhigovSearchSamples.``benchmark Ozhigov search`` 0 6 100
#endif
