#if INTERACTIVE
#r @"/home/alexgo/Liquid/bin/Liquid1.dll"
#else
namespace Microsoft.Research.Liquid // Tell the compiler our namespace
#endif
open Microsoft.Research.Liquid      // Get necessary Liquid libraries

module Utility =

    let qubitsToInt(qs:Qubits) = 
        let mutable qubitsAsInt = 0
        for i in 0 .. qs.Length-1 do            
            qubitsAsInt <- qubitsAsInt + ((1 <<< ((qs.Length-1)-i)) * qs.[i].Bit.v)
        qubitsAsInt
    
    // Tail of list is most significant bit
    let intToBoolList (number:int) (bitLength:int) =
        let mutable resultDivisor = number
        let mutable boolList = []
        for i in 1..bitLength do
            boolList <- boolList @ [resultDivisor % 2 = 1]
            resultDivisor <- resultDivisor / 2
        boolList
 
    // Tail of list is most significant bit
    let boolListToInt (boolList:bool list) =
        let rec recF bitList i n =
            match bitList with
                | [] -> n
                | x::xs ->  if x then recF xs (i+1) (n+ (1 <<< i)) else recF xs (i+1) n
        recF boolList 0 0

    //
    // might be important later, but is not used for now
    //
    
    // log_a b
    let log a b =
        System.Math.Log b / System.Math.Log a


    let doesContainNumberWithCondition aList condition =
        let rec helperFunction aList =
            match aList with
                | [] -> false
                | x::xs -> if condition x then true else helperFunction xs 
        helperFunction aList

    module PositivePowerOfTwo =

        type T = private PositivePowerOfTwo of int

        let create number =
            let isNumberAPositivePowerOfTwo number =
                if number <= 0. then false
                else 
                    let mutable tempNumber = number; 
                    while (number % 2. = 0.) do
                        tempNumber <- tempNumber / 2.
                    if tempNumber = 1. then true
                    else false 

            if isNumberAPositivePowerOfTwo <| float number then PositivePowerOfTwo number
            else invalidArg "number" (sprintf "The number %i is not a power of two." number)

        let createFromExponent exponent =
            create <| (1 <<< exponent)

        let value (PositivePowerOfTwo s) = s

        let exponent (positivePowerOfTwo:T) =
            value positivePowerOfTwo |> float |> log 2. |> ceil |> int

#if INTERACTIVE
//open Util
//show "The solution=%i" <| Utility.boolListToInt [true;true;false;true]
#endif
