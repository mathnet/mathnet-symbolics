namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

module Expand =

    open ExpressionPatterns

    let rec expandProduct x y =
        match x, y with
        | Sum ax, b | b, Sum ax -> sum <| List.map (expandProduct b) ax
        | a, b -> a*b

    let rec expandPower x (y:int) =
        match x, y with
        | Sum [a], b -> a**(number b)
        | Sum (a::ax), n when n > 1 ->
            let e = int n
            [for k in 0 .. e -> (k, int <| SpecialFunctions.Binomial(e, k))]
            |> List.map (fun (k,c) -> expandProduct (c * a**number(e-k)) (expandPower (Sum ax) k))
            |> sum
        | a, b -> a**(number b)

    let rec algebraicExpand = function
        | Number _ | Identifier _ as x -> x
        | Sum ax -> sum <| List.map algebraicExpand ax
        | Product ax -> List.map algebraicExpand ax |> List.reduce expandProduct
        | PosIntPower (r, Number (Integer n)) -> expandPower (algebraicExpand r) (int n)
        | x -> x
