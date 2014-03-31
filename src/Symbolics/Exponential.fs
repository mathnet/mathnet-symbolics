namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics


[<RequireQualifiedAccess>]
module Exponential =

    open ExpressionPatterns
    open Elementary

    /// Expand exponential and logarithmic terms
    let rec expand x =
        let rec expRules = function
            | Sum ax -> product <| List.map expRules ax
            | Product ((Integer _ as n)::ax) -> (expRules (product ax))**n
            | x -> Functions.exp x
        let rec lnRules = function
            | Product ax -> sum <| List.map lnRules ax
            | Power (r, p) -> p*lnRules r |> algebraicExpand
            | x -> Functions.ln x
        let x' = map expand x
        match x' with
        | Function (Exp, a) -> expRules (algebraicExpand a)
        | Function (Ln, a) -> lnRules (algebraicExpand a)
        | a -> a


[<RequireQualifiedAccess>]
module Trigonometric =

    open ExpressionPatterns
    open Elementary

    let rec expand x =
        let rec rules = function
            | Sum ax ->
                List.map rules ax
                |> List.reduce (fun (s1,c1) (s2,c2) -> (s1*c2 + c1*s2, c1*c2 - s1*s2))
            | Product ((Integer n)::ax) when n.IsPositive ->
                let e = int n
                let t = product ax
                let sint = Functions.sin t
                let cost = Functions.cos t
                let esin =
                    [for k in 1 .. 2 .. e -> (k, (if Euclid.IsEven((k-1)/2) then 1 else -1) * int(SpecialFunctions.Binomial(e, k)))]
                    |> List.map (fun (k,c) -> c*cost**number(e-k)*sint**k) |> sum
                let ecos =
                    [for k in 0 .. 2 .. e -> (k, (if Euclid.IsEven(k/2) then 1 else -1) * int(SpecialFunctions.Binomial(e, k)))]
                    |> List.map (fun (k,c) -> c*cost**number(e-k)*sint**k) |> sum
                (esin, ecos)
            | x -> Functions.sin x, Functions.cos x
        let x' = map expand x
        match x' with
        | Function (Sin, a) -> rules (algebraicExpand a) |> fst
        | Function (Cos, a) -> rules (algebraicExpand a) |> snd
        | a -> a
