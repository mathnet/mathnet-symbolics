namespace MathNet.Symbolics

open MathNet.Numerics
open MathNet.Symbolics

open ExpressionPatterns
open Operators

[<RequireQualifiedAccess>]
module Exponential =

    /// Expand exponential and logarithmic terms
    [<CompiledName("Expand")>]
    let rec expand x =
        let rec expRules = function
            | Sum ax -> product <| List.map expRules ax
            | Product ((Integer _ as n)::ax) -> (expRules (product ax))**n
            | x -> exp x
        let rec lnRules f = function
            | Product ax -> sum <| List.map (lnRules f) ax
            | Power (r, p) -> p*lnRules f r |> Algebraic.expand
            | x -> f x
        let rec lognRules basis = function
            | Product ax -> sum <| List.map (lognRules basis) ax
            | Power (r, p) -> p*lognRules basis r |> Algebraic.expand
            | x -> log basis x
        match Structure.map expand x with
        | Function (Exp, a) -> expRules (Algebraic.expand a)
        | Function (Ln, a) -> lnRules ln (Algebraic.expand a)
        | Function (Lg, a) -> lnRules lg (Algebraic.expand a)
        | FunctionN (Log, [basis;a]) -> lognRules basis (Algebraic.expand a)
        | a -> a

    [<CompiledName("Contract")>]
    let rec contract x =
        let rec rules x =
            match Algebraic.expandMain x with
            | Power (Function (Exp, a), s) ->
                match a*s with
                | Product _ | Power _ as p -> exp (rules p)
                | p -> exp p
            | Product ax ->
                let f (p,s) = function | Function (Exp, a) -> (p,s+a) | a -> (p*a,s)
                let p, s = List.fold f (one, zero) ax in p * exp s
            | Sum ax ->
                let f s = function | Product _ | Power _ as a -> s+(rules a) | a -> s+a
                List.fold f zero ax
            | a -> a
        match Structure.map contract x with
        | Product _ | Power _ as a -> rules a
        | a -> a

    [<CompiledName("Simplify")>]
    let simplify x =
        let x' = Rational.rationalize x
        (Rational.numerator x' |> contract) / (Rational.denominator x' |> contract)
