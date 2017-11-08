﻿namespace MathNet.Symbolics

open MathNet.Numerics
open MathNet.Symbolics

open ExpressionPatterns
open Operators

[<RequireQualifiedAccess>]
module Trigonometric =

    let private binomial n k = SpecialFunctions.Binomial(n, k) |> int |> number
    let private oneIfEven (k:int) = if Euclid.IsEven(k) then 1 else -1

    [<CompiledName("Expand")>]
    let rec expand x =
        let rec rules = function
            | Sum ax ->
                List.map rules ax
                |> List.reduce (fun (s1,c1) (s2,c2) -> (s1*c2 + c1*s2, c1*c2 - s1*s2))
            | Product ((Integer n)::ax) when n.IsPositive ->
                let e = int n
                let t = product ax
                let sint = sin t
                let cost = cos t
                let esin =
                    [for k in 1 .. 2 .. e -> (k, oneIfEven((k-1)/2) * binomial e k)]
                    |> List.map (fun (k,c) -> c*cost**number(e-k)*sint**k) |> sum
                let ecos =
                    [for k in 0 .. 2 .. e -> (k, oneIfEven(k/2) * binomial e k)]
                    |> List.map (fun (k,c) -> c*cost**number(e-k)*sint**k) |> sum
                (esin, ecos)
            | x -> sin x, cos x
        let rec rules' = function
            | Sum ax ->
                List.map rules' ax
                |> List.reduce (fun (s1,c1) (s2,c2) -> (s1*c2 + c1*s2, c1*c2 + s1*s2))
            | Product ((Integer n)::ax) when n.IsPositive ->
                let e = int n
                let t = product ax
                let sinht = sinh t
                let cosht = cosh t
                let esinh =
                    [for k in 1 .. 2 .. e -> (k, binomial e k)] // 0 for even k, binomial(e,k) for odd k
                    |> List.map (fun (k,c) -> c*cosht**number(e-k)*sinht**k) |> sum
                let ecosh =
                    [for k in 0 .. 2 .. e -> (k, binomial e k)] // binomial(e,k) for even k, 0 for odd k
                    |> List.map (fun (k,c) -> c*cosht**number(e-k)*sinht**k) |> sum
                (esinh, ecosh)
            | x -> sinh x, cosh x    
        match Structure.map expand x with
        | Function (Sin, a) -> rules (Algebraic.expand a) |> fst
        | Function (Cos, a) -> rules (Algebraic.expand a) |> snd
        | Function (Sinh, a) -> rules' (Algebraic.expand a) |> fst
        | Function (Cosh, a) -> rules' (Algebraic.expand a) |> snd
        | a -> a

    /// Splits a product into a tuple (rest, sin or cos or a positive integer power of them)
    [<CompiledName("SeparateFactors")>]
    let separateFactors x =
        let rec isSinCosPart = function
            | PosIntPower (r, _) -> isSinCosPart r
            | SinCos _ -> true
            | _ -> false
        match x with
        | Product ax -> let s, r = List.partition isSinCosPart ax in (product r, product s)
        | x when isSinCosPart x -> (one, x)
        | x -> (x, one)

    [<CompiledName("Contract")>]
    let rec contract x =
        let powerRules r p =
            match r, p with
            | Function (Sin, x), (Number n as p) when n.IsInteger && n.IsPositive ->
                let e = int n
                if Euclid.IsEven(e) then
                    let w =  oneIfEven(e/2) * (2Q**(1-e))
                    let z = sum [for j in 0 .. (e/2-1) -> oneIfEven j * w * (binomial e j) * cos((e-2*j)*x)]
                    oneIfEven e * (binomial e (e/2))/(2Q**e) + z
                else
                    let w = oneIfEven((e-1)/2) * (2Q**(1-e))
                    sum [for j in 0 .. (e/2) -> oneIfEven j * w * (binomial e j) * sin((e-2*j)*x)]
            | Function (Cos, x), (Number n as p) when n.IsInteger && n.IsPositive ->
                let e = int n
                if Euclid.IsEven(e) then
                    let w = (2Q**(1-e))
                    let z = sum [for j in 0 .. (e/2-1) -> w * (binomial e j) * cos((e-2*j)*x)]
                    (binomial e (e/2))/(2Q**e) + z
                else
                    let w = (2Q**(1-e))
                    sum [for j in 0 .. (e/2) -> w * (binomial e j) * cos((e-2*j)*x)]
            | Function (Sinh, x), (Number n as p) when n.IsInteger && n.IsPositive ->
                let e = int n
                if Euclid.IsEven(e) then
                    let w =  oneIfEven(e/2) * (2Q**(1-e))
                    let z = sum [for j in 0 .. (e/2-1) -> oneIfEven j * w * (binomial e j) * cosh((e-2*j)*x)]
                    oneIfEven(e/2) * (oneIfEven e) * (binomial e (e/2))/(2Q**e) + oneIfEven(e/2) * z
                else
                    let w = oneIfEven((e-1)/2) * (2Q**(1-e))
                    sum [for j in 0 .. (e/2) -> oneIfEven((e-1)/2) * oneIfEven j * w * (binomial e j) * sinh((e-2*j)*x)]
            | Function (Cosh, x), (Number n as p) when n.IsInteger && n.IsPositive ->
                let e = int n
                if Euclid.IsEven(e) then
                    let w = (2Q**(1-e))
                    let z = sum [for j in 0 .. (e/2-1) -> w * (binomial e j) * cosh((e-2*j)*x)]
                    (binomial e (e/2))/(2Q**e) + z
                else
                    let w = (2Q**(1-e))
                    sum [for j in 0 .. (e/2) -> w * (binomial e j) * cosh((e-2*j)*x)]
            | _ -> r**p
        let rec productRules = function
            | [u; v] ->
                match u, v with
                | Power (r, p), b | b, Power (r, p) -> rules (b * powerRules r p)
                | Function (Sin, a), Function (Sin, b) -> cos(a-b)/2 - cos(a+b)/2
                | Function (Cos, a), Function (Cos, b) -> cos(a+b)/2 + cos(a-b)/2
                | Function (Sin, a), Function (Cos, b) -> sin(a+b)/2 + sin(a-b)/2
                | Function (Cos, a), Function (Sin, b) -> sin(a+b)/2 - sin(a-b)/2
                | Function (Sinh, a), Function (Sinh, b) -> cosh(a+b)/2 - cosh(a-b)/2
                | Function (Cosh, a), Function (Cosh, b) -> cosh(a+b)/2 + cosh(a-b)/2
                | Function (Sinh, a), Function (Cosh, b) -> sinh(a-b)/2 + sinh(a+b)/2
                | Function (Cosh, a), Function (Sinh, b) -> sinh(a+b)/2 - sinh(a-b)/2
                | _ -> failwith "unexpected expression"
            | x::xs -> rules (x * productRules xs)
            | _ -> failwith "algorithm error 2"
        and rules x =
            match Algebraic.expandMain x with
            | SinCosPosIntPower (r, p) -> powerRules r p
            | Product _ as a ->
                let c, d = separateFactors a
                match d with
                | One -> a
                | SinCos _ -> a
                | Power (r, p) -> c * powerRules r p |> Algebraic.expandMain
                | Product ax -> c * productRules ax |> Algebraic.expandMain
                | v -> c*d
            | Sum ax ->
                let f s = function | Product _ | Power _ as a -> s+(rules a) | a -> s+a
                List.fold f zero ax
            | a -> a
        match Structure.map contract x with
        | Product _ | Power _ as a -> rules a
        | a -> a

    // Substitute Tan, Cot, Sec, Csc to sin and cos
    [<CompiledName("Substitute")>]
    let rec substitute x =
        match x with
        | Function (Tan, a) -> let a' = substitute a in sin(a')/cos(a')
        | Function (Cot, a) -> let a' = substitute a in cos(a')/sin(a')
        | Function (Sec, a) -> let a' = substitute a in one/cos(a')
        | Function (Csc, a) -> let a' = substitute a in one/sin(a')
        | Function (Tanh, a) -> let a' = substitute a in sinh(a')/cosh(a')
        | Function (Coth, a) -> let a' = substitute a in cosh(a')/sinh(a')
        | Function (Sech, a) -> let a' = substitute a in one/cosh(a')
        | Function (Csch, a) -> let a' = substitute a in one/sinh(a')
        | Sum ax -> sum <| List.map substitute ax
        | Product ax -> product <| List.map substitute ax
        | Power (radix, p) -> (substitute radix) ** (substitute p)
        | Function (fn, x) -> apply fn (substitute x)
        | FunctionN (fn, xs) -> applyN fn (List.map substitute xs)
        | x -> x

    [<CompiledName("Simplify")>]
    let simplify x =
        let x' = substitute x
        let w = Rational.rationalize x'
        (Rational.numerator w |> expand |> contract) / (Rational.denominator w |> expand |> contract)
