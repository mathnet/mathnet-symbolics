namespace MathNet.Symbolics

open MathNet.Symbolics

open ExpressionPatterns
open Operators


/// General Rational Expressions
module Rational =

    [<CompiledName("Numerator")>]
    let rec numerator = function
        | Number n when not n.IsInteger -> Expression.FromInteger n.Numerator
        | NegRationalPower _ -> one
        | Product ax -> product <| List.map numerator ax
        | z -> z

    [<CompiledName("Denominator")>]
    let rec denominator = function
        | Number n when not n.IsInteger -> Expression.FromInteger n.Denominator
        | NegRationalPower (r, p) -> r ** -p
        | Product ax -> product <| List.map denominator ax
        | _ -> one

    [<CompiledName("Variables")>]
    let variables x =
        let hs = numerator x |> Polynomial.variables
        hs.UnionWith(denominator x |> Polynomial.variables)
        hs

    [<CompiledName("IsRational")>]
    let isRational symbol x =
        (numerator x |> Polynomial.isPolynomial symbol) && (denominator x |> Polynomial.isPolynomial symbol)

    [<CompiledName("IsMultivariateRational")>]
    let isRationalMV symbols x =
        (numerator x |> Polynomial.isPolynomialMV symbols) && (denominator x |> Polynomial.isPolynomialMV symbols)

    [<CompiledName("Reduce")>]
    let reduce x =
        let n = numerator x
        let d = denominator x
        let ncf = Polynomial.commonFactors n
        let dcf = Polynomial.commonFactors d
        let cf = Polynomial.commonMonomialFactors [ncf; dcf]
        if isOne cf then x else (Algebraic.expand (n/cf))/(Algebraic.expand (d/cf))

    let rec private rationalizeSum d x y =
        let a = denominator x
        let b = denominator y
        if isOne a && isOne b then (x+y)/d
        else rationalizeSum (a*b*d) ((numerator x)*b) ((numerator y)*a)

    [<CompiledName("Rationalize")>]
    let rec rationalize = function
        | Power (r, p) -> (rationalize r)**p
        | Product ax -> product <| List.map rationalize ax
        | Sum ax -> List.map rationalize ax |> List.reduce (rationalizeSum one)
        | x -> x

    [<CompiledName("Expand")>]
    let expand x =
        let rec expandRationalize x =
            let n = numerator x |> Algebraic.expand
            let d = denominator x |> Algebraic.expand
            let z = rationalize (n/d)
            if x = z then z else expandRationalize z
        let expanded = expandRationalize x |> reduce
        match denominator expanded with
        | Number _ -> Algebraic.expand expanded
        | _ -> expanded

    [<CompiledName("Simplify")>]
    let simplify symbol x =
        let z = expand x
        let n = numerator z
        let d = denominator z
        let g = Polynomial.gcd symbol n d
        (Polynomial.quot symbol n g)/(Polynomial.quot symbol d g)
