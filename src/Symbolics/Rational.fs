namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

/// General Rational Expressions
module Rational =

    open Numbers
    open Elementary
    open ExpressionPatterns

    let rec numerator = function
        | Number n when not n.IsInteger -> Expression.OfInteger n.Numerator
        | NegRationalPower _ -> one
        | Product ax -> product <| List.map numerator ax
        | z -> z

    let rec denominator = function
        | Number n when not n.IsInteger -> Expression.OfInteger n.Denominator
        | NegRationalPower (r, p) -> r ** -p
        | Product ax -> product <| List.map denominator ax
        | _ -> one

    let variables x =
        let hs = numerator x |> Polynomial.variables
        hs.UnionWith(denominator x |> Polynomial.variables)
        hs

    let isRational symbol x =
        (numerator x |> Polynomial.isPolynomial symbol) && (denominator x |> Polynomial.isPolynomial symbol)

    let isRationalMV symbols x =
        (numerator x |> Polynomial.isPolynomialMV symbols) && (denominator x |> Polynomial.isPolynomialMV symbols)

    let rec private rationalizeSum d x y =
        let a = denominator x
        let b = denominator y
        if a = one && b = one then (x+y)/d
        else rationalizeSum (a*b*d) ((numerator x)*b) ((numerator y)*a)

    let rec rationalize = function
        | Power (r, p) -> (rationalize r)**p
        | Product ax -> product <| List.map rationalize ax
        | Sum ax -> List.map rationalize ax |> List.reduce (rationalizeSum one)
        | x -> x

    let rec rationalExpand x =
        let n = numerator x |> algebraicExpand
        let d = denominator x |> algebraicExpand
        let z = rationalize (n/d)
        if x = z then z else rationalExpand z

    let rationalSimplify symbol x =
        let z = rationalExpand x
        let n = numerator z
        let d = denominator z
        let g = Polynomial.gcd symbol n d
        (Polynomial.quot symbol n g)/(Polynomial.quot symbol d g)
