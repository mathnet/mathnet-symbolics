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
        let hs = numerator x |> MultivariatePolynomial.variables
        hs.UnionWith(denominator x |> MultivariatePolynomial.variables)
        hs

    let isRational symbol x =
        (numerator x |> Polynomial.isPolynomial symbol) && (denominator x |> Polynomial.isPolynomial symbol)

    let isRationalMV symbols x =
        (numerator x |> MultivariatePolynomial.isPolynomialMV symbols) && (denominator x |> MultivariatePolynomial.isPolynomialMV symbols)

    let rec rationalizeSum x y =
        let a = denominator x
        let b = denominator y
        if a = one && b = one then x+y
        else (rationalizeSum ((numerator x)*b) ((numerator y)*a))/(a*b)

    let rec rationalize = function
        | Power (r, p) -> (rationalize r)**p
        | Product ax -> product <| List.map rationalize ax
        | Sum ax -> List.map rationalize ax |> List.reduce rationalizeSum
        | x -> x

    let rec rationalExpand x =
        let n = numerator x |> algebraicExpand
        let d = denominator x |> algebraicExpand
        let z = rationalize (n/d)
        if x = z then z else rationalExpand z
