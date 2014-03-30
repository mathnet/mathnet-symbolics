namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics


/// General Univariate Rational Expressions
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

    let isRational symbol x =
        (numerator x |> Polynomial.isPolynomial symbol) && (denominator x |> Polynomial.isPolynomial symbol)


module MultivariateRational =

    open System.Collections.Generic
    open Rational

    let variables x =
        let hs = numerator x |> MultivariatePolynomial.variables
        hs.UnionWith(denominator x |> MultivariatePolynomial.variables)
        hs

    let isRationalMV symbols x =
        (numerator x |> MultivariatePolynomial.isPolynomialMV symbols) && (denominator x |> MultivariatePolynomial.isPolynomialMV symbols)
