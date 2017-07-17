namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics
open System.Linq.Expressions

[<StructuralEquality;NoComparison;RequireQualifiedAccess>]
type VisualExpression =
    | Identifier of Symbol
    | Constant of Constant
    | PositiveInteger of BigInteger
    | PositiveFloatingPoint of double
    | Parenthesis of VisualExpression
    | Prodct of VisualExpression list
    | Fraction of VisualExpression * VisualExpression
    | Negative of VisualExpression
    | Sum of VisualExpression list
    | Power of VisualExpression * VisualExpression
    | Function of string * VisualExpression
    | FunctionN of string * (VisualExpression list)
    | ComplexInfinity
    | Infinity
    | Undefined

module VisualExpression =

    let fromExpression expression =
        let parenthesis priority threshold ve = if priority > threshold then VisualExpression.Parenthesis ve else ve
        let rec convert priority = function
            | Number n when n.IsNegative && n.IsInteger ->
                VisualExpression.Negative (VisualExpression.PositiveInteger (-n.Numerator))
                |> parenthesis priority 0
            | Number n when n.IsNegative ->
                VisualExpression.Negative (VisualExpression.Fraction (VisualExpression.PositiveInteger (-n.Numerator), VisualExpression.PositiveInteger n.Denominator))
                |> parenthesis priority 1
            | Number n when n.IsInteger ->
                VisualExpression.PositiveInteger n.Numerator
            | Number n ->
                VisualExpression.Fraction (VisualExpression.PositiveInteger n.Numerator, VisualExpression.PositiveInteger n.Denominator)
                |> parenthesis priority 1
            | Approximation (Approximation.Real fp) when fp < 0.0 ->
                VisualExpression.Negative (VisualExpression.PositiveFloatingPoint (-fp))
                |> parenthesis priority 0
            | Approximation (Approximation.Real fp) -> VisualExpression.PositiveFloatingPoint fp
            | Approximation (Approximation.Complex fp) when fp.IsRealNonNegative() -> VisualExpression.PositiveFloatingPoint fp.Real
            | Approximation (Approximation.Complex fp) when fp.IsReal() ->
                VisualExpression.Negative (VisualExpression.PositiveFloatingPoint (-fp.Real))
                |> parenthesis priority 0
            | Approximation (Approximation.Complex fp) -> //when fp.Real = 0.0 && fp.Imaginary < 0.0 ->
                failwith "TODO"
            | Sum (x::xs) ->

            | Identifier s -> VisualExpression.Identifier s
            | Constant c -> VisualExpression.Constant c
            | ComplexInfinity -> VisualExpression.ComplexInfinity
            | PositiveInfinity -> VisualExpression.Infinity
            | NegativeInfinity ->
                VisualExpression.Negative VisualExpression.Infinity
                |> parenthesis priority 0
            | Undefined -> VisualExpression.Undefined

        convert 1 expression

