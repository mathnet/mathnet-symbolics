namespace MathNet.Symbolics

open MathNet.Numerics
open MathNet.Symbolics

/// Enriched expression tree which includes an enrichment element on every node.
/// Not intended for algebraic manipulations, but can be converted from and to Expression.
[<StructuralEquality;NoComparison;RequireQualifiedAccess>]
type EnrichedExpression<'e> =
    | Number of 'e * BigRational
    | Approximation of 'e * Approximation
    | Identifier of 'e * Symbol
    | Argument of 'e * Symbol
    | Constant of 'e * Constant
    | Sum of 'e * (EnrichedExpression<'e> list)
    | Product of 'e * (EnrichedExpression<'e> list)
    | Power of 'e * (EnrichedExpression<'e> * EnrichedExpression<'e>)
    | Function of 'e * (Function * EnrichedExpression<'e>)
    | FunctionN of 'e * (FunctionN * (EnrichedExpression<'e> list))
    | ComplexInfinity of 'e
    | PositiveInfinity of 'e
    | NegativeInfinity of 'e
    | Undefined

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EnrichedExpression =
    open Rational
    open ExpressionPatterns

    let enrichment (enrichedExpression: EnrichedExpression<'e>) : 'e option =
        match enrichedExpression with
        | EnrichedExpression.Number (e, _) -> Some e
        | EnrichedExpression.Approximation (e, _) -> Some e
        | EnrichedExpression.Identifier (e, _) -> Some e
        | EnrichedExpression.Argument (e, _) -> Some e
        | EnrichedExpression.Constant (e, _) -> Some e
        | EnrichedExpression.Sum (e, _) -> Some e
        | EnrichedExpression.Product (e, _) -> Some e
        | EnrichedExpression.Power (e, _) -> Some e
        | EnrichedExpression.Function (e, _) -> Some e
        | EnrichedExpression.FunctionN (e, _) -> Some e
        | EnrichedExpression.ComplexInfinity e -> Some e
        | EnrichedExpression.PositiveInfinity e -> Some e
        | EnrichedExpression.NegativeInfinity e -> Some e
        | EnrichedExpression.Undefined -> None

    let toExpression (enrichedExpression: EnrichedExpression<'e>) : Expression =
        let rec convert = function
            | EnrichedExpression.Number (_, x) -> Expression.Number x
            | EnrichedExpression.Approximation (_, x) -> Expression.Approximation x
            | EnrichedExpression.Identifier (_, x) -> Expression.Identifier x
            | EnrichedExpression.Argument (_, x) -> Expression.Argument x
            | EnrichedExpression.Constant (_, x) -> Expression.Constant x
            | EnrichedExpression.Sum (_, xs) -> Expression.Sum (xs |> List.map convert)
            | EnrichedExpression.Product (_, xs) -> Expression.Product (xs |> List.map convert)
            | EnrichedExpression.Power (_, (a, b)) -> Expression.Power (convert a, convert b)
            | EnrichedExpression.Function (_, (f, x)) -> Expression.Function (f, convert x)
            | EnrichedExpression.FunctionN (_, (f, xs)) -> Expression.FunctionN (f, xs |> List.map convert)
            | EnrichedExpression.ComplexInfinity _ -> Expression.ComplexInfinity
            | EnrichedExpression.PositiveInfinity _ -> Expression.PositiveInfinity
            | EnrichedExpression.NegativeInfinity _ -> Expression.NegativeInfinity
            | EnrichedExpression.Undefined -> Expression.Undefined
        convert enrichedExpression
