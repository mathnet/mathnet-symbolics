namespace MathNet.Symbolics

open System.Collections.Generic
open MathNet.Numerics

open Operators

[<RequireQualifiedAccess>]
module Approximate =

    [<CompiledName("Real")>]
    let real x = Approximation.fromReal x

    [<CompiledName("Complex")>]
    let complex r i = Approximation.fromComplex (complex r i)

    [<CompiledName("ApproximateSubstitute")>]
    let rec approximateSubstitute (symbols:IDictionary<string, Approximation>) x =
        match x with
        | Number n -> Values.real (float n)
        | Approximation _ -> x
        | Constant c ->
            match c with
            | Constant.E -> Values.real Constants.E
            | Constant.Pi -> Values.real Constants.Pi
            | Constant.I -> Values.complex Complex.onei
        | Sum sx -> sum (List.map (approximateSubstitute symbols) sx)
        | Product px -> product (List.map (approximateSubstitute symbols) px)
        | Power (a,b) -> pow (approximateSubstitute symbols a) (approximateSubstitute symbols b)
        | Function (f,a) ->
            match approximateSubstitute symbols a with
            | Values.Value v -> Values.apply f v
            | x -> apply f x
        | Identifier (Symbol s) ->
            match symbols.TryGetValue s with
            | true, a -> a |> Value.approx |> Values.unpack
            | _ -> x
        | Argument _ -> x
        | FunctionN _
        | ComplexInfinity
        | PositiveInfinity
        | NegativeInfinity
        | Undefined -> x

    [<CompiledName("Approximate")>]
    let rec approximate x = approximateSubstitute Map.empty x
