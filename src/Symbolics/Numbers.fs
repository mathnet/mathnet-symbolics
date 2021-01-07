namespace MathNet.Symbolics

open MathNet.Numerics
open MathNet.Symbolics

[<RequireQualifiedAccess>]
module Numbers =

    /// Represent the constant as a real number if possible
    let (|RealConstant|_|) = function
        | Approximation (Real r) -> Some r
        | Constant E -> Some Constants.E
        | Constant Pi -> Some Constants.Pi
        | ComplexInfinity -> Some System.Double.PositiveInfinity
        | PositiveInfinity -> Some System.Double.PositiveInfinity
        | NegativeInfinity -> Some System.Double.NegativeInfinity
        | _ -> None

    [<CompiledName("Compare")>]
    let compare x y =
        match x, y with
        | a, b when a = b -> 0
        | Number _, ComplexInfinity -> -1
        | Number _, PositiveInfinity -> -1
        | Number _, NegativeInfinity -> 1
        | ComplexInfinity, Number _ -> 1
        | PositiveInfinity, Number _ -> 1
        | NegativeInfinity, Number _ -> -1
        | Number a, Number b -> compare a b
        | Number a, RealConstant b -> compare (float a) b
        | RealConstant a, Number b -> compare a (float b)
        | RealConstant a, RealConstant b -> compare a b
        | _ -> failwith "only numbers and +/-infinity are supported"

    [<CompiledName("Max2")>]
    let max2 u v = if compare u v >= 0 then u else v

    [<CompiledName("Min2")>]
    let min2 u v = if compare u v <= 0 then u else v

    [<CompiledName("Max")>]
    let max ax = List.reduce max2 ax

    [<CompiledName("Min")>]
    let min ax = List.reduce min2 ax

    [<CompiledName("GreatestCommonDivisor2")>]
    let gcd2 u v =
        match u, v with
        | Number a, Number b when a.IsInteger && b.IsInteger ->
            Euclid.GreatestCommonDivisor(a.Numerator, b.Numerator) |> Expression.Integer
        | _ -> Undefined

    [<CompiledName("LeastCommonMultiple2")>]
    let lcm2 u v =
        match u, v with
        | Number a, Number b when a.IsInteger && b.IsInteger ->
            Euclid.LeastCommonMultiple(a.Numerator, b.Numerator) |> Expression.Integer
        | _ -> Undefined

    [<CompiledName("GreatestCommonDivisor")>]
    let gcd ax = List.reduce gcd2 ax

    [<CompiledName("LeastCommonMultiple")>]
    let lcm ax = List.reduce lcm2 ax
