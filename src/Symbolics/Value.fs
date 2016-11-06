namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics

[<RequireQualifiedAccess>]
type Value =
    | Number of BigRational
    | Approximation of Approximation
    | ComplexInfinity
    | PositiveInfinity
    | NegativeInfinity
    | Undefined

[<RequireQualifiedAccess>]
module ValueOperations =

    let real (x:float) =
        if Double.IsPositiveInfinity x then Value.PositiveInfinity
        elif Double.IsNegativeInfinity x then Value.NegativeInfinity
        elif Double.IsNaN x then Value.Undefined
        else Value.Approximation (Approximation.Real x)

    let complex (x:Complex) =
        if x.IsReal() then real x.Real
        elif x.IsInfinity() then Value.ComplexInfinity
        elif x.IsNaN() then Value.Undefined
        else Value.Approximation (Approximation.Complex x)

    let approx = function
        | Real d -> real d
        | Complex c -> complex c

    let zero = Value.Number (BigRational.Zero)
    let one = Value.Number (BigRational.One)

    let (|Zero|_|) = function
        | Value.Number n when n.IsZero -> Some Zero
        | Value.Approximation a when ApproxOperations.isZero a -> Some Zero
        | _ -> None

    let (|One|_|) = function
        | Value.Number n when n.IsOne -> Some One
        | Value.Approximation a when ApproxOperations.isOne a -> Some One
        | _ -> None

    let (|MinusOne|_|) = function
        | Value.Number n when n.IsInteger && n.Numerator = BigInteger.MinusOne -> Some MinusOne
        | Value.Approximation a when ApproxOperations.isMinusOne a -> Some MinusOne
        | _ -> None

    let (|Positive|_|) = function
        | Value.Number n when n.IsPositive -> Some Positive
        | Value.Approximation x when ApproxOperations.isPositive x -> Some Positive
        | Value.PositiveInfinity -> Some Positive
        | _ -> None

    let (|Negative|_|) = function
        | Value.Number n when n.IsNegative -> Some Negative
        | Value.Approximation x when ApproxOperations.isNegative x -> Some Negative
        | Value.NegativeInfinity -> Some Negative
        | _ -> None

    let isZero = function | Zero -> true | _ -> false
    let isOne = function | One -> true | _ -> false
    let isMinusOne = function | MinusOne -> true | _ -> false
    let isPositive = function | Positive -> true | _ -> false
    let isNegative = function | Negative -> true | _ -> false

    let negate = function
        | Value.Number a -> Value.Number (-a)
        | Value.Approximation a -> ApproxOperations.negate a |> approx
        | Value.NegativeInfinity -> Value.PositiveInfinity
        | Value.PositiveInfinity -> Value.NegativeInfinity
        | Value.ComplexInfinity -> Value.ComplexInfinity
        | Value.Undefined -> Value.Undefined

    let abs = function
        | Value.Number a when a.IsNegative -> Value.Number (-a)
        | Value.Number _ as x -> x
        | Value.Approximation a -> ApproxOperations.abs a |> approx
        | Value.NegativeInfinity | Value.PositiveInfinity | Value.ComplexInfinity -> Value.PositiveInfinity
        | Value.Undefined -> Value.Undefined

    let sum = function
        | Value.Undefined, _ | _, Value.Undefined -> Value.Undefined
        | Zero, b | b, Zero -> b
        | Value.Number a, Value.Number b -> Value.Number (a + b)
        | Value.Approximation a, Value.Approximation b -> ApproxOperations.sum (a, b) |> approx
        | Value.Number a, Value.Approximation b | Value.Approximation b, Value.Number a -> ApproxOperations.sum (ApproxOperations.fromRational a, b) |> approx
        | Value.ComplexInfinity, (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity) -> Value.Undefined
        | (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity),  Value.ComplexInfinity -> Value.Undefined
        | Value.ComplexInfinity, _ | _, Value.ComplexInfinity -> Value.ComplexInfinity
        | Value.PositiveInfinity, Value.NegativeInfinity | Value.NegativeInfinity, Value.PositiveInfinity -> Value.Undefined
        | Value.PositiveInfinity, _ | _, Value.PositiveInfinity -> Value.PositiveInfinity
        | Value.NegativeInfinity, _ | _, Value.NegativeInfinity -> Value.NegativeInfinity

    let product = function
        | Value.Undefined, _ | _, Value.Undefined -> Value.Undefined
        | One, b | b, One -> b
        | Zero, _ | _, Zero -> zero
        | Value.Number a, Value.Number b -> Value.Number (a * b)
        | Value.Approximation a, Value.Approximation b -> ApproxOperations.product (a, b) |> approx
        | Value.Number a, Value.Approximation b | Value.Approximation b, Value.Number a -> ApproxOperations.product (ApproxOperations.fromRational a, b) |> approx
        | Value.ComplexInfinity, _ | _, Value.ComplexInfinity -> Value.ComplexInfinity
        | Value.PositiveInfinity, Positive | Positive, Value.PositiveInfinity -> Value.PositiveInfinity
        | Value.PositiveInfinity, Negative | Negative, Value.PositiveInfinity -> Value.NegativeInfinity
        | Value.NegativeInfinity, Positive | Positive, Value.NegativeInfinity -> Value.NegativeInfinity
        | Value.NegativeInfinity, Negative | Negative, Value.NegativeInfinity -> Value.PositiveInfinity
        | Value.NegativeInfinity, _ | _, Value.NegativeInfinity -> Value.NegativeInfinity
        | Value.PositiveInfinity, _ | _, Value.PositiveInfinity -> Value.PositiveInfinity

    let invert = function
        | Zero -> Value.ComplexInfinity
        | Value.Number a -> Value.Number (BigRational.Reciprocal a)
        | Value.Approximation a -> ApproxOperations.invert a |> approx
        | Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity -> zero
        | Value.Undefined -> Value.Undefined

    let power = function
        | Value.Undefined, _ | _, Value.Undefined -> Value.Undefined
        | Zero, Zero -> Value.Undefined
        | _, Zero -> one
        | a, One -> a
        | One, _ -> one
        | Value.Number a, Value.Number b when b.IsInteger ->
            if b.IsNegative then
                if a.IsZero then Value.ComplexInfinity
                // workaround bug in BigRational with negative powers - drop after upgrading to > v3.0.0-alpha9
                else Value.Number (BigRational.Pow(BigRational.Reciprocal a, -int(b.Numerator)))
            else Value.Number (BigRational.Pow(a, int(b.Numerator)))
        | Value.Approximation a, Value.Approximation b -> ApproxOperations.pow (a, b) |> approx
        | Value.Number a, Value.Number b -> ApproxOperations.pow (ApproxOperations.fromRational a, ApproxOperations.fromRational b) |> approx
        | Value.Approximation a, Value.Number b -> ApproxOperations.pow (a, ApproxOperations.fromRational b) |> approx
        | Value.Number a, Value.Approximation b -> ApproxOperations.pow (ApproxOperations.fromRational a, b) |> approx

    let apply f = function
        | Value.Approximation a -> ApproxOperations.apply f a |> approx
        | Value.Number a -> ApproxOperations.fromRational a |> ApproxOperations.apply f |> approx
        | Value.Undefined _ -> Value.Undefined
        | Value.ComplexInfinity -> Value.Undefined // TODO
        | Value.PositiveInfinity -> Value.Undefined // TODO
        | Value.NegativeInfinity -> Value.Undefined // TODO
