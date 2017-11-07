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
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Value =

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
        | Value.Approximation a when Approximation.isZero a -> Some Zero
        | _ -> None

    let (|One|_|) = function
        | Value.Number n when n.IsOne -> Some One
        | Value.Approximation a when Approximation.isOne a -> Some One
        | _ -> None

    let (|MinusOne|_|) = function
        | Value.Number n when n.IsInteger && n.Numerator = BigInteger.MinusOne -> Some MinusOne
        | Value.Approximation a when Approximation.isMinusOne a -> Some MinusOne
        | _ -> None

    let (|Positive|_|) = function
        | Value.Number n when n.IsPositive -> Some Positive
        | Value.Approximation x when Approximation.isPositive x -> Some Positive
        | Value.PositiveInfinity -> Some Positive
        | _ -> None

    let (|Negative|_|) = function
        | Value.Number n when n.IsNegative -> Some Negative
        | Value.Approximation x when Approximation.isNegative x -> Some Negative
        | Value.NegativeInfinity -> Some Negative
        | _ -> None

    let isZero = function | Zero -> true | _ -> false
    let isOne = function | One -> true | _ -> false
    let isMinusOne = function | MinusOne -> true | _ -> false
    let isPositive = function | Positive -> true | _ -> false
    let isNegative = function | Negative -> true | _ -> false

    let negate = function
        | Value.Number a -> Value.Number (-a)
        | Value.Approximation a -> Approximation.negate a |> approx
        | Value.NegativeInfinity -> Value.PositiveInfinity
        | Value.PositiveInfinity -> Value.NegativeInfinity
        | Value.ComplexInfinity -> Value.ComplexInfinity
        | Value.Undefined -> Value.Undefined

    let abs = function
        | Value.Number a when a.IsNegative -> Value.Number (-a)
        | Value.Number _ as x -> x
        | Value.Approximation a -> Approximation.abs a |> approx
        | Value.NegativeInfinity | Value.PositiveInfinity | Value.ComplexInfinity -> Value.PositiveInfinity
        | Value.Undefined -> Value.Undefined

    let sum = function
        | Value.Undefined, _ | _, Value.Undefined -> Value.Undefined
        | Zero, b | b, Zero -> b
        | Value.Number a, Value.Number b -> Value.Number (a + b)
        | Value.Approximation a, Value.Approximation b -> Approximation.sum (a, b) |> approx
        | Value.Number a, Value.Approximation b | Value.Approximation b, Value.Number a -> Approximation.sum (Approximation.fromRational a, b) |> approx
        | Value.ComplexInfinity, (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity) -> Value.Undefined
        | (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity),  Value.ComplexInfinity -> Value.Undefined
        | Value.ComplexInfinity, _ | _, Value.ComplexInfinity -> Value.ComplexInfinity
        | Value.PositiveInfinity, Value.PositiveInfinity -> Value.PositiveInfinity
        | Value.PositiveInfinity, Value.NegativeInfinity | Value.NegativeInfinity, Value.PositiveInfinity -> Value.Undefined
        | Value.PositiveInfinity, _ | _, Value.PositiveInfinity -> Value.PositiveInfinity
        | Value.NegativeInfinity, Value.NegativeInfinity -> Value.NegativeInfinity
        | Value.NegativeInfinity, _ | _, Value.NegativeInfinity -> Value.NegativeInfinity        

    let product = function
        | Value.Undefined, _ | _, Value.Undefined -> Value.Undefined
        | One, b | b, One -> b
        | Zero, (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity) -> Value.Undefined
        | (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity), Zero -> Value.Undefined
        | Zero, _ | _, Zero -> zero
        | Value.Number a, Value.Number b -> Value.Number (a * b)
        | Value.Approximation a, Value.Approximation b -> Approximation.product (a, b) |> approx
        | Value.Number a, Value.Approximation b | Value.Approximation b, Value.Number a -> Approximation.product (Approximation.fromRational a, b) |> approx
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
        | Value.Approximation a -> Approximation.invert a |> approx
        | Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity -> zero
        | Value.Undefined -> Value.Undefined

    let power = function
        | Value.Undefined, _ | _, Value.Undefined -> Value.Undefined
        | Zero, Zero -> Value.Undefined
        | Zero, (Value.ComplexInfinity | Value.PositiveInfinity) -> zero
        | Zero, Value.NegativeInfinity -> Value.ComplexInfinity
        | Zero, Positive -> zero
        | Zero, Negative -> Value.ComplexInfinity
        | (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity), Zero -> Value.Undefined
        | (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity), Value.PositiveInfinity -> Value.ComplexInfinity
        | (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity), Value.Number b when b.IsNegative -> zero
        | Value.ComplexInfinity, Positive -> Value.ComplexInfinity
        | Value.PositiveInfinity, Positive -> Value.PositiveInfinity
        | Value.NegativeInfinity, Value.Number b when b.IsPositive && b.IsInteger ->
            if (b.Numerator % 2I).IsZero then Value.PositiveInfinity else Value.NegativeInfinity
        | One, (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity) | MinusOne, (Value.ComplexInfinity | Value.PositiveInfinity | Value.NegativeInfinity) -> Value.Undefined
        | One, _ | _, Zero -> one
        | _, Zero -> one
        | a, One -> a
        | One, _ -> one
        | Positive, Value.PositiveInfinity -> Value.PositiveInfinity
        | Negative, Value.PositiveInfinity -> Value.ComplexInfinity
        | _, Value.NegativeInfinity -> zero
        | _, Value.ComplexInfinity -> Value.Undefined
        | Value.Number a, Value.Number b when b.IsInteger ->
            if b.IsNegative then
                if a.IsZero then Value.ComplexInfinity
                // workaround bug in BigRational with negative powers - drop after upgrading to > v3.0.0-alpha9
                else Value.Number (BigRational.Pow(BigRational.Reciprocal a, -int(b.Numerator)))
            else Value.Number (BigRational.Pow(a, int(b.Numerator)))
        | Value.Approximation a, Value.Approximation b -> Approximation.pow (a, b) |> approx
        | Value.Number a, Value.Number b -> Approximation.pow (Approximation.fromRational a, Approximation.fromRational b) |> approx
        | Value.Approximation a, Value.Number b -> Approximation.pow (a, Approximation.fromRational b) |> approx
        | Value.Number a, Value.Approximation b -> Approximation.pow (Approximation.fromRational a, b) |> approx
        | _ -> Value.Undefined // TODO

    let apply f = function
        | Value.Approximation a -> Approximation.apply f a |> approx
        | Value.Number a -> Approximation.fromRational a |> Approximation.apply f |> approx
        | Value.Undefined _ -> Value.Undefined
        | Value.ComplexInfinity -> Value.Undefined // TODO
        | Value.PositiveInfinity -> Value.Undefined // TODO
        | Value.NegativeInfinity -> Value.Undefined // TODO
