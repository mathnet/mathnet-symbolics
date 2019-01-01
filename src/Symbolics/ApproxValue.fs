namespace MathNet.Symbolics

open System
open MathNet.Numerics
open MathNet.Symbolics


[<RequireQualifiedAccess>]
type ApproxValue =
    | Real of float
    | Complex of complex
    | ComplexInfinity
    | PositiveInfinity
    | NegativeInfinity
    | DirectedInfinity of complex
    | Undefined


[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ApproxValue =

    let zero = ApproxValue.Real 0.0
    let one = ApproxValue.Real 1.0
    let minusOne = ApproxValue.Real -1.0
    let imagOne = ApproxValue.Complex (complex 0.0 1.0)
    let imagMinusOne = ApproxValue.Complex (complex 0.0 -1.0)

    let inline fromReal (x:float) = ApproxValue.Real x
    let fromReal32 (x:float32) = ApproxValue.Real (float x)
    let fromComplex (x:complex) =
        if x.Imaginary = 0.0 then ApproxValue.Real x.Real
        else ApproxValue.Complex x
    let fromComplex32 (x:complex32) =
        if x.Imaginary = 0.0f then ApproxValue.Real (float x.Real)
        else ApproxValue.Complex (complex (float x.Real) (float x.Imaginary))
    let fromComplexReal (real:float) (imag:float) =
        if imag = 0.0 then ApproxValue.Real real
        else ApproxValue.Complex (complex real imag)

    let fromInt32 (x:int) = ApproxValue.Real (float x)
    let fromInt64 (x:int64) = ApproxValue.Real (float x)
    let fromInteger (x:BigInteger) = ApproxValue.Real (float x)
    let fromIntegerFraction (n:BigInteger) (d:BigInteger) = ApproxValue.Real (float n / float d)
    let fromRational (x:BigRational) = ApproxValue.Real (float x)
    let fromComplexInt32 (real:int) (imag:int) =
        if imag = 0 then fromInt32 real
        else ApproxValue.Complex (complex (float real) (float imag))
    let fromComplexInt64 (real:int64) (imag:int64) =
        if imag = 0L then fromInt64 real
        else ApproxValue.Complex (complex (float real) (float imag))
    let fromComplexInteger (real:BigInteger) (imag:BigInteger) =
        if imag.IsZero then fromInteger real
        else ApproxValue.Complex (complex (float real) (float imag))
    let fromComplexRational (real:BigRational) (imag:BigRational) =
        if imag.IsZero then fromRational real
        else ApproxValue.Complex (complex (float real) (float imag))

    let fromInfinityDirectedInt32 (real:int) (imag:int) =
        match real, imag with
        | 0, 0 -> ApproxValue.Undefined
        | 1, 0 -> ApproxValue.PositiveInfinity
        | -1, 0 -> ApproxValue.NegativeInfinity
        | r, i -> ApproxValue.DirectedInfinity (complex (float r) (float i))
    let fromInfinityDirectedInteger (real:BigInteger) (imag:BigInteger) =
        match real, imag with
        | r, i when r.IsZero && i.IsZero -> ApproxValue.Undefined
        | r, i when r.IsOne && i.IsZero -> ApproxValue.PositiveInfinity
        | r, i when r = BigInteger.MinusOne && i.IsZero -> ApproxValue.NegativeInfinity
        | r, i -> ApproxValue.DirectedInfinity (complex (float r) (float i))
    let fromInfinityDirectedRational (real:BigRational) (imag:BigRational) =
        match real, imag with
        | r, i when r.IsZero && i.IsZero -> ApproxValue.Undefined
        | r, i when r.IsOne && i.IsZero -> ApproxValue.PositiveInfinity
        | r, i when r.IsInteger && r.Numerator = BigInteger.MinusOne && i.IsZero -> ApproxValue.NegativeInfinity
        | r, i -> ApproxValue.DirectedInfinity (complex (float r) (float i))
    let fromInfinityDirectedReal (real:float) (imag:float) =
        match real, imag with
        | r, i when r = 0.0 && i = 0.0 -> ApproxValue.Undefined
        | r, i when r = 1.0 && i = 0.0 -> ApproxValue.PositiveInfinity
        | r, i when r = -1.0 && i = 0.0 -> ApproxValue.NegativeInfinity
        | r, i -> ApproxValue.DirectedInfinity (complex r i)

    let (|Zero|_|) = function
        | ApproxValue.Real x when x = 0.0 -> Some Zero
        | ApproxValue.Complex x when x.IsZero() -> Some Zero
        | _ -> None

    let (|Positive|_|) = function
        | ApproxValue.Real x when x > 0.0 -> Some Positive
        | ApproxValue.Complex x when x.IsReal() && x.Real > 0.0 -> Some Positive
        | ApproxValue.PositiveInfinity -> Some Positive
        | _ -> None

    let (|Negative|_|) = function
        | ApproxValue.Real x when x < 0.0 -> Some Negative
        | ApproxValue.Complex x when x.IsReal() && x.Real < 0.0 -> Some Negative
        | ApproxValue.NegativeInfinity -> Some Negative
        | _ -> None

    let isZero = function | Zero -> true | _ -> false
    let isPositive = function | Positive -> true | _ -> false
    let isNegative = function | Negative -> true | _ -> false

    let negate = function
        | ApproxValue.Real a -> fromReal (-a)
        | ApproxValue.Complex a -> fromComplexReal (-a.Real) (-a.Imaginary)
        | ApproxValue.NegativeInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.PositiveInfinity -> ApproxValue.NegativeInfinity
        | ApproxValue.ComplexInfinity -> ApproxValue.ComplexInfinity
        | ApproxValue.DirectedInfinity d -> fromInfinityDirectedReal (-d.Real) (-d.Imaginary)
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let abs = function
        | ApproxValue.Real a -> fromReal (Math.Abs a)
        | ApproxValue.Complex a -> fromReal (Complex.magnitude a)
        | ApproxValue.NegativeInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.DirectedInfinity _ -> ApproxValue.PositiveInfinity
        | ApproxValue.ComplexInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let add augend addend =
        match augend, addend with
        | ApproxValue.Undefined, _ | _, ApproxValue.Undefined -> ApproxValue.Undefined
        | Zero, b | b, Zero -> b
        | ApproxValue.Real a, ApproxValue.Real b -> fromReal (a + b)
        | ApproxValue.Complex a, ApproxValue.Complex b -> fromComplex (a + b)
        | ApproxValue.Real a, ApproxValue.Complex b | ApproxValue.Complex b, ApproxValue.Real a -> fromComplexReal (a + b.Real) b.Imaginary
        | ApproxValue.ComplexInfinity, (ApproxValue.ComplexInfinity | ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity | ApproxValue.DirectedInfinity _) -> ApproxValue.Undefined
        | (ApproxValue.ComplexInfinity | ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity | ApproxValue.DirectedInfinity _),  ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity, _ | _, ApproxValue.ComplexInfinity -> ApproxValue.ComplexInfinity
        | ApproxValue.PositiveInfinity, ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.PositiveInfinity, ApproxValue.NegativeInfinity | ApproxValue.NegativeInfinity, ApproxValue.PositiveInfinity -> ApproxValue.Undefined
        | ApproxValue.PositiveInfinity, ApproxValue.DirectedInfinity _ | ApproxValue.DirectedInfinity _, ApproxValue.PositiveInfinity -> ApproxValue.Undefined
        | ApproxValue.PositiveInfinity, _ | _, ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity, ApproxValue.NegativeInfinity -> ApproxValue.NegativeInfinity
        | ApproxValue.NegativeInfinity, ApproxValue.DirectedInfinity _ | ApproxValue.DirectedInfinity _, ApproxValue.NegativeInfinity -> ApproxValue.Undefined
        | ApproxValue.NegativeInfinity, _ | _, ApproxValue.NegativeInfinity -> ApproxValue.NegativeInfinity
        | ApproxValue.DirectedInfinity a, ApproxValue.DirectedInfinity b when a = b -> ApproxValue.DirectedInfinity a
        | ApproxValue.DirectedInfinity a, ApproxValue.DirectedInfinity b -> fromInfinityDirectedReal (a.Real + b.Real) (a.Imaginary + b.Imaginary)
        | ApproxValue.DirectedInfinity d, _ | _, ApproxValue.DirectedInfinity d -> ApproxValue.DirectedInfinity d

    let multiply multiplier multiplicand =
        match multiplier, multiplicand with
        | ApproxValue.Undefined, _ | _, ApproxValue.Undefined -> ApproxValue.Undefined
        | Zero, (ApproxValue.ComplexInfinity | ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity | ApproxValue.DirectedInfinity _) -> ApproxValue.Undefined
        | (ApproxValue.ComplexInfinity | ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity | ApproxValue.DirectedInfinity _), Zero -> ApproxValue.Undefined
        | Zero, _ | _, Zero -> zero
        | ApproxValue.Real a, ApproxValue.Real b -> fromReal (a * b)
        | ApproxValue.Complex a, ApproxValue.Complex b -> fromComplex (a * b)
        | ApproxValue.Real a, ApproxValue.Complex b | ApproxValue.Complex b, ApproxValue.Real a -> fromComplexReal (a * b.Real) (a * b.Imaginary)
        | ApproxValue.ComplexInfinity, _ | _, ApproxValue.ComplexInfinity -> ApproxValue.ComplexInfinity
        | ApproxValue.PositiveInfinity, Positive | Positive, ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.PositiveInfinity, Negative | Negative, ApproxValue.PositiveInfinity -> ApproxValue.NegativeInfinity
        | ApproxValue.NegativeInfinity, Positive | Positive, ApproxValue.NegativeInfinity -> ApproxValue.NegativeInfinity
        | ApproxValue.NegativeInfinity, Negative | Negative, ApproxValue.NegativeInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.DirectedInfinity d, Positive | Positive, ApproxValue.DirectedInfinity d -> (ApproxValue.DirectedInfinity d)
        | ApproxValue.DirectedInfinity d, Negative | Negative, ApproxValue.DirectedInfinity d -> fromInfinityDirectedReal (-d.Real) (-d.Imaginary)
        | ApproxValue.NegativeInfinity, _ | _, ApproxValue.NegativeInfinity -> ApproxValue.NegativeInfinity
        | ApproxValue.PositiveInfinity, _ | _, ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.DirectedInfinity d, _ | _, ApproxValue.DirectedInfinity d -> (ApproxValue.DirectedInfinity d)

    let invert = function
        | Zero -> ApproxValue.ComplexInfinity
        | ApproxValue.Real a -> fromReal (1.0 / a)
        | ApproxValue.Complex a ->
            let denominator = a.Real*a.Real + a.Imaginary*a.Imaginary
            fromComplexReal (a.Real/denominator) (-a.Imaginary/denominator)
        | ApproxValue.ComplexInfinity | ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity | ApproxValue.DirectedInfinity _ -> zero
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let power exponent radix =
        match radix, exponent with
        | ApproxValue.Undefined, _ | _, ApproxValue.Undefined -> ApproxValue.Undefined
        | Zero, Zero -> ApproxValue.Undefined
        | Zero, (ApproxValue.ComplexInfinity | ApproxValue.PositiveInfinity) -> zero
        | Zero, ApproxValue.NegativeInfinity -> ApproxValue.ComplexInfinity
        | Zero, Positive -> zero
        | Zero, Negative -> ApproxValue.ComplexInfinity
        | (ApproxValue.ComplexInfinity | ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity | ApproxValue.DirectedInfinity _), Zero -> ApproxValue.Undefined
        | (ApproxValue.ComplexInfinity | ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity | ApproxValue.DirectedInfinity _), ApproxValue.PositiveInfinity -> ApproxValue.ComplexInfinity
        | (ApproxValue.ComplexInfinity | ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity | ApproxValue.DirectedInfinity _), ApproxValue.Real b when b < 0.0 -> zero
        | ApproxValue.ComplexInfinity, Positive -> ApproxValue.ComplexInfinity
        | ApproxValue.PositiveInfinity, Positive -> ApproxValue.PositiveInfinity
        | _, Zero -> one
        | Positive, ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | Negative, ApproxValue.PositiveInfinity -> ApproxValue.ComplexInfinity
        | _, ApproxValue.NegativeInfinity -> zero
        | _, ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.DirectedInfinity _, _ | _, ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined // TODO
        | ApproxValue.Real a, ApproxValue.Real b -> fromReal (a**b)
        | ApproxValue.Complex a, ApproxValue.Complex b -> fromComplex (Complex.pow b a)
        | ApproxValue.Real a, ApproxValue.Complex b -> fromComplex (Complex.pow b (complex a 0.0))
        | ApproxValue.Complex a, ApproxValue.Real b -> fromComplex (Complex.pow (complex b 0.0) a)

    let ln = function
        | Zero -> ApproxValue.NegativeInfinity
        | ApproxValue.Real x when x < 0.0 -> fromComplex (Complex.ln (complex x 0.0))
        | ApproxValue.Real x -> fromReal (Math.Log x)
        | ApproxValue.Complex x -> fromComplex (Complex.ln x)
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.DirectedInfinity _ -> ApproxValue.PositiveInfinity
        | ApproxValue.ComplexInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let log10 = function
        | Zero -> ApproxValue.NegativeInfinity
        | ApproxValue.Real x when x < 0.0 -> fromComplex (Complex.log10 (complex x 0.0))
        | ApproxValue.Real x -> fromReal (Math.Log10 x)
        | ApproxValue.Complex x -> fromComplex (Complex.log10 x)
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.DirectedInfinity _ -> ApproxValue.PositiveInfinity
        | ApproxValue.ComplexInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let log b x =
        match b, x with
        | ApproxValue.Real v, ApproxValue.Real w -> fromReal (Math.Log(w, v))
        | ApproxValue.Real v, ApproxValue.Complex w -> fromComplex (Complex.log v w)
        | _ -> failwith "not supported"

    let exp = function
        | Zero -> one
        | ApproxValue.Real x -> fromReal (Math.Exp x)
        | ApproxValue.Complex x -> fromComplex (Complex.exp x)
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined


    let sin = function
        | Zero -> zero
        | ApproxValue.Real a -> fromReal (Math.Sin a)
        | ApproxValue.Complex a -> fromComplex (Complex.sin a)
        | ApproxValue.PositiveInfinity -> ApproxValue.Undefined
        | ApproxValue.NegativeInfinity -> ApproxValue.Undefined
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let cos = function
        | Zero -> one
        | ApproxValue.Real a -> fromReal (Math.Cos a)
        | ApproxValue.Complex a -> fromComplex (Complex.cos a)
        | ApproxValue.PositiveInfinity -> ApproxValue.Undefined
        | ApproxValue.NegativeInfinity -> ApproxValue.Undefined
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let tan = function
        | Zero -> zero
        | ApproxValue.Real a -> fromReal (Math.Tan a)
        | ApproxValue.Complex a -> fromComplex (Complex.tan a)
        | ApproxValue.PositiveInfinity -> ApproxValue.Undefined
        | ApproxValue.NegativeInfinity -> ApproxValue.Undefined
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let csc = function
        | Zero -> ApproxValue.ComplexInfinity
        | ApproxValue.Real a -> fromReal (Trig.Csc a)
        | ApproxValue.Complex a -> fromComplex (Trig.Csc a)
        | ApproxValue.PositiveInfinity -> ApproxValue.Undefined
        | ApproxValue.NegativeInfinity -> ApproxValue.Undefined
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let sec = function
        | Zero -> one
        | ApproxValue.Real a -> fromReal (Trig.Sec a)
        | ApproxValue.Complex a -> fromComplex (Trig.Sec a)
        | ApproxValue.PositiveInfinity -> ApproxValue.Undefined
        | ApproxValue.NegativeInfinity -> ApproxValue.Undefined
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let cot = function
        | Zero -> ApproxValue.ComplexInfinity
        | ApproxValue.Real a -> fromReal (Trig.Cot a)
        | ApproxValue.Complex a -> fromComplex (Trig.Cot a)
        | ApproxValue.PositiveInfinity -> ApproxValue.Undefined
        | ApproxValue.NegativeInfinity -> ApproxValue.Undefined
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined


    let sinh = function
        | Zero -> zero
        | ApproxValue.Real a -> fromReal (Trig.Sinh a)
        | ApproxValue.Complex a -> fromComplex (Trig.Sinh a)
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity -> ApproxValue.NegativeInfinity
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let cosh = function
        | Zero -> one
        | ApproxValue.Real a -> fromReal (Trig.Cosh a)
        | ApproxValue.Complex a -> fromComplex (Trig.Cosh a)
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let tanh = function
        | Zero -> zero
        | ApproxValue.Real a -> fromReal (Trig.Tanh a)
        | ApproxValue.Complex a -> fromComplex (Trig.Tanh a)
        | ApproxValue.PositiveInfinity -> one
        | ApproxValue.NegativeInfinity -> minusOne
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let csch = function
        | Zero -> ApproxValue.ComplexInfinity
        | ApproxValue.Real a -> fromReal (Trig.Csch a)
        | ApproxValue.Complex a -> fromComplex (Trig.Csch a)
        | ApproxValue.PositiveInfinity -> zero
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let sech = function
        | Zero -> one
        | ApproxValue.Real a -> fromReal (Trig.Sech a)
        | ApproxValue.Complex a -> fromComplex (Trig.Sech a)
        | ApproxValue.PositiveInfinity -> zero
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let coth = function
        | Zero -> ApproxValue.ComplexInfinity
        | ApproxValue.Real a -> fromReal (Trig.Coth a)
        | ApproxValue.Complex a -> fromComplex (Trig.Coth a)
        | ApproxValue.PositiveInfinity -> one
        | ApproxValue.NegativeInfinity -> minusOne
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined


    let asin = function
        | Zero -> zero
        | ApproxValue.Real a -> fromReal (Trig.Asin a)
        | ApproxValue.Complex a -> fromComplex (Trig.Asin a)
        | ApproxValue.PositiveInfinity -> fromInfinityDirectedInt32 0 -1
        | ApproxValue.NegativeInfinity -> fromInfinityDirectedInt32 0 1
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.ComplexInfinity
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let acos = function
        | Zero -> ApproxValue.Real Constants.PiOver2
        | ApproxValue.Real a -> fromReal (Trig.Acos a)
        | ApproxValue.Complex a -> fromComplex (Trig.Acos a)
        | ApproxValue.PositiveInfinity -> fromInfinityDirectedInt32 0 1
        | ApproxValue.NegativeInfinity -> fromInfinityDirectedInt32 0 -1
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.ComplexInfinity
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let atan = function
        | Zero -> zero
        | ApproxValue.Real a -> fromReal (Trig.Atan a)
        | ApproxValue.Complex a -> fromComplex (Trig.Atan a)
        | ApproxValue.PositiveInfinity -> ApproxValue.Real Constants.PiOver2
        | ApproxValue.NegativeInfinity -> ApproxValue.Real -Constants.PiOver2
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let atan2 x y =
        match x, y with
        | ApproxValue.Real x, ApproxValue.Real y -> Math.Atan2 (x, y) |> fromReal
        | ApproxValue.Complex x, ApproxValue.Complex y -> Complex.atan (x / y) |> fromComplex
        | ApproxValue.Complex x, ApproxValue.Real y -> Complex.atan (x / (complex y 0.0)) |> fromComplex
        | ApproxValue.Real x, ApproxValue.Complex y -> Complex.atan (complex x 0.0) / y |> fromComplex
        | ApproxValue.Undefined, _ | _, ApproxValue.Undefined -> ApproxValue.Undefined
        | ApproxValue.ComplexInfinity, _ | _, ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.DirectedInfinity _, _ | _, ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | (ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity), (ApproxValue.PositiveInfinity | ApproxValue.NegativeInfinity) -> ApproxValue.Undefined
        | _, ApproxValue.PositiveInfinity -> ApproxValue.Real Constants.PiOver2
        | _, ApproxValue.NegativeInfinity -> ApproxValue.Real (-Constants.PiOver2)
        | ApproxValue.PositiveInfinity, _ -> zero
        | ApproxValue.NegativeInfinity, _ -> ApproxValue.Real Constants.Pi

    let acsc = function
        | Zero -> ApproxValue.ComplexInfinity
        | ApproxValue.Real a -> fromReal (Trig.Acsc a)
        | ApproxValue.Complex a -> fromComplex (Trig.Acsc a)
        | ApproxValue.PositiveInfinity -> zero
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> zero
        | ApproxValue.ComplexInfinity -> zero
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let asec = function
        | Zero -> ApproxValue.ComplexInfinity
        | ApproxValue.Real a -> fromReal (Trig.Asec a)
        | ApproxValue.Complex a -> fromComplex (Trig.Asec a)
        | ApproxValue.PositiveInfinity -> ApproxValue.Real Constants.PiOver2
        | ApproxValue.NegativeInfinity -> ApproxValue.Real Constants.PiOver2
        | ApproxValue.DirectedInfinity _ -> ApproxValue.Real Constants.PiOver2
        | ApproxValue.ComplexInfinity -> ApproxValue.Real Constants.PiOver2
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let acot = function
        | Zero -> ApproxValue.Real Constants.PiOver2
        | ApproxValue.Real a -> fromReal (Trig.Acot a)
        | ApproxValue.Complex a -> fromComplex (Trig.Acot a)
        | ApproxValue.PositiveInfinity -> zero
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> zero
        | ApproxValue.ComplexInfinity -> zero
        | ApproxValue.Undefined -> ApproxValue.Undefined


    let asinh = function
        | Zero -> zero
        | ApproxValue.Real a -> fromReal (Trig.Asinh a)
        | ApproxValue.Complex a -> fromComplex (Trig.Asinh a)
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity -> ApproxValue.NegativeInfinity
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.ComplexInfinity
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let acosh = function
        | ApproxValue.Real a -> fromReal (Trig.Acosh a)
        | ApproxValue.Complex a -> fromComplex (Trig.Acosh a)
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let atanh = function
        | ApproxValue.Real a -> fromReal (Trig.Atanh a)
        | ApproxValue.Complex a -> fromComplex (Trig.Atanh a)
        | ApproxValue.PositiveInfinity -> fromComplexReal 0.0 -Constants.PiOver2
        | ApproxValue.NegativeInfinity -> fromComplexReal 0.0 Constants.PiOver2
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let acsch = function
        | ApproxValue.Real a -> fromReal (Trig.Acsch a)
        | ApproxValue.Complex a -> fromComplex (Trig.Acsch a)
        | ApproxValue.PositiveInfinity -> zero
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> zero
        | ApproxValue.ComplexInfinity -> zero
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let asech = function
        | ApproxValue.Real a -> fromReal (Trig.Asech a)
        | ApproxValue.Complex a -> fromComplex (Trig.Asech a)
        | ApproxValue.PositiveInfinity -> fromComplexReal 0.0 Constants.PiOver2
        | ApproxValue.NegativeInfinity -> fromComplexReal 0.0 Constants.PiOver2
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let acoth = function
        | ApproxValue.Real a -> fromReal (Trig.Acoth a)
        | ApproxValue.Complex a -> fromComplex (Trig.Acoth a)
        | ApproxValue.PositiveInfinity -> zero
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> zero
        | ApproxValue.ComplexInfinity -> zero
        | ApproxValue.Undefined -> ApproxValue.Undefined


    let airyai = function
        | ApproxValue.Real a -> fromReal (SpecialFunctions.AiryAi a)
        | ApproxValue.Complex a -> fromComplex (SpecialFunctions.AiryAi a)
        | ApproxValue.PositiveInfinity -> zero
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let airyaiprime = function
        | ApproxValue.Real a -> fromReal (SpecialFunctions.AiryAiPrime a)
        | ApproxValue.Complex a -> fromComplex (SpecialFunctions.AiryAiPrime a)
        | ApproxValue.PositiveInfinity -> zero
        | ApproxValue.NegativeInfinity _ -> failwith "not implemented"
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let airybi = function
        | ApproxValue.Real a -> fromReal (SpecialFunctions.AiryBi a)
        | ApproxValue.Complex a -> fromComplex (SpecialFunctions.AiryBi a)
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined

    let airybiprime = function
        | ApproxValue.Real a -> fromReal (SpecialFunctions.AiryBiPrime a)
        | ApproxValue.Complex a -> fromComplex (SpecialFunctions.AiryBiPrime a)
        | ApproxValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ApproxValue.NegativeInfinity -> zero
        | ApproxValue.DirectedInfinity _ -> failwith "not implemented"
        | ApproxValue.ComplexInfinity -> ApproxValue.Undefined
        | ApproxValue.Undefined -> ApproxValue.Undefined


    let besselj nu z =
        match nu, z with
        | ApproxValue.Real a, ApproxValue.Real b -> SpecialFunctions.BesselJ (a, b) |> fromReal
        | ApproxValue.Real a, ApproxValue.Complex b -> SpecialFunctions.BesselJ (a, b) |> fromComplex
        | _ -> failwith "not supported"

    let bessely nu z =
        match nu, z with
        | ApproxValue.Real a, ApproxValue.Real b -> SpecialFunctions.BesselY (a, b) |> fromReal
        | ApproxValue.Real a, ApproxValue.Complex b -> SpecialFunctions.BesselY (a, b) |> fromComplex
        | _ -> failwith "not supported"

    let besseli nu z =
        match nu, z with
        | ApproxValue.Real a, ApproxValue.Real b -> SpecialFunctions.BesselI (a, b) |> fromReal
        | ApproxValue.Real a, ApproxValue.Complex b -> SpecialFunctions.BesselI (a, b) |> fromComplex
        | _ -> failwith "not supported"

    let besselk nu z =
        match nu, z with
        | ApproxValue.Real a, ApproxValue.Real b -> SpecialFunctions.BesselK (a, b) |> fromReal
        | ApproxValue.Real a, ApproxValue.Complex b -> SpecialFunctions.BesselK (a, b) |> fromComplex
        | _ -> failwith "not supported"

    let besseliratio nu z =
        match nu, z with
        | ApproxValue.Real a, ApproxValue.Real b -> SpecialFunctions.BesselIScaled (a + 1.0, b) / SpecialFunctions.BesselIScaled (a, b) |> fromReal
        | ApproxValue.Real a, ApproxValue.Complex b -> SpecialFunctions.BesselIScaled (a + 1.0, b) / SpecialFunctions.BesselIScaled (a, b) |> fromComplex
        | _ -> failwith "not supported"

    let besselkratio nu z =
        match nu, z with
        | ApproxValue.Real a, ApproxValue.Real b -> SpecialFunctions.BesselKScaled (a + 1.0, b) / SpecialFunctions.BesselKScaled (a, b) |> fromReal
        | ApproxValue.Real a, ApproxValue.Complex b -> SpecialFunctions.BesselKScaled (a + 1.0, b) / SpecialFunctions.BesselKScaled (a, b) |> fromComplex
        | _ -> failwith "not supported"

    let hankelh1 nu z =
        match nu, z with
        | ApproxValue.Real a, ApproxValue.Real b -> SpecialFunctions.HankelH1 (a, complex b 0.0) |> fromComplex
        | ApproxValue.Real a, ApproxValue.Complex b -> SpecialFunctions.HankelH1 (a, b) |> fromComplex
        | _ -> failwith "not supported"

    let hankelh2 nu z =
        match nu, z with
        | ApproxValue.Real a, ApproxValue.Real b -> SpecialFunctions.HankelH2 (a, complex b 0.0) |> fromComplex
        | ApproxValue.Real a, ApproxValue.Complex b -> SpecialFunctions.HankelH2 (a, b) |> fromComplex
        | _ -> failwith "not supported"

    let apply f a =
        match f with
        | Abs -> abs a
        | Ln -> ln a
        | Log -> log10 a
        | Exp -> exp a
        | Sin -> sin a
        | Cos -> cos a
        | Tan -> tan a
        | Csc -> csc a
        | Sec -> sec a
        | Cot -> cot a
        | Sinh -> sinh a
        | Cosh-> cosh a
        | Tanh -> tanh a
        | Csch -> csch a
        | Sech -> sech a
        | Coth -> coth a
        | Asin -> asin a
        | Acos -> acos a
        | Atan -> atan a
        | Acsc -> acsc a
        | Asec -> asec a
        | Acot -> acot a
        | Asinh -> asinh a
        | Acosh -> acosh a
        | Atanh -> atanh a
        | Acsch -> acsch a
        | Asech -> asech a
        | Acoth -> acoth a
        | AiryAi -> airyai a
        | AiryAiPrime -> airyaiprime a
        | AiryBi -> airybi a
        | AiryBiPrime -> airybiprime a
        | _ -> failwith "Not Supported"

    let applyN f xs =
        match f, xs with
        | Atan, [x; y] -> atan2 x y
        | Log, [b; x] -> log b x
        | BesselJ, [nu; x] -> besselj nu x
        | BesselY, [nu; x] -> bessely nu x
        | BesselI, [nu; x] -> besseli nu x
        | BesselK, [nu; x] -> besselk nu x
        | BesselIRatio, [nu; x] -> besseliratio nu x
        | BesselKRatio, [nu; x] -> besselkratio nu x
        | HankelH1, [nu; x] -> hankelh1 nu x
        | HankelH2, [nu; x] -> hankelh2 nu x
        | _ -> failwith "Not Supported"

    let internal orderRelation (x:ApproxValue) (y:ApproxValue) =
        match x, y with
        | ApproxValue.Real x, ApproxValue.Real y -> x < y
        | ApproxValue.Complex x, ApproxValue.Complex y -> x.Real < y.Real || x.Real = y.Real && x.Imaginary < y.Imaginary
        | ApproxValue.Real x, ApproxValue.Complex y -> not (y.IsReal()) || x < y.Real
        | ApproxValue.Complex x, ApproxValue.Real y -> x.IsReal() && x.Real < y
        | ApproxValue.DirectedInfinity x, ApproxValue.DirectedInfinity y -> x.Real < y.Real || x.Real = y.Real && x.Imaginary < y.Imaginary
        | _, ApproxValue.Undefined -> true
        | _, ApproxValue.ComplexInfinity -> true
        | _, ApproxValue.NegativeInfinity -> true
        | _, ApproxValue.PositiveInfinity -> true
        | _, ApproxValue.DirectedInfinity _ -> true
        | ApproxValue.DirectedInfinity _, _ -> false
        | ApproxValue.PositiveInfinity, _ -> false
        | ApproxValue.NegativeInfinity, _ -> false
        | ApproxValue.ComplexInfinity, _ -> false
        | ApproxValue.Undefined, _ -> false

    /// Sort in a list with standard expression ordering.
    [<CompiledName("SortList")>]
    let sortList list =
        List.sortWith (fun a b -> if a = b then 0 elif orderRelation a b then -1 else 1) list
