namespace MathNet.Symbolics

open System
open MathNet.Numerics
open MathNet.Symbolics

type BigInteger = System.Numerics.BigInteger

type ComplexBigRational = {
    Real: BigRational
    Imaginary: BigRational
}

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ComplexBigRational =
    let (|RealRational|_|) ({ ComplexBigRational.Real=real; Imaginary=imag }) : BigRational option = if imag.IsZero then Some real else None
    let (|RealInteger|_|) ({ ComplexBigRational.Real=real; Imaginary=imag }) : BigInteger option = if imag.IsZero && real.IsInteger then Some real.Numerator else None
    let (|Complex|) ({ ComplexBigRational.Real=real; Imaginary=imag }) : BigRational * BigRational = (real, imag)
    let (|ComplexInteger|_|) ({ ComplexBigRational.Real=real; Imaginary=imag }) : (BigInteger * BigInteger) option = if real.IsInteger && imag.IsInteger then Some (real.Numerator, imag.Numerator) else None


[<RequireQualifiedAccess>]
module internal FromPrimitive =
    let inline complex32 (x:complex32) = complex (float x.Real) (float x.Imaginary)
    let inline int32 (x:int) = BigRational.FromInt x
    let inline int64 (x:int64) = BigRational.FromBigInt (BigInteger x)
    let inline bigint (x:BigInteger) = BigRational.FromBigInt x

[<RequireQualifiedAccess>]
type ExactValue =

    /// A rational number, example: 3/4
    | Rational of BigRational

    /// A complex rational number, example: 3/4 + 1/4*j
    | ComplexRational of ComplexBigRational

    /// A well-defined mathematical constant, example: pi, e
    | Constant of Constant

    /// A well-defined mathematical constant scaled by a complex rational number, example: 1/3 * j * pi
    | DirectedConstant of Constant * ComplexBigRational

    /// Complex infinity, i.e. a quantity with infinite magnitude but undetermined complex phase.
    | ComplexInfinity

    /// Infinity
    | PositiveInfinity

    /// Negative Infinity, i.e. -1 * oo
    | NegativeInfinity

    /// Infinity scaled (directed) by a complex rational number
    | DirectedInfinity of ComplexBigRational

    /// Undefined or underetmined
    | Undefined


[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExactValue =

    let zero = ExactValue.Rational BigRational.Zero
    let one = ExactValue.Rational BigRational.One
    let minusOne = ExactValue.Rational (BigRational.FromInt -1)
    let imagOne = ExactValue.ComplexRational { ComplexBigRational.Real = BigRational.Zero; Imaginary = BigRational.One }
    let imagMinusOne = ExactValue.ComplexRational { ComplexBigRational.Real = BigRational.Zero; Imaginary = -BigRational.One }

    let fromInt32 (x:int) = ExactValue.Rational (FromPrimitive.int32 x)
    let fromInt64 (x:int64) = ExactValue.Rational (FromPrimitive.int64 x)
    let fromInteger (x:BigInteger) = ExactValue.Rational (FromPrimitive.bigint x)
    let fromIntegerFraction (n:BigInteger) (d:BigInteger) = ExactValue.Rational (BigRational.FromBigIntFraction (n, d))
    let fromRational (x:BigRational) = ExactValue.Rational x

    let fromComplexInt32 (real:int) (imag:int) =
        if imag = 0 then fromInt32 real
        else ExactValue.ComplexRational { ComplexBigRational.Real = FromPrimitive.int32 real; Imaginary = FromPrimitive.int32 imag }
    let fromComplexInt64 (real:int64) (imag:int64) =
        if imag = 0L then fromInt64 real
        else ExactValue.ComplexRational { ComplexBigRational.Real = FromPrimitive.int64 real; Imaginary = FromPrimitive.int64 imag }
    let fromComplexInteger (real:BigInteger) (imag:BigInteger) =
        if imag.IsZero then fromInteger real
        else ExactValue.ComplexRational { ComplexBigRational.Real = FromPrimitive.bigint real; Imaginary = FromPrimitive.bigint imag }
    let fromComplexRational (real:BigRational) (imag:BigRational) =
        if imag.IsZero then fromRational real
        else ExactValue.ComplexRational { ComplexBigRational.Real = real; Imaginary = imag }

    let fromConstant (c:Constant) =
        match c with
        | I -> imagOne
        | constant -> ExactValue.Constant constant

    let fromConstantDirectedInt32 (c:Constant) (real:int) (imag:int) =
        if c = I then fromComplexInt32 -imag real else
        match real, imag with
        | 0, 0 -> zero
        | 1, 0 -> fromConstant c
        | r, i -> ExactValue.DirectedConstant (c, { ComplexBigRational.Real = FromPrimitive.int32 r; Imaginary = FromPrimitive.int32 i })
    let fromConstantDirectedInteger (c:Constant) (real:BigInteger) (imag:BigInteger) =
        if c = I then fromComplexInteger -imag real else
        match real, imag with
        | r, i when r.IsZero && i.IsZero -> zero
        | r, i when r.IsOne && i.IsZero -> fromConstant c
        | r, i -> ExactValue.DirectedConstant (c, { ComplexBigRational.Real = FromPrimitive.bigint r; Imaginary = FromPrimitive.bigint i })
    let fromConstantDirectedRational (c:Constant) (real:BigRational) (imag:BigRational) =
        if c = I then fromComplexRational -imag real else
        match real, imag with
        | r, i when r.IsZero && i.IsZero -> zero
        | r, i when r.IsOne && i.IsZero -> fromConstant c
        | r, i -> ExactValue.DirectedConstant (c, { ComplexBigRational.Real = r; Imaginary = i })

    let fromInfinityDirectedInt32 (real:int) (imag:int) =
        match real, imag with
        | 0, 0 -> ExactValue.Undefined
        | 1, 0 -> ExactValue.PositiveInfinity
        | -1, 0 -> ExactValue.NegativeInfinity
        | r, i -> ExactValue.DirectedInfinity { ComplexBigRational.Real = FromPrimitive.int32 r; Imaginary = FromPrimitive.int32 i }
    let fromInfinityDirectedInteger (real:BigInteger) (imag:BigInteger) =
        match real, imag with
        | r, i when r.IsZero && i.IsZero -> ExactValue.Undefined
        | r, i when r.IsOne && i.IsZero -> ExactValue.PositiveInfinity
        | r, i when r = BigInteger.MinusOne && i.IsZero -> ExactValue.NegativeInfinity
        | r, i -> ExactValue.DirectedInfinity { ComplexBigRational.Real = FromPrimitive.bigint r; Imaginary = FromPrimitive.bigint i }
    let fromInfinityDirectedRational (real:BigRational) (imag:BigRational) =
        match real, imag with
        | r, i when r.IsZero && i.IsZero -> ExactValue.Undefined
        | r, i when r.IsOne && i.IsZero -> ExactValue.PositiveInfinity
        | r, i when r.IsInteger && r.Numerator = BigInteger.MinusOne && i.IsZero -> ExactValue.NegativeInfinity
        | r, i -> ExactValue.DirectedInfinity { ComplexBigRational.Real = r; Imaginary = i }

    let (|Zero|_|) = function
        | ExactValue.Rational n when n.IsZero -> Some Zero
        | ExactValue.ComplexRational x when x.Real.IsZero && x.Imaginary.IsZero -> Some Zero
        | _ -> None

    let (|One|_|) = function
        | ExactValue.Rational n when n.IsOne -> Some One
        | ExactValue.ComplexRational n when n.Real.IsOne && n.Imaginary.IsZero -> Some One
        | _ -> None

    let (|MinusOne|_|) = function
        | ExactValue.Rational n when n.IsInteger && n.Numerator = BigInteger.MinusOne -> Some MinusOne
        | ExactValue.ComplexRational n when n.Real.IsInteger && n.Real.Numerator = BigInteger.MinusOne && n.Imaginary.IsZero -> Some MinusOne
        | _ -> None

    let (|Positive|_|) = function
        | ExactValue.Rational n when n.IsPositive -> Some Positive
        | ExactValue.ComplexRational n when n.Real.IsPositive && n.Imaginary.IsZero -> Some Positive
        | ExactValue.PositiveInfinity -> Some Positive
        | ExactValue.Constant E | ExactValue.Constant Pi -> Some Positive
        | _ -> None

    let (|Negative|_|) = function
        | ExactValue.Rational n when n.IsNegative -> Some Negative
        | ExactValue.ComplexRational n when n.Real.IsNegative && n.Imaginary.IsZero -> Some Negative
        | ExactValue.NegativeInfinity -> Some Negative
        | _ -> None

    let isZero = function | Zero -> true | _ -> false
    let isOne = function | One -> true | _ -> false
    let isMinusOne = function | MinusOne -> true | _ -> false
    let isPositive = function | Positive -> true | _ -> false
    let isNegative = function | Negative -> true | _ -> false

    let negate = function
        | ExactValue.Rational a -> ExactValue.Rational (-a)
        | ExactValue.ComplexRational a -> fromComplexRational (-a.Real) (-a.Imaginary)
        | ExactValue.Constant c -> fromConstantDirectedInt32 c -1 0
        | ExactValue.DirectedConstant (c, d) -> fromConstantDirectedRational c (-d.Real) (-d.Imaginary)
        | ExactValue.NegativeInfinity -> ExactValue.PositiveInfinity
        | ExactValue.PositiveInfinity -> ExactValue.NegativeInfinity
        | ExactValue.ComplexInfinity -> ExactValue.ComplexInfinity
        | ExactValue.DirectedInfinity d -> fromInfinityDirectedRational (-d.Real) (-d.Imaginary)
        | ExactValue.Undefined -> ExactValue.Undefined

    let tryAbs = function
        | Zero | Positive as x -> Some x
        | ExactValue.Rational a -> fromRational (BigRational.Abs a) |> Some
        | ExactValue.ComplexRational a ->
            match a.Real, a.Imaginary with
            | r, i when r.IsZero -> fromRational (BigRational.Abs i) |> Some
            | r, i when i.IsZero -> fromRational (BigRational.Abs r) |> Some
            | _ -> None
        | ExactValue.Constant I -> Some one
        | ExactValue.Constant _ as positiveReal -> Some positiveReal
        | ExactValue.DirectedConstant (c, d) -> None // TODO
        | ExactValue.NegativeInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.DirectedInfinity _ -> Some ExactValue.PositiveInfinity
        | ExactValue.ComplexInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAdd augend addend =
        match augend, addend with
        | ExactValue.Undefined, _ | _, ExactValue.Undefined -> Some ExactValue.Undefined
        | Zero, b | b, Zero -> Some b
        | ExactValue.Rational a, ExactValue.Rational b -> fromRational (a + b) |> Some
        | ExactValue.ComplexRational a, ExactValue.ComplexRational b -> fromComplexRational (a.Real + b.Real) (a.Imaginary + b.Imaginary) |> Some
        | ExactValue.Rational a, ExactValue.ComplexRational b | ExactValue.ComplexRational b, ExactValue.Rational a -> fromComplexRational (a + b.Real) b.Imaginary |> Some
        | ExactValue.Constant a, ExactValue.Constant b when a = b -> fromConstantDirectedInt32 a 2 0 |> Some
        | ExactValue.DirectedConstant (a,da), ExactValue.DirectedConstant (b,db) when a = b -> fromConstantDirectedRational a (da.Real + db.Real) (da.Imaginary + db.Imaginary) |> Some
        | ExactValue.Constant a, ExactValue.DirectedConstant (b, d) | ExactValue.DirectedConstant (b, d), ExactValue.Constant a when a = b -> fromConstantDirectedRational a (d.Real + BigRational.One) d.Imaginary |> Some
        | ExactValue.ComplexInfinity, (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity | ExactValue.DirectedInfinity _) -> Some ExactValue.Undefined
        | (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity | ExactValue.DirectedInfinity _),  ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.ComplexInfinity, _ | _, ExactValue.ComplexInfinity -> Some ExactValue.ComplexInfinity
        | ExactValue.PositiveInfinity, ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.PositiveInfinity, ExactValue.NegativeInfinity | ExactValue.NegativeInfinity, ExactValue.PositiveInfinity -> Some ExactValue.Undefined
        | ExactValue.PositiveInfinity, ExactValue.DirectedInfinity _ | ExactValue.DirectedInfinity _, ExactValue.PositiveInfinity -> Some ExactValue.Undefined
        | ExactValue.PositiveInfinity, _ | _, ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity, ExactValue.NegativeInfinity -> Some ExactValue.NegativeInfinity
        | ExactValue.NegativeInfinity, ExactValue.DirectedInfinity _ | ExactValue.DirectedInfinity _, ExactValue.NegativeInfinity -> Some ExactValue.Undefined
        | ExactValue.NegativeInfinity, _ | _, ExactValue.NegativeInfinity -> Some ExactValue.NegativeInfinity
        | ExactValue.DirectedInfinity a, ExactValue.DirectedInfinity b when a = b -> Some (ExactValue.DirectedInfinity a)
        | ExactValue.DirectedInfinity a, ExactValue.DirectedInfinity b -> fromInfinityDirectedRational (a.Real + b.Real) (a.Imaginary + b.Imaginary) |> Some
        | ExactValue.DirectedInfinity d, _ | _, ExactValue.DirectedInfinity d -> Some (ExactValue.DirectedInfinity d)
        | ExactValue.Constant _, _ | _, ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _, _ | _, ExactValue.DirectedConstant _ -> None

    let tryMultiply multiplier multiplicand =
        match multiplier, multiplicand with
        | ExactValue.Undefined, _ | _, ExactValue.Undefined -> Some ExactValue.Undefined
        | One, b | b, One -> Some b
        | Zero, (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity | ExactValue.DirectedInfinity _) -> Some ExactValue.Undefined
        | (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity | ExactValue.DirectedInfinity _), Zero -> Some ExactValue.Undefined
        | Zero, _ | _, Zero -> Some zero
        | ExactValue.Rational a, ExactValue.Rational b -> fromRational (a * b) |> Some
        | ExactValue.ComplexRational a, ExactValue.ComplexRational b -> fromComplexRational (a.Real * b.Real - a.Imaginary * b.Imaginary) (a.Real * b.Imaginary + a.Imaginary * b.Real) |> Some
        | ExactValue.Rational a, ExactValue.ComplexRational b | ExactValue.ComplexRational b, ExactValue.Rational a -> fromComplexRational (a * b.Real) (a * b.Imaginary) |> Some
        | ExactValue.Constant a, ExactValue.Rational b | ExactValue.Rational b, ExactValue.Constant a -> fromConstantDirectedRational a b BigRational.Zero |> Some
        | ExactValue.Constant a, ExactValue.ComplexRational b | ExactValue.ComplexRational b, ExactValue.Constant a -> fromConstantDirectedRational a b.Real b.Imaginary |> Some
        | ExactValue.DirectedConstant (a,d), ExactValue.Rational b | ExactValue.Rational b, ExactValue.DirectedConstant (a,d) -> fromConstantDirectedRational a (b * d.Real) (b * d.Imaginary) |> Some
        | ExactValue.DirectedConstant (a,d), ExactValue.ComplexRational b | ExactValue.ComplexRational b, ExactValue.DirectedConstant (a,d) -> fromConstantDirectedRational a (b.Real * d.Real - b.Imaginary * d.Imaginary) (b.Real * d.Imaginary + b.Imaginary * d.Real)|> Some
        | ExactValue.ComplexInfinity, _ | _, ExactValue.ComplexInfinity -> Some ExactValue.ComplexInfinity
        | ExactValue.PositiveInfinity, Positive | Positive, ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.PositiveInfinity, Negative | Negative, ExactValue.PositiveInfinity -> Some ExactValue.NegativeInfinity
        | ExactValue.NegativeInfinity, Positive | Positive, ExactValue.NegativeInfinity -> Some ExactValue.NegativeInfinity
        | ExactValue.NegativeInfinity, Negative | Negative, ExactValue.NegativeInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.DirectedInfinity d, Positive | Positive, ExactValue.DirectedInfinity d -> Some (ExactValue.DirectedInfinity d)
        | ExactValue.DirectedInfinity d, Negative | Negative, ExactValue.DirectedInfinity d -> fromInfinityDirectedRational (-d.Real) (-d.Imaginary) |> Some
        | ExactValue.NegativeInfinity, _ | _, ExactValue.NegativeInfinity -> Some ExactValue.NegativeInfinity
        | ExactValue.PositiveInfinity, _ | _, ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.DirectedInfinity d, _ | _, ExactValue.DirectedInfinity d -> Some (ExactValue.DirectedInfinity d)
        | ExactValue.Constant _, _ | _, ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _, _ | _, ExactValue.DirectedConstant _ -> None

    let tryInvert = function
        | Zero -> Some ExactValue.ComplexInfinity
        | ExactValue.Rational a -> fromRational (BigRational.Reciprocal a) |> Some
        | ExactValue.ComplexRational a ->
            let denominator = a.Real*a.Real + a.Imaginary*a.Imaginary
            fromComplexRational (a.Real/denominator) (-a.Imaginary/denominator) |> Some
        | ExactValue.Constant I -> Some imagMinusOne
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity | ExactValue.DirectedInfinity _ -> Some zero
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryPower exponent radix =
        match radix, exponent with
        | ExactValue.Undefined, _ | _, ExactValue.Undefined -> Some ExactValue.Undefined
        | Zero, Zero -> Some ExactValue.Undefined
        | Zero, (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity) -> Some zero
        | Zero, ExactValue.NegativeInfinity -> Some ExactValue.ComplexInfinity
        | Zero, Positive -> Some zero
        | Zero, Negative -> Some ExactValue.ComplexInfinity
        | (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity), Zero -> Some ExactValue.Undefined
        | (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity), ExactValue.PositiveInfinity -> Some ExactValue.ComplexInfinity
        | (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity), ExactValue.Rational b when b.IsNegative -> Some zero
        | ExactValue.ComplexInfinity, Positive -> Some ExactValue.ComplexInfinity
        | ExactValue.PositiveInfinity, Positive -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity, ExactValue.Rational b when b.IsPositive && b.IsInteger ->
            if (b.Numerator % 2I).IsZero then Some ExactValue.PositiveInfinity else Some ExactValue.NegativeInfinity
        | One, (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity) | MinusOne, (ExactValue.ComplexInfinity | ExactValue.PositiveInfinity | ExactValue.NegativeInfinity) -> Some ExactValue.Undefined
        | One, _ | _, Zero -> Some one
        | _, Zero -> Some one
        | a, One -> Some a
        | One, _ -> Some one
        | Positive, ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | Negative, ExactValue.PositiveInfinity -> Some ExactValue.ComplexInfinity
        | _, ExactValue.NegativeInfinity -> Some zero
        | _, ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Rational a, ExactValue.Rational b when b.IsInteger ->
            if b.IsNegative then
                if a.IsZero then Some ExactValue.ComplexInfinity
                // workaround bug in BigRational with negative powers - drop after upgrading to > v3.0.0-alpha9
                else Some (ExactValue.Rational (BigRational.Pow(BigRational.Reciprocal a, -int(b.Numerator))))
            else Some (ExactValue.Rational (BigRational.Pow(a, int(b.Numerator))))
        | ExactValue.Rational a, ExactValue.Rational b -> None // TODO
        | _ -> None // TODO

    let tryLn = function
        | Zero -> Some ExactValue.NegativeInfinity
        | One -> Some zero
        | MinusOne -> fromConstantDirectedRational Constant.Pi (0N) (1N) |> Some
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant E -> Some one
        | ExactValue.Constant I -> fromConstantDirectedRational Constant.Pi (0N) (1N/2N) |> Some
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.DirectedInfinity _ -> Some ExactValue.PositiveInfinity
        | ExactValue.ComplexInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryLog10 = function
        | Zero -> Some ExactValue.NegativeInfinity
        | One -> Some zero
        | ExactValue.Rational n when n.Equals(10N) -> Some one
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.DirectedInfinity _ -> Some ExactValue.PositiveInfinity
        | ExactValue.ComplexInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryLog b x = None

    let tryExp = function
        | Zero -> Some one
        | One -> ExactValue.Constant E |> Some
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant (Pi, ComplexBigRational.ComplexInteger (r,i)) when r.IsZero -> if i.IsEven then Some one else Some minusOne
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined


    let trySin = function
        | Zero -> Some zero
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant Pi -> Some zero
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant (Pi, ComplexBigRational.RealInteger _) -> Some zero // sin(n*pi) = 0 for integer n
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.Undefined
        | ExactValue.NegativeInfinity -> Some ExactValue.Undefined
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryCos = function
        | Zero -> Some one
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant Pi -> Some minusOne
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant (Pi, ComplexBigRational.RealInteger n) -> if n.IsEven then Some one else Some minusOne
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.Undefined
        | ExactValue.NegativeInfinity -> Some ExactValue.Undefined
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryTan = function
        | Zero -> Some zero
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant Pi -> Some zero
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant (Pi, ComplexBigRational.RealInteger _) -> Some zero // tan(n*pi) = 0 for integer n
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.Undefined
        | ExactValue.NegativeInfinity -> Some ExactValue.Undefined
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryCsc = function
        | Zero -> Some ExactValue.ComplexInfinity
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant Pi -> Some ExactValue.ComplexInfinity
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant (Pi, ComplexBigRational.RealInteger _) -> Some ExactValue.ComplexInfinity
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.Undefined
        | ExactValue.NegativeInfinity -> Some ExactValue.Undefined
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let trySec = function
        | Zero -> Some one
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant Pi -> Some minusOne
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant (Pi, ComplexBigRational.RealInteger n) -> if n.IsEven then Some one else Some minusOne
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.Undefined
        | ExactValue.NegativeInfinity -> Some ExactValue.Undefined
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryCot = function
        | Zero -> Some ExactValue.ComplexInfinity
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant Pi -> Some ExactValue.ComplexInfinity
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant (Pi, ComplexBigRational.RealInteger _) -> Some ExactValue.ComplexInfinity
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.Undefined
        | ExactValue.NegativeInfinity -> Some ExactValue.Undefined
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined


    let trySinh = function
        | Zero -> Some zero
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity -> Some ExactValue.NegativeInfinity
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryCosh = function
        | Zero -> Some one
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryTanh = function
        | Zero -> Some zero
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some one
        | ExactValue.NegativeInfinity -> Some minusOne
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryCsch = function
        | Zero -> Some ExactValue.ComplexInfinity
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some zero
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let trySech = function
        | Zero -> Some one
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some zero
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryCoth = function
        | Zero -> Some ExactValue.ComplexInfinity
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some one
        | ExactValue.NegativeInfinity -> Some minusOne
        | ExactValue.DirectedInfinity _ -> Some ExactValue.Undefined // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined


    let tryAsin = function
        | Zero -> Some zero
        | One -> fromConstantDirectedRational Constant.Pi (1N/2N) (0N) |> Some
        | MinusOne -> fromConstantDirectedRational Constant.Pi (-1N/2N) (0N) |> Some
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> fromInfinityDirectedInt32 0 -1 |> Some
        | ExactValue.NegativeInfinity -> fromInfinityDirectedInt32 0 1 |> Some
        | ExactValue.DirectedInfinity _ -> None // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.ComplexInfinity
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAcos = function
        | Zero -> fromConstantDirectedRational Constant.Pi (1N/2N) (0N) |> Some
        | One -> Some zero
        | MinusOne -> ExactValue.Constant Constant.Pi |> Some
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> fromInfinityDirectedInt32 0 1 |> Some
        | ExactValue.NegativeInfinity -> fromInfinityDirectedInt32 0 -1 |> Some
        | ExactValue.DirectedInfinity _ -> None // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.ComplexInfinity
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAtan = function
        | Zero -> Some zero
        | One -> fromConstantDirectedRational Constant.Pi (1N/4N) (0N) |> Some
        | MinusOne -> fromConstantDirectedRational Constant.Pi (-1N/2N) (0N) |> Some
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> fromConstantDirectedRational Constant.Pi (1N/2N) (0N) |> Some
        | ExactValue.NegativeInfinity -> fromConstantDirectedRational Constant.Pi (-1N/2N) (0N) |> Some
        | ExactValue.DirectedInfinity _ -> None // TODO
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAtan2 x y = None

    let tryAcsc = function
        | Zero -> Some ExactValue.ComplexInfinity
        | One -> fromConstantDirectedRational Constant.Pi (1N/2N) (0N) |> Some
        | MinusOne -> fromConstantDirectedRational Constant.Pi (-1N/2N) (0N) |> Some
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some zero
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> Some zero
        | ExactValue.ComplexInfinity -> Some zero
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAsec = function
        | Zero -> Some ExactValue.ComplexInfinity
        | One -> Some zero
        | MinusOne -> ExactValue.Constant Constant.Pi |> Some
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> fromConstantDirectedRational Constant.Pi (1N/2N) (0N) |> Some
        | ExactValue.NegativeInfinity -> fromConstantDirectedRational Constant.Pi (1N/2N) (0N) |> Some
        | ExactValue.DirectedInfinity _ -> fromConstantDirectedRational Constant.Pi (1N/2N) (0N) |> Some
        | ExactValue.ComplexInfinity -> fromConstantDirectedRational Constant.Pi (1N/2N) (0N) |> Some
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAcot = function
        | Zero -> fromConstantDirectedRational Constant.Pi (1N/2N) (0N) |> Some
        | One -> fromConstantDirectedRational Constant.Pi (1N/4N) (0N) |> Some
        | MinusOne -> fromConstantDirectedRational Constant.Pi (-1N/4N) (0N) |> Some
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some zero
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> Some zero
        | ExactValue.ComplexInfinity -> Some zero
        | ExactValue.Undefined -> Some ExactValue.Undefined


    let tryAsinh = function
        | Zero -> Some zero
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity -> Some ExactValue.NegativeInfinity
        | ExactValue.DirectedInfinity _ -> None
        | ExactValue.ComplexInfinity -> Some ExactValue.ComplexInfinity
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAcosh = function
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.DirectedInfinity _ -> None
        | ExactValue.ComplexInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAtanh = function
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> fromConstantDirectedRational Constant.Pi (0N) (-1N/2N) |> Some
        | ExactValue.NegativeInfinity -> fromConstantDirectedRational Constant.Pi (0N) (1N/2N) |> Some
        | ExactValue.DirectedInfinity _ -> None
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAcsch = function
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some zero
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> Some zero
        | ExactValue.ComplexInfinity -> Some zero
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAsech = function
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> fromConstantDirectedRational Constant.Pi (0N) (1N/2N) |> Some
        | ExactValue.NegativeInfinity -> fromConstantDirectedRational Constant.Pi (0N) (1N/2N) |> Some
        | ExactValue.DirectedInfinity _ -> None
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAcoth = function
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some zero
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> Some zero
        | ExactValue.ComplexInfinity -> Some zero
        | ExactValue.Undefined -> Some ExactValue.Undefined


    let tryAiryai = function
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some zero
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> None
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAiryaiprime = function
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some zero
        | ExactValue.NegativeInfinity _ -> None
        | ExactValue.DirectedInfinity _ -> None
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAirybi = function
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> None
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined

    let tryAirybiprime = function
        | ExactValue.Rational _ -> None
        | ExactValue.ComplexRational _ -> None
        | ExactValue.Constant _ -> None
        | ExactValue.DirectedConstant _ -> None
        | ExactValue.PositiveInfinity -> Some ExactValue.PositiveInfinity
        | ExactValue.NegativeInfinity -> Some zero
        | ExactValue.DirectedInfinity _ -> None
        | ExactValue.ComplexInfinity -> Some ExactValue.Undefined
        | ExactValue.Undefined -> Some ExactValue.Undefined


    let tryBesselj nu z = None
    let tryBessely nu z = None
    let tryBesseli nu z = None
    let tryBesselk nu z = None
    let tryBesseliratio nu z = None
    let tryBesselkratio nu z = None

    let tryHankelh1 nu z = None
    let tryHankelh2 nu z = None

    let tryApply f a =
        match f with
        | Abs -> tryAbs a
        | Ln -> tryLn a
        | Log -> tryLog10 a
        | Exp -> tryExp a
        | Sin ->trySin a
        | Cos -> tryCos a
        | Tan -> tryTan a
        | Csc -> tryCsc a
        | Sec -> trySec a
        | Cot -> tryCot a
        | Sinh -> trySinh a
        | Cosh-> tryCosh a
        | Tanh -> tryTanh a
        | Csch -> tryCsch a
        | Sech -> trySech a
        | Coth -> tryCoth a
        | Asin -> tryAsin a
        | Acos -> tryAcos a
        | Atan -> tryAtan a
        | Acsc -> tryAcsc a
        | Asec -> tryAsec a
        | Acot -> tryAcot a
        | Asinh -> tryAsinh a
        | Acosh -> tryAcosh a
        | Atanh -> tryAtanh a
        | Acsch -> tryAcsch a
        | Asech -> tryAsech a
        | Acoth -> tryAcoth a
        | AiryAi -> tryAiryai a
        | AiryAiPrime -> tryAiryaiprime a
        | AiryBi -> tryAirybi a
        | AiryBiPrime -> tryAirybiprime a
        | _ -> None

    let tryApplyN f xs =
        match f, xs with
        | Atan, [x; y] -> tryAtan2 x y
        | Log, [b; x] -> tryLog b x
        | BesselJ, [nu; x] -> tryBesselj nu x
        | BesselY, [nu; x] -> tryBessely nu x
        | BesselI, [nu; x] -> tryBesseli nu x
        | BesselK, [nu; x] -> tryBesselk nu x
        | BesselIRatio, [nu; x] -> tryBesseliratio nu x
        | BesselKRatio, [nu; x] -> tryBesselkratio nu x
        | HankelH1, [nu; x] -> tryHankelh1 nu x
        | HankelH2, [nu; x] -> tryHankelh2 nu x
        | _ -> None
