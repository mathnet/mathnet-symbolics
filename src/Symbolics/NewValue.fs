namespace MathNet.Symbolics

open System
open MathNet.Numerics
open MathNet.Symbolics


[<RequireQualifiedAccess>]
type NewValue =
    | Exact of ExactValue
    | Approx of ApproxValue


[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NewValue =

    let fromInt32 (x:int) = NewValue.Exact (ExactValue.fromInt32 x)
    let fromInt64 (x:int64) = NewValue.Exact (ExactValue.fromInt64 x)
    let fromInteger (x:BigInteger) = NewValue.Exact (ExactValue.fromInteger x)
    let fromIntegerFraction (n:BigInteger) (d:BigInteger) = NewValue.Exact (ExactValue.fromIntegerFraction n d)
    let fromRational (x:BigRational) = NewValue.Exact (ExactValue.fromRational x)
    let fromComplexInt32 (real:int) (imag:int) = NewValue.Exact (ExactValue.fromComplexInt32 real imag)
    let fromComplexInt64 (real:int64) (imag:int64) = NewValue.Exact (ExactValue.fromComplexInt64 real imag)
    let fromComplexInteger (real:BigInteger) (imag:BigInteger) = NewValue.Exact (ExactValue.fromComplexInteger real imag)
    let fromComplexRational (real:BigRational) (imag:BigRational) = NewValue.Exact (ExactValue.fromComplexRational real imag)

    let fromReal (x:float) = NewValue.Approx (ApproxValue.fromReal x)
    let fromReal32 (x:float32) = NewValue.Approx (ApproxValue.fromReal32 x)
    let fromComplex (x:complex) = NewValue.Approx (ApproxValue.fromComplex x)
    let fromComplex32 (x:complex32) = NewValue.Approx (ApproxValue.fromComplex32 x)

    let fromConstant (c:Constant) = NewValue.Exact (ExactValue.fromConstant c)

    let fromExact (x:ExactValue) = NewValue.Exact x
    let fromApprox (x:ApproxValue) = NewValue.Approx x

    let zero = NewValue.Exact ExactValue.zero
    let one = NewValue.Exact ExactValue.one
    let minusOne = NewValue.Exact ExactValue.minusOne

    let isApprox = function | NewValue.Exact _ -> false | NewValue.Approx _ -> true
    let isExact = function | NewValue.Exact _ -> true | NewValue.Approx _ -> false

    let resolveConstant = function
        | E -> ApproxValue.Real Constants.E
        | Pi -> ApproxValue.Real Constants.Pi
        | I -> ApproxValue.Complex Complex.onei

    let approximate = function
        | ExactValue.Rational a -> ApproxValue.fromRational a
        | ExactValue.ComplexRational a -> ApproxValue.fromComplexRational a.Real a.Imaginary
        | ExactValue.Constant c -> resolveConstant c
        | ExactValue.DirectedConstant (c, ComplexBigRational.RealRational r) ->
            let fr = float r
            match resolveConstant c with
            | ApproxValue.Real x -> fr * x |> ApproxValue.fromReal
            | ApproxValue.Complex x -> complex (fr * x.Real) (fr * x.Imaginary) |> ApproxValue.fromComplex
        | ExactValue.DirectedConstant (c, ComplexBigRational.Complex (r,i)) ->
            let fr = float r
            let fi = float i
            match resolveConstant c with
            | ApproxValue.Real x -> complex (fr * x) (fi * x) |> ApproxValue.fromComplex
            | ApproxValue.Complex x -> complex (fr * x.Real - fi * x.Imaginary) (fr * x.Imaginary + fi * x.Real) |> ApproxValue.fromComplex
        | ExactValue.NegativeInfinity -> ApproxValue.NegativeInfinity
        | ExactValue.PositiveInfinity -> ApproxValue.PositiveInfinity
        | ExactValue.DirectedInfinity d -> ApproxValue.DirectedInfinity (complex (float d.Real) (float d.Imaginary))
        | ExactValue.ComplexInfinity -> ApproxValue.ComplexInfinity
        | ExactValue.Undefined -> ApproxValue.fromReal System.Double.NaN

    let inline private mapUnary exact approx = function
        | NewValue.Exact x -> exact x |> fromExact
        | NewValue.Approx x -> approx x |> fromApprox

    let inline private mapUnaryTry tryExact approx = function
        | NewValue.Exact x -> tryExact x |> Option.map fromExact |> Option.defaultWith (fun () -> approximate x |> approx |> fromApprox)
        | NewValue.Approx x -> approx x |> fromApprox

    let inline private mapBinaryTry tryExact approx x y =
        match x, y with
        | NewValue.Exact x, NewValue.Exact y -> tryExact x y |> Option.map fromExact |> Option.defaultWith (fun () -> approx (approximate x) (approximate y) |> fromApprox)
        | NewValue.Approx x, NewValue.Approx y -> approx x y |> fromApprox
        | NewValue.Exact x, NewValue.Approx y -> approx (approximate x) y |> fromApprox
        | NewValue.Approx x, NewValue.Exact y ->approx x (approximate y) |> fromApprox

    let inline private mapBinaryTupleTry tryExact approx = function
        | NewValue.Exact a, NewValue.Exact b -> tryExact (a, b) |> Option.map fromExact |> Option.defaultWith (fun () -> approx (approximate a, approximate b) |> fromApprox)
        | NewValue.Approx a, NewValue.Approx b -> approx (a, b) |> fromApprox
        | NewValue.Exact a, NewValue.Approx b -> approx (approximate a, b) |> fromApprox
        | NewValue.Approx a, NewValue.Exact b -> approx (a, approximate b) |> fromApprox

    let private mapNaryListTry tryExact approx args =
        let asApprox args = args |> List.map (function | NewValue.Exact x -> approximate x | NewValue.Approx x -> x)
        let asExact args = args |> List.map (function | NewValue.Exact x -> x | NewValue.Approx x -> failwith "unexpected approx value")
        if List.exists isApprox args then
            approx (asApprox args) |> fromApprox
        else
            tryExact (asExact args) |> Option.map fromExact |> Option.defaultWith (fun () -> approx (asApprox args) |> fromApprox)

    let abs x = mapUnaryTry ExactValue.tryAbs ApproxValue.abs x
    let negate x = mapUnary ExactValue.negate ApproxValue.negate x
    let add augend addend = mapBinaryTry ExactValue.tryAdd ApproxValue.add augend addend
    let subtract minuend subtrahend = add minuend (negate subtrahend)

    let multiply multiplier multiplicand = mapBinaryTry ExactValue.tryMultiply ApproxValue.multiply multiplier multiplicand
    let invert x = mapUnaryTry ExactValue.tryInvert ApproxValue.invert x
    let divide dividend divisor = multiply dividend (invert divisor)
    let power exponent radix = mapBinaryTry ExactValue.tryPower ApproxValue.power exponent radix

    let ln x = mapUnaryTry ExactValue.tryLn ApproxValue.ln x
    let log10 x = mapUnaryTry ExactValue.tryLog10 ApproxValue.log10 x
    let log b x = mapBinaryTry ExactValue.tryLog ApproxValue.log b x
    let exp x = mapUnaryTry ExactValue.tryExp ApproxValue.exp x

    let sin x = mapUnaryTry ExactValue.trySin ApproxValue.sin x
    let cos x = mapUnaryTry ExactValue.tryCos ApproxValue.cos x
    let tan x = mapUnaryTry ExactValue.tryTan ApproxValue.tan x
    let csc x = mapUnaryTry ExactValue.tryCsc ApproxValue.csc x
    let sec x = mapUnaryTry ExactValue.trySec ApproxValue.sec x
    let cot x = mapUnaryTry ExactValue.tryCot ApproxValue.cot x

    let sinh x = mapUnaryTry ExactValue.trySinh ApproxValue.sinh x
    let cosh x = mapUnaryTry ExactValue.tryCosh ApproxValue.cosh x
    let tanh x = mapUnaryTry ExactValue.tryTanh ApproxValue.tanh x
    let csch x = mapUnaryTry ExactValue.tryCsch ApproxValue.csch x
    let sech x = mapUnaryTry ExactValue.trySech ApproxValue.sech x
    let coth x = mapUnaryTry ExactValue.tryCoth ApproxValue.coth x

    let asin x = mapUnaryTry ExactValue.tryAsin ApproxValue.asin x
    let acos x = mapUnaryTry ExactValue.tryAcos ApproxValue.acos x
    let atan x = mapUnaryTry ExactValue.tryAtan ApproxValue.atan x
    let atan2 x y = mapBinaryTry ExactValue.tryAtan2 ApproxValue.atan2 x y
    let acsc x = mapUnaryTry ExactValue.tryAcsc ApproxValue.acsc x
    let asec x = mapUnaryTry ExactValue.tryAsec ApproxValue.asec x
    let acot x = mapUnaryTry ExactValue.tryAcot ApproxValue.acot x

    let asinh x = mapUnaryTry ExactValue.tryAsinh ApproxValue.asinh x
    let acosh x = mapUnaryTry ExactValue.tryAcosh ApproxValue.acosh x
    let atanh x = mapUnaryTry ExactValue.tryAtanh ApproxValue.atanh x
    let acsch x = mapUnaryTry ExactValue.tryAcsch ApproxValue.acsch x
    let asech x = mapUnaryTry ExactValue.tryAsech ApproxValue.asech x
    let acoth x = mapUnaryTry ExactValue.tryAcoth ApproxValue.acoth x

    let airyai x = mapUnaryTry ExactValue.tryAiryai ApproxValue.airyai x
    let airyaiprime x = mapUnaryTry ExactValue.tryAiryaiprime ApproxValue.airyaiprime x
    let airybi x = mapUnaryTry ExactValue.tryAirybi ApproxValue.airybi x
    let airybiprime x = mapUnaryTry ExactValue.tryAirybiprime ApproxValue.airybiprime x

    let besselj nu z = mapBinaryTry ExactValue.tryBesselj ApproxValue.besselj nu z
    let bessely nu z = mapBinaryTry ExactValue.tryBessely ApproxValue.bessely nu z
    let besseli nu z = mapBinaryTry ExactValue.tryBesseli ApproxValue.besseli nu z
    let besselk nu z = mapBinaryTry ExactValue.tryBesselk ApproxValue.besselk nu z
    let besseliratio nu z = mapBinaryTry ExactValue.tryBesseliratio ApproxValue.besseliratio nu z
    let besselkratio nu z = mapBinaryTry ExactValue.tryBesselkratio ApproxValue.besselkratio nu z

    let hankelh1 nu z = mapBinaryTry ExactValue.tryHankelh1 ApproxValue.hankelh1 nu z
    let hankelh2 nu z = mapBinaryTry ExactValue.tryHankelh2 ApproxValue.hankelh2 nu z

    let apply f x = mapUnaryTry (ExactValue.tryApply f) (ApproxValue.apply f) x
    let applyN f xs = mapNaryListTry (ExactValue.tryApplyN f) (ApproxValue.applyN f) xs


//type NewValue with

    //static member op_Implicit (x:int) = NewValue.fromInt32 x
    //static member op_Implicit (x:int64) = NewValue.fromInt64 x
    //static member op_Implicit (x:BigInteger) = NewValue.fromInteger x
    //static member op_Implicit (x:BigRational) = NewValue.fromRational x
    //static member op_Implicit (x:float) = NewValue.fromReal x
    //static member op_Implicit (x:float32) = NewValue.fromReal32 x
    //static member op_Implicit (x:complex) = NewValue.fromComplex x
    //static member op_Implicit (x:complex32) = NewValue.fromComplex32 x

