namespace MathNet.Symbolics

open System
open System.Collections.Generic
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Symbolics

[<NoComparison>]
type FloatingPoint =
    | Real of float
    | Complex of complex
    | RealVector of Vector<float>
    | ComplexVector of Vector<complex>
    | RealMatrix of Matrix<float>
    | ComplexMatrix of Matrix<complex>
    | Undef
    | PosInf
    | NegInf
    | ComplexInf

    // Simpler usage in C#
    static member op_Implicit (x:float) = Real x
    static member op_Implicit (x:float32) = Real (float x)
    static member op_Implicit (x:complex) = Complex x
    static member op_Implicit (x:complex32) = Complex (Primitive.complex x)
    static member op_Implicit (x:Vector<float>) = RealVector x
    static member op_Implicit (x:Vector<complex>) = ComplexVector x
    static member op_Implicit (x:Matrix<float>) = RealMatrix x
    static member op_Implicit (x:Matrix<complex>) = ComplexMatrix x
    member x.RealValue =
        match x with
        | Real x -> x
        | Complex x when x.IsReal() -> x.Real
        | _ -> failwith "Value not convertible to a real number."
    member x.ComplexValue =
        match x with
        | Real x -> complex x 0.0
        | Complex x -> x
        | _ -> failwith "Value not convertible to a complex number."
    member x.RealVectorValue =
        match x with
        | RealVector x -> x
        | _ -> failwith "Value not convertible to a real vector."
    member x.ComplexVectorValue =
        match x with
        | ComplexVector x -> x
        | _ -> failwith "Value not convertible to a complex vector."
    member x.RealMatrixValue =
        match x with
        | RealMatrix x -> x
        | _ -> failwith "Value not convertible to a real matrix."
    member x.ComplexMatrixValue =
        match x with
        | ComplexMatrix x -> x
        | _ -> failwith "Value not convertible to a complex matrix."


module Evaluate =

    let (|Infinity|_|) = function
        | PosInf | NegInf | ComplexInf -> Some Infinity
        | _ -> None

    [<CompiledName("Real")>]
    let freal x = FloatingPoint.Real(x)

    [<CompiledName("Complex")>]
    let fcomplex r i = FloatingPoint.Complex (complex r i)

    let rec fnormalize = function
        | Real x when Double.IsPositiveInfinity(x) -> PosInf
        | Real x when Double.IsNegativeInfinity(x) -> NegInf
        | Real x when Double.IsInfinity(x) -> ComplexInf // not supported by double?
        | Real x when Double.IsNaN(x) -> Undef
        | Complex x when x.IsInfinity() && x.IsReal() -> if x.Real > 0.0 then PosInf else NegInf
        | Complex x when x.IsInfinity() -> ComplexInf
        | Complex x when x.IsReal() -> fnormalize (Real x.Real)
        | Complex x when x.IsNaN() -> Undef
        | x -> x

    let fadd u v =
        match u, v with
        | Real x, Real y -> Real (x+y)
        | Real x, Complex y | Complex y, Real x -> Complex ((complex x 0.0) + y)
        | Complex x, Complex y -> Complex (x+y)
        | RealVector x, RealVector y -> RealVector (x+y)
        | ComplexVector x, ComplexVector y -> ComplexVector (x+y)
        | RealMatrix x, RealMatrix y -> RealMatrix (x+y)
        | ComplexMatrix x, ComplexMatrix y -> ComplexMatrix (x+y)
        | Undef, _ | _, Undef -> Undef
        | ComplexInf, Infinity | Infinity, ComplexInf -> ComplexInf
        | PosInf, NegInf -> Undef
        | PosInf, _ | _, PosInf -> PosInf
        | NegInf, _ | _, NegInf -> NegInf
        | _ -> failwith "not supported"

    let fmultiply u v =
        match u, v with
        | Real x, Real y -> Real (x*y)
        | Real x, Complex y | Complex y, Real x -> Complex ((complex x 0.0) * y)
        | Complex x, Complex y -> Complex (x*y)
        | RealVector x, RealVector y -> Real (x*y)
        | ComplexVector x, ComplexVector y -> Complex (x*y)
        | RealMatrix x, RealMatrix y -> RealMatrix (x*y)
        | ComplexMatrix x, ComplexMatrix y -> ComplexMatrix (x*y)
        | Undef, _ | _, Undef -> Undef
        | ComplexInf, Infinity | Infinity, ComplexInf -> ComplexInf
        | PosInf, NegInf -> NegInf
        | PosInf, Real x | Real x, PosInf ->
            if x < 0.0 then NegInf else if x > 0.0 then PosInf else Undef
        | NegInf, Real x | Real x, NegInf ->
            if x < 0.0 then PosInf else if x > 0.0 then NegInf else Undef
        | PosInf, _ | _, PosInf -> PosInf
        | NegInf, _ | _, NegInf -> NegInf
        | _ -> failwith "not supported"

    let fpower u v =
        match u, v with
        | Real x, Real y when x < 0.0 && (y%1.0 <> 0.0) -> Complex (Complex.pow (complex y 0.0) (complex x 0.0))
        | Real x, Real y -> Real (Math.Pow(x, y))
        | Complex x, Real y -> Complex (Complex.pow (complex y 0.0) x)
        | Real x, Complex y -> Complex (Complex.pow y (complex x 0.0))
        | Complex x, Complex y -> Complex (Complex.pow y x)
        | Undef, _ | _, Undef -> Undef
        | ComplexInf, Infinity | Infinity, ComplexInf -> ComplexInf
        | Infinity, PosInf -> ComplexInf
        | Infinity, NegInf -> Real (0.0)
        | _ -> failwith "not supported"

    let fapply f u =
        match f, u with
        | Abs, Real x -> Real (Math.Abs x)
        | Abs, Complex x -> Real (Complex.magnitude x)
        | Abs, RealVector x -> Real (x.L2Norm())
        | Abs, ComplexVector x -> Real (x.L2Norm())
        | Abs, RealMatrix x -> Real (x.L2Norm())
        | Abs, ComplexMatrix x -> Real (x.L2Norm())
        | Ln, Real x -> Real (Math.Log(x))
        | Ln, Complex x -> Complex (Complex.ln x)
        | Lg, Real x -> Real (Math.Log10 x)
        | Lg, Complex x -> Complex (Complex.log10 x)
        | Exp, Real x -> Real (Math.Exp x)
        | Exp, Complex x -> Complex (Complex.exp x)
        | Sin, Real x -> Real (Math.Sin x)
        | Sin, Complex x -> Complex (Complex.sin x)
        | Cos, Real x -> Real (Math.Cos x)
        | Cos, Complex x -> Complex (Complex.cos x)
        | Tan, Real x -> Real (Math.Tan x)
        | Tan, Complex x -> Complex (Complex.tan x)
        | Csc, Real x -> Real (Trig.Csc x)
        | Csc, Complex x -> Complex (Trig.Csc x)
        | Sec, Real x -> Real (Trig.Sec x)
        | Sec, Complex x -> Complex (Trig.Sec x)
        | Cot, Real x -> Real (Trig.Cot x)
        | Cot, Complex x -> Complex (Trig.Cot x)
        | Sinh, Real x -> Real (Trig.Sinh(x))
        | Sinh, Complex x -> Complex (Trig.Sinh(x))
        | Cosh, Real x -> Real(Trig.Cosh(x))
        | Cosh, Complex x -> Complex (Trig.Cosh(x))
        | Tanh, Real x -> Real (Trig.Tanh(x))
        | Tanh, Complex x -> Complex (Trig.Tanh(x))
        | Csch, Real x -> Real (Trig.Csch(x))
        | Csch, Complex x -> Complex (Trig.Csch(x))
        | Sech, Real x -> Real(Trig.Sech(x))
        | Sech, Complex x -> Complex (Trig.Sech(x))
        | Coth, Real x -> Real (Trig.Coth(x))
        | Coth, Complex x -> Complex (Trig.Coth(x))
        | Asin, Real x -> Real (Trig.Asin(x))
        | Asin, Complex x -> Complex (Trig.Asin(x))
        | Acos, Real x -> Real (Trig.Acos(x))
        | Acos, Complex x -> Complex (Trig.Acos(x))
        | Atan, Real x -> Real (Trig.Atan(x))
        | Atan, Complex x -> Complex (Trig.Atan(x))
        | Acsc, Real x -> Real (Trig.Acsc(x))
        | Acsc, Complex x -> Complex (Trig.Acsc(x))
        | Asec, Real x -> Real (Trig.Asec(x))
        | Asec, Complex x -> Complex (Trig.Asec(x))
        | Acot, Real x -> Real (Trig.Acot(x))
        | Acot, Complex x -> Complex (Trig.Acot(x))
        | Asinh, Real x -> Real (Trig.Asinh(x))
        | Asinh, Complex x -> Complex (Trig.Asinh(x))
        | Acosh, Real x -> Real (Trig.Acosh(x))
        | Acosh, Complex x -> Complex (Trig.Acosh(x))
        | Atanh, Real x -> Real (Trig.Atanh(x))
        | Atanh, Complex x -> Complex (Trig.Atanh(x))
        | Acsch, Real x -> Real (Trig.Acsch(x))
        | Acsch, Complex x -> Complex (Trig.Acsch(x))
        | Asech, Real x -> Real (Trig.Asech(1.0/x))
        | Asech, Complex x -> Complex (Trig.Asech(x))
        | Acoth, Real x -> Real (Trig.Acoth(x))
        | Acoth, Complex x -> Complex (Trig.Acoth(x))
        | AiryAi, Real x -> Real (SpecialFunctions.AiryAi(x))
        | AiryAi, Complex x -> Complex (SpecialFunctions.AiryAi(x))
        | AiryAiPrime, Real x -> Real (SpecialFunctions.AiryAiPrime(x))
        | AiryAiPrime, Complex x -> Complex (SpecialFunctions.AiryAiPrime(x))
        | AiryBi, Real x -> Real (SpecialFunctions.AiryBi(x))
        | AiryBi, Complex x -> Complex (SpecialFunctions.AiryBi(x))
        | AiryBiPrime, Real x -> Real (SpecialFunctions.AiryBiPrime(x))
        | AiryBiPrime, Complex x -> Complex (SpecialFunctions.AiryBiPrime(x))
        | _ -> failwith "not supported"

    let fapplyN f xs =
        match f, xs with
        | Atan2, [Real x; Real y] -> Real (Math.Atan2(x, y))
        | Atan2, [Complex x; Real y] -> Complex (Complex.atan (x / (complex y 0.0)))
        | Atan2, [Complex x; Complex y] -> Complex (Complex.atan (x / y))
        | Atan2, [Real x; Complex y] -> Complex (Complex.atan ((complex x 0.0) / y))
        | Log, [Real b; Real x] -> Real (Math.Log(x, b))
        | Log, [Real b; Complex x] -> Complex (Complex.log b x)
        | Log, [Complex b; Complex x] -> Complex (Complex.ln x / Complex.ln b)
        | Log, [Complex b; Real x] -> Complex (Complex.ln (complex x 0.0) / Complex.ln b)
        | BesselJ, [Real nu; Real x] -> Real (SpecialFunctions.BesselJ (nu, x))
        | BesselJ, [Real nu; Complex x] -> Complex (SpecialFunctions.BesselJ (nu, x))
        | BesselY, [Real nu; Real x] -> Real (SpecialFunctions.BesselY (nu, x))
        | BesselY, [Real nu; Complex x] -> Complex (SpecialFunctions.BesselY (nu, x))
        | BesselI, [Real nu; Real x] -> Real (SpecialFunctions.BesselI (nu, x))
        | BesselI, [Real nu; Complex x] -> Complex (SpecialFunctions.BesselI (nu, x))
        | BesselK, [Real nu; Real x] -> Real (SpecialFunctions.BesselK (nu, x))
        | BesselK, [Real nu; Complex x] -> Complex (SpecialFunctions.BesselK (nu, x))
        | BesselIRatio, [Real nu; Real x] -> Real (SpecialFunctions.BesselIScaled (nu + 1.0, x) / SpecialFunctions.BesselIScaled (nu, x))
        | BesselIRatio, [Real nu; Complex x] -> Complex (SpecialFunctions.BesselIScaled (nu + 1.0, x) / SpecialFunctions.BesselIScaled (nu, x))
        | BesselKRatio, [Real nu; Real x] -> Real (SpecialFunctions.BesselKScaled (nu + 1.0, x) / SpecialFunctions.BesselKScaled (nu, x))
        | BesselKRatio, [Real nu; Complex x] -> Complex (SpecialFunctions.BesselKScaled (nu + 1.0, x) / SpecialFunctions.BesselKScaled (nu, x))
        | HankelH1, [Real nu; Real x] -> Complex (SpecialFunctions.HankelH1 (nu, complex x 0.0))
        | HankelH1, [Real nu; Complex x] -> Complex (SpecialFunctions.HankelH1 (nu, x))
        | HankelH2, [Real nu; Real x] -> Complex (SpecialFunctions.HankelH2 (nu, complex x 0.0))
        | HankelH2, [Real nu; Complex x] -> Complex (SpecialFunctions.HankelH2 (nu, x))
        | _ -> failwith "not supported"

    

    [<CompiledName("Evaluate")>]
    let rec evaluate (symbols:IDictionary<string, FloatingPoint>) = function
        | Number n -> Real (float n) |> fnormalize
        | Undefined -> Undef
        | ComplexInfinity -> ComplexInf
        | PositiveInfinity -> PosInf
        | NegativeInfinity -> NegInf
        | Constant E -> Real (Constants.E)
        | Constant Pi -> Real (Constants.Pi)
        | Constant I -> Complex (Complex.onei)
        | Approximation (Approximation.Real fp) -> Real fp
        | Approximation (Approximation.Complex fp) -> Complex fp
        | Identifier (Symbol s) ->
            match symbols.TryGetValue s with
            | true, a -> a |> fnormalize
            | _ -> failwithf  "Failed to find symbol %s" s
        | Argument (Symbol s) -> failwithf  "Cannot evaluate a argument %s" s
        | Sum xs -> xs |> List.map (evaluate symbols) |> List.reduce fadd |> fnormalize
        | Product xs -> xs |> List.map (evaluate symbols) |> List.reduce fmultiply |> fnormalize
        | Power (r, p) -> fpower (evaluate symbols r) (evaluate symbols p) |> fnormalize
        | Function (f, x) -> fapply f (evaluate symbols x) |> fnormalize
        | FunctionN (f, xs) -> xs |> List.map (evaluate symbols) |> fapplyN f |> fnormalize
        | FunInvocation (Symbol fnm, xs) ->
            let param, fx = Definition.funDict.[fnm]
            let cmpl = Compile.compileExpressionOrThrow fx param

            let param_val =
                xs
                |> List.map (evaluate symbols)
                |> List.map (fun pv ->
                    match pv with
                    | (FloatingPoint.Real v) -> box v
                    | _ -> null
                )
                |> Array.ofList
            cmpl.DynamicInvoke(param_val:obj[]) :?> float |> Real
