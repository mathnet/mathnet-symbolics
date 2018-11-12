namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics

// this could be extended to arbitrary/custom precision approximations in the future
type Approximation =
    | Real of float
    | Complex of Complex

    // Simpler usage in C#
    static member op_Implicit (x:float) = Real x
    static member op_Implicit (x:complex) = Complex x
    member x.RealValue =
        match x with
        | Real x -> x
        | Complex x when x.IsReal() -> x.Real
        | _ -> failwith "Value not convertible to a real number."
    member x.ComplexValue =
        match x with
        | Real x -> complex x 0.0
        | Complex x -> x


[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Approximation =

    type C = System.Numerics.Complex

    let fromRational (x:BigRational) = Real (float x)

    let negate = function
        | Real a -> Real (-a)
        | Complex a -> Complex (-a)
    let sum = function
        | Real a, Real b -> Real (a+b)
        | Complex a, Complex b -> Complex (a+b)
        | Complex a, Real b | Real b, Complex a -> Complex (a + complex b 0.0)
    let product = function
        | Real a, Real b -> Real (a*b)
        | Complex a, Complex b -> Complex (a*b)
        | Complex a, Real b | Real b, Complex a -> Complex (a * complex b 0.0)
    let pow = function
        | Real a, Real b -> Real (a**b)
        | Complex a, Complex b -> Complex (C.Pow(a,b))
        | Real a, Complex b -> Complex (C.Pow(complex a 0.0, b))
        | Complex a, Real b -> Complex (C.Pow(a, complex b 0.0))
    let invert = function
        | Real a -> Real (1.0/a)
        | Complex a -> Complex (C.Reciprocal a)

    let abs = function
        | Real a -> Real (Math.Abs a)
        | Complex a -> Real (C.Abs a)
    let ln = function
        | Real a -> Real (Math.Log a)
        | Complex a -> Complex (C.Log a)
    let log10 = function
        | Real a -> Real (Math.Log10 a)
        | Complex a -> Complex (C.Log10 a)
    let log b x =
        match b, x with
        | Real v, Real w -> Real (Math.Log (v, w))
        | Real v, Complex w -> Complex (Complex.Log (w, v))
        | _ -> failwith "not supported"
    let exp = function
        | Real a -> Real (Math.Exp a)
        | Complex a -> Complex (C.Exp a)
    let sin = function
        | Real a -> Real (Math.Sin a)
        | Complex a -> Complex (C.Sin a)
    let cos = function
        | Real a -> Real (Math.Cos a)
        | Complex a -> Complex (C.Cos a)
    let tan = function
        | Real a -> Real (Math.Tan a)
        | Complex a -> Complex (C.Tan a)
    let csc = function
        | Real a -> Real (Trig.Csc a)
        | Complex a -> Complex (Complex.csc a)
    let sec = function
        | Real a -> Real (Trig.Sec a)
        | Complex a -> Complex (Complex.sec a)
    let cot = function
        | Real a -> Real (Trig.Cot a)
        | Complex a -> Complex (Complex.cot a)
    let sinh = function
        | Real a -> Real (Trig.Sinh a)
        | Complex a -> Complex (Trig.Sinh a)
    let cosh = function
        | Real a -> Real (Trig.Cosh a)
        | Complex a -> Complex (Trig.Cosh a)
    let tanh = function
        | Real a -> Real (Trig.Tanh a)
        | Complex a -> Complex (Trig.Tanh a)
    let csch = function
        | Real a -> Real (Trig.Csch a)
        | Complex a -> Complex (Complex.csch a)
    let sech = function
        | Real a -> Real (Trig.Sech a)
        | Complex a -> Complex (Complex.sech a)
    let coth = function
        | Real a -> Real (Trig.Coth a)
        | Complex a -> Complex (Complex.coth a)
    let asin = function
        | Real a -> Real (Trig.Asin a)
        | Complex a -> Complex (Trig.Asin a)
    let acos = function
        | Real a -> Real (Trig.Acos a)
        | Complex a -> Complex (Trig.Acos a)
    let atan = function
        | Real a -> Real (Trig.Atan a)
        | Complex a -> Complex (Trig.Atan a)
    let atan2 x y =
        match x, y with
        | Real a, Real b -> Real (Math.Atan2 (a, b))
        | Complex a, Complex b -> Complex (Complex.Atan (a / b))
        | Complex a, Real b -> Complex (Complex.Atan (a / (Complex.Create (b, 0.0))))
        | Real a, Complex b -> Complex (Complex.Atan ((Complex.Create (a, 0.0)) / b))
    let acsc = function
        | Real a -> Real (Trig.Acsc a)
        | Complex a -> Complex (Trig.Acsc a)
    let asec = function
        | Real a -> Real (Trig.Asec a)
        | Complex a -> Complex (Trig.Asec a)
    let acot = function
        | Real a -> Real (Trig.Acot a)
        | Complex a -> Complex (Trig.Acot a)
    let asinh = function
        | Real a -> Real (Trig.Asinh a)
        | Complex a -> Complex (Trig.Asinh a)
    let acosh = function
        | Real a -> Real (Trig.Acosh a)
        | Complex a -> Complex (Trig.Acosh a)
    let atanh = function
        | Real a -> Real (Trig.Atanh a)
        | Complex a -> Complex (Trig.Atanh a)
    let acsch = function
        | Real a -> Real (Trig.Acsch a)
        | Complex a -> Complex (Trig.Acsch a)
    let asech = function
        | Real a -> Real (Trig.Asech a)
        | Complex a -> Complex (Trig.Asech a)
    let acoth = function
        | Real a -> Real (Trig.Acoth a)
        | Complex a -> Complex (Trig.Acoth a)

    let airyai = function
        | Real a -> Real (SpecialFunctions.AiryAi a)
        | Complex a -> Complex (SpecialFunctions.AiryAi a)
    let airyaiprime = function
        | Real a -> Real (SpecialFunctions.AiryAiPrime a)
        | Complex a -> Complex (SpecialFunctions.AiryAiPrime a)
    let airybi = function
        | Real a -> Real (SpecialFunctions.AiryBi a)
        | Complex a -> Complex (SpecialFunctions.AiryBi a)
    let airybiprime = function
        | Real a -> Real (SpecialFunctions.AiryBiPrime a)
        | Complex a -> Complex (SpecialFunctions.AiryBiPrime a)

    let besselj nu z =
        match nu, z with
        | Real a, Real b -> Real (SpecialFunctions.BesselJ (a, b));
        | Real a, Complex b -> Complex (SpecialFunctions.BesselJ (a, b));
        | Complex a, Real b -> failwith "not supported"     
        | Complex a, Complex b -> failwith "not supported"  
    let bessely nu z =
        match nu, z with
        | Real a, Real b -> Real (SpecialFunctions.BesselY (a, b));
        | Real a, Complex b -> Complex (SpecialFunctions.BesselY (a, b));
        | Complex a, Real b -> failwith "not supported" 
        | Complex a, Complex b -> failwith "not supported"
    let besseli nu z =
        match nu, z with
        | Real a, Real b -> Real (SpecialFunctions.BesselI (a, b));
        | Real a, Complex b -> Complex (SpecialFunctions.BesselI (a, b));
        | Complex a, Real b -> failwith "not supported"
        | Complex a, Complex b -> failwith "not supported"    
    let besselk nu z =
        match nu, z with
        | Real a, Real b -> Real (SpecialFunctions.BesselK (a, b));
        | Real a, Complex b -> Complex (SpecialFunctions.BesselK (a, b));
        | Complex a, Real b -> failwith "not supported"  
        | Complex a, Complex b -> failwith "not supported"
    let besseliratio nu z =
        match nu, z with
        | Real a, Real b -> Real (SpecialFunctions.BesselIScaled (a + 1.0, b) / SpecialFunctions.BesselIScaled (a, b));
        | Real a, Complex b -> Complex (SpecialFunctions.BesselIScaled (a + 1.0, b) / SpecialFunctions.BesselIScaled (a, b));
        | Complex a, Real b -> failwith "not supported"
        | Complex a, Complex b -> failwith "not supported"
    let besselkratio nu z =
        match nu, z with
        | Real a, Real b -> Real (SpecialFunctions.BesselKScaled (a + 1.0, b) / SpecialFunctions.BesselKScaled (a, b));
        | Real a, Complex b -> Complex (SpecialFunctions.BesselKScaled (a + 1.0, b) / SpecialFunctions.BesselKScaled (a, b));
        | Complex a, Real b -> failwith "not supported"
        | Complex a, Complex b -> failwith "not supported"
    let hankelh1 nu z =
        match nu, z with
        | Real a, Real b -> Complex (SpecialFunctions.HankelH1 (a, complex b 0.0));
        | Real a, Complex b -> Complex (SpecialFunctions.HankelH1 (a, b));
        | Complex a, Real b -> failwith "not supported"
        | Complex a, Complex b -> failwith "not supported"
    let hankelh2 nu z =
        match nu, z with
        | Real a, Real b -> Complex (SpecialFunctions.HankelH2 (a, complex b 0.0));
        | Real a, Complex b -> Complex (SpecialFunctions.HankelH2 (a, b));
        | Complex a, Real b -> failwith "not supported"
        | Complex a, Complex b -> failwith "not supported"

    let apply f a =
        match f with
        | Abs -> abs a
        | Ln -> ln a
        | Log -> log10 a
        | Exp -> exp a
        | Sin ->sin a
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
        | _ -> failwith "not supported"

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
        | _ -> failwith "not supported"

    let isZero = function
        | Real x when x = 0.0 -> true
        | Complex c when c.IsZero() -> true
        | _ -> false
    let isOne = function
        | Real x when x = 1.0 -> true
        | Complex c when c = C.One -> true
        | _ -> false
    let isMinusOne = function
        | Real x when x = -1.0 -> true
        | Complex c when c.IsReal() && c.Real = -1.0 -> true
        | _ -> false
    let isPositive = function
        | Real x when x > 0.0 -> true
        | Complex c when c.IsReal() && c.Real > 0.0 -> true
        | _ -> false
    let isNegative = function
        | Real x when x < 0.0 -> true
        | Complex c when c.IsReal() && c.Real < 0.0 -> true
        | _ -> false

    let internal orderRelation (x:Approximation) (y:Approximation) =
        match x, y with
        | (Real x), (Real y) -> x < y
        | (Complex x), (Complex y) -> x.Real < y.Real || x.Real = y.Real && x.Imaginary < y.Imaginary
        | (Real x), (Complex y) -> not (y.IsReal()) || x < y.Real
        | (Complex x), (Real y) -> x.IsReal() && x.Real < y

    /// Sort approximations in a list with standard expression ordering.
    [<CompiledName("SortList")>]
    let sortList list =
        List.sortWith (fun a b -> if a = b then 0 elif orderRelation a b then -1 else 1) list
