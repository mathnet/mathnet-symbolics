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
    let sinh = function
        | Real a -> Real (Math.Sinh a)
        | Complex a -> Complex (C.Sinh a)
    let cosh = function
        | Real a -> Real (Math.Cosh a)
        | Complex a -> Complex (C.Cosh a)
    let tanh = function
        | Real a -> Real (Math.Tanh a)
        | Complex a -> Complex (C.Tanh a)
    let asin = function
        | Real a -> Real (Math.Asin a)
        | Complex a -> Complex (C.Asin a)
    let acos = function
        | Real a -> Real (Math.Acos a)
        | Complex a -> Complex (C.Acos a)
    let atan = function
        | Real a -> Real (Math.Atan a)
        | Complex a -> Complex (C.Atan a)

    let apply (f:Function) a =
        match f with
        | Abs -> abs a
        | Ln -> ln a
        | Exp -> exp a
        | Sin ->sin a
        | Cos -> cos a
        | Tan -> tan a
        | Cosh-> cosh a
        | Sinh -> sinh a
        | Tanh -> tanh a
        | Asin -> asin a
        | Acos -> acos a
        | Atan -> atan a

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
