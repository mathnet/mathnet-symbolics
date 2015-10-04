namespace MathNet.Symbolics

open System
open System.Numerics
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
    static member op_Implicit (x:complex) = Complex x
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

    open System.Collections.Generic
    type C = System.Numerics.Complex

    let (|Infinity|_|) = function
        | PosInf | NegInf | ComplexInf -> Some Infinity
        | _ -> None

    [<CompiledName("Real")>]
    let freal x = FloatingPoint.Real(x)

    [<CompiledName("Complex")>]
    let fcomplex r i = FloatingPoint.Complex (System.Numerics.Complex(r, i))

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
        | Real x, Complex y | Complex y, Real x -> Complex (C(x,0.0)+y)
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
        | Real x, Complex y | Complex y, Real x -> Complex (C(x,0.0)*y)
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
        | Real x, Real y -> Real (Math.Pow(x, y))
        | Complex x, Real y -> Complex (Complex.Pow(x, y))
        | Real x, Complex y -> Complex (Complex.Pow(C(x,0.0), y))
        | Complex x, Complex y -> Complex (Complex.Pow(x, y))
        | Undef, _ | _, Undef -> Undef
        | ComplexInf, Infinity | Infinity, ComplexInf -> ComplexInf
        | Infinity, PosInf -> ComplexInf
        | Infinity, NegInf -> Real (0.0)
        | _ -> failwith "not supported"

    let fapply f u =
        match f, u with
        | Abs, Real x -> Real (Math.Abs(x))
        | Abs, Complex x -> Real (Complex.Abs(x))
        | Abs, RealVector x -> Real (x.L2Norm())
        | Abs, ComplexVector x -> Real (x.L2Norm())
        | Abs, RealMatrix x -> Real (x.L2Norm())
        | Abs, ComplexMatrix x -> Real (x.L2Norm())
        | Ln, Real x -> Real (Math.Log(x))
        | Ln, Complex x -> Complex (Complex.Log(x))
        | Exp, Real x -> Real (Math.Exp(x))
        | Exp, Complex x -> Complex (Complex.Exp(x))
        | Sin, Real x -> Real (Math.Sin(x))
        | Sin, Complex x -> Complex (Complex.Sin(x))
        | Cos, Real x -> Real (Math.Cos(x))
        | Cos, Complex x -> Complex (Complex.Cos(x))
        | Tan, Real x -> Real (Math.Tan(x))
        | Tan, Complex x -> Complex (Complex.Tan(x))
        | _ -> failwith "not supported"

    let fapplyN f xs = failwith "not supported"

    [<CompiledName("Evaluate")>]
    let rec evaluate (symbols:IDictionary<string, FloatingPoint>) = function
        | Number n -> Real (float n) |> fnormalize
        | Undefined -> Undef
        | Constant PositiveInfinity -> PosInf
        | Constant NegativeInfinity -> NegInf
        | Constant ComplexInfinity -> ComplexInf
        | Constant E -> Real (Constants.E)
        | Constant Pi -> Real (Constants.Pi)
        | Constant I -> Complex (Complex.ImaginaryOne)
        | Constant (Constant.Real fp) -> Real fp
        | Identifier (Symbol s) -> symbols.[s] |> fnormalize
        | Sum xs -> xs |> List.map (evaluate symbols) |> List.reduce fadd |> fnormalize
        | Product xs -> xs |> List.map (evaluate symbols) |> List.reduce fmultiply |> fnormalize
        | Power (r, p) -> fpower (evaluate symbols r) (evaluate symbols p) |> fnormalize
        | Function (f, x) -> fapply f (evaluate symbols x) |> fnormalize
        | FunctionN (f, xs) -> xs |> List.map (evaluate symbols) |> fapplyN f |> fnormalize
