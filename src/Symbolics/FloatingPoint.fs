namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Symbolics

type FloatingPoint =
    | Real of float
    | Complex of Complex
    | RealVector of Vector<float>
    | ComplexVector of Vector<Complex>
    | RealMatrix of Matrix<float>
    | ComplexMatrix of Matrix<Complex>
    | NaN
    | PositiveInfinity
    | NegativeInfinity
    | ComplexInfinity

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FloatingPoint =

    type C = System.Numerics.Complex

    let (|Infinity|_|) = function
        | PositiveInfinity | NegativeInfinity | ComplexInfinity -> Some Infinity
        | _ -> None

    let rec normalize = function
        | Real x when Double.IsPositiveInfinity(x) -> PositiveInfinity
        | Real x when Double.IsNegativeInfinity(x) -> NegativeInfinity
        | Real x when Double.IsInfinity(x) -> ComplexInfinity // not supported by double?
        | Real x when Double.IsNaN(x) -> NaN
        | Complex x when x.IsReal() -> normalize (Real x.Real)
        | Complex x when x.IsInfinity() -> ComplexInfinity
        | Complex x when x.IsNaN() -> NaN
        | x -> x

    let add u v =
        match u, v with
        | Real x, Real y -> Real (x+y)
        | Real x, Complex y | Complex y, Real x -> Complex (C(x,0.0)+y)
        | Complex x, Complex y -> Complex (x+y)
        | RealVector x, RealVector y -> RealVector (x+y)
        | ComplexVector x, ComplexVector y -> ComplexVector (x+y)
        | RealMatrix x, RealMatrix y -> RealMatrix (x+y)
        | ComplexMatrix x, ComplexMatrix y -> ComplexMatrix (x+y)
        | NaN, _ | _, NaN -> NaN
        | ComplexInfinity, Infinity | Infinity, ComplexInfinity -> ComplexInfinity
        | PositiveInfinity, NegativeInfinity -> NaN
        | PositiveInfinity, _ | _, PositiveInfinity -> PositiveInfinity
        | NegativeInfinity, _ | _, NegativeInfinity -> NegativeInfinity
        | _ -> failwith "not supported"

    let multiply u v =
        match u, v with
        | Real x, Real y -> Real (x*y)
        | Real x, Complex y | Complex y, Real x -> Complex (C(x,0.0)*y)
        | Complex x, Complex y -> Complex (x*y)
        | RealVector x, RealVector y -> Real (x*y)
        | ComplexVector x, ComplexVector y -> Complex (x*y)
        | RealMatrix x, RealMatrix y -> RealMatrix (x*y)
        | ComplexMatrix x, ComplexMatrix y -> ComplexMatrix (x*y)
        | NaN, _ | _, NaN -> NaN
        | ComplexInfinity, Infinity | Infinity, ComplexInfinity -> ComplexInfinity
        | PositiveInfinity, NegativeInfinity -> NegativeInfinity
        | PositiveInfinity, Real x | Real x, PositiveInfinity ->
            if x < 0.0 then NegativeInfinity else if x > 0.0 then PositiveInfinity else NaN
        | NegativeInfinity, Real x | Real x, NegativeInfinity ->
            if x < 0.0 then PositiveInfinity else if x > 0.0 then NegativeInfinity else NaN
        | PositiveInfinity, _ | _, PositiveInfinity -> PositiveInfinity
        | NegativeInfinity, _ | _, NegativeInfinity -> NegativeInfinity
        | _ -> failwith "not supported"

    let power u v =
        match u, v with
        | Real x, Real y -> Real (Math.Pow(x, y))
        | Complex x, Real y -> Complex (Complex.Pow(x, y))
        | Real x, Complex y -> Complex (Complex.Pow(C(x,0.0), y))
        | Complex x, Complex y -> Complex (Complex.Pow(x, y))
        | NaN, _ | _, NaN -> NaN
        | ComplexInfinity, Infinity | Infinity, ComplexInfinity -> ComplexInfinity
        | Infinity, PositiveInfinity -> ComplexInfinity
        | Infinity, NegativeInfinity -> Real (0.0)
        | _ -> failwith "not supported"

    let unary f u =
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

    let nary f xs = failwith "not supported"

    let rec evaluate symbols = function
        | Number (Integer x) -> Real (float x) |> normalize
        | Number (Rational x) -> Real (float x) |> normalize
        | Identifier Symbol.Undefined -> NaN
        | Identifier Symbol.Infinity -> PositiveInfinity
        | Identifier _ as x -> Map.find x symbols |> normalize
        | Sum xs -> xs |> List.map (evaluate symbols) |> List.reduce add |> normalize
        | Product xs -> xs |> List.map (evaluate symbols) |> List.reduce multiply |> normalize
        | Power (r, p) -> power (evaluate symbols r) (evaluate symbols p) |> normalize
        | Function (f, x) -> unary f (evaluate symbols x) |> normalize
        | FunctionN (f, xs) -> xs |> List.map (evaluate symbols) |> nary f |> normalize
