namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

type Symbol =
    | Symbol of string
    | Undefined
    | Infinity
    | ComplexInfinity

type Function =
    | Abs
    | Ln | Exp
    | Sin | Cos | Tan
