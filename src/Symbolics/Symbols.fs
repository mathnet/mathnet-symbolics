namespace MathNet.Symbolics

type Symbol =
    | Symbol of string
    | Undefined
    | NegativeInfinity
    | PositiveInfinity
    | ComplexInfinity

type Function =
    | Abs
    | Ln | Exp
    | Sin | Cos | Tan
