namespace MathNet.Symbolics

type Symbol =
    | Symbol of string
    | Undefined
    | Infinity
    | ComplexInfinity

type Function =
    | Abs
    | Ln | Exp
    | Sin | Cos | Tan
