namespace MathNet.Symbolics

type Symbol = Symbol of string

type Function =
    | Abs
    | Ln | Exp
    | Sin | Cos | Tan
    | Cosh | Sinh | Tanh
    | ArcSin
    | ArcCos
    | ArcTan

type Constant =
    | E
    | Pi
    | I

type Approximation =
    | Double of float
