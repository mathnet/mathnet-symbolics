namespace MathNet.Symbolics

type Symbol = Symbol of string

type Function =
    | Abs
    | Sqrt
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
    | Real of float
    | NegativeInfinity
    | PositiveInfinity
    | ComplexInfinity
