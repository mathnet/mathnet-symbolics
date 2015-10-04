namespace MathNet.Symbolics

type Symbol = Symbol of string

type Function =
    | Abs
    | Ln | Exp
    | Sin | Cos | Tan

type Constant =
    | E
    | Pi
    | I
    | Real of float
    | NegativeInfinity
    | PositiveInfinity
    | ComplexInfinity
