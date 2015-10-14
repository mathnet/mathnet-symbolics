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
    | Real of float


module ConstantPatterns =

    let (|PositiveReal|NegativeReal|ZeroReal|UndefinedReal|Complex|) = function
        | E | Pi -> PositiveReal
        | I -> Complex
        | Real x when x > 0.0 -> PositiveReal
        | Real x when x < 0.0 -> NegativeReal
        | Real x when x = 0.0 -> ZeroReal
        | Real x -> UndefinedReal
