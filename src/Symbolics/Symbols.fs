namespace MathNet.Symbolics

type Symbol = Symbol of string

type Function =
    | Abs
    | Ln | Log | Exp
    | Sin | Cos | Tan
    | Csc | Sec | Cot
    | Sinh | Cosh | Tanh
    | Csch | Sech | Coth
    | Asin | Acos | Atan
    | Acsc | Asec | Acot
    | Asinh | Acosh | Atanh
    | Acsch | Asech | Acoth

type Constant =
    | E
    | Pi
    | I
