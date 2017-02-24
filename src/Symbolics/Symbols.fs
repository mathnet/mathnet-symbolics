namespace MathNet.Symbolics

type Symbol = Symbol of string

type Function =
    | Abs
    | Ln | Exp
    | Sin | Cos | Tan
    | Cot | Sec | Csc 
    | Cosh | Sinh | Tanh
    | Asin | Acos | Atan

type Constant =
    | E
    | Pi
    | I
