namespace MathNet.Symbolics

open MathNet.Numerics

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
    | BesselJ   // Bessel function of the first kind
    | BesselY   // Bessel function of the second kind
    | BesselI   // Modified Bessel function of the first kind
    | BesselK   // Modified Bessel function of the second kind
    | HankelH1  // Hankel function of the first kind
    | HankelH2  // Hankel function of the second kind

type Constant =
    | E
    | Pi
    | I
