﻿namespace MathNet.Symbolics

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
    | BesselI   // Modified Bessel function of the first kind and exponentially scaled
    | BesselK   // Modified Bessel function of the second kind
    | BesselIRatio     // Ratio of modified Bessel function of the first kind,
                       // BesselIRatio(n, x) is defined as BesselIRatio(n + 1, x) / BesselIRatio(n, x).
    | BesselKRatio     // Ratio of modified Bessel function of the second kind scaled by exponential
                       // BesselKRatio(n, x) is defined as BesselKRatio(n + 1, x) / BesselKRatio(n, x).
    | HankelH1  // Hankel function of the first kind
    | HankelH2  // Hankel function of the second kind

type Constant =
    | E
    | Pi
    | I
