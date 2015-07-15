namespace MathNet.Symbolics

open MathNet.Symbolics
open Operators

[<RequireQualifiedAccess>]
module Curve =

    /// Find tangent line function for x(symbol) at symbol=a
    [<CompiledName("TangentLine")>]
    let tangentLine symbol a x =
        let m = Calculus.differentiate symbol x |> Structure.substitute symbol a
        m*(symbol - a) + Structure.substitute symbol a x |> Algebraic.expand

    /// Find normal line (perpendicular to tangent) function for x(symbol) at symbol=a
    [<CompiledName("NormalLine")>]
    let normalLine symbol a x =
        let m = Calculus.differentiate symbol x |> Structure.substitute symbol a
        -(1/m)*(symbol - a) + Structure.substitute symbol a x |> Algebraic.expand
