namespace MathNet.Symbolics

open MathNet.Symbolics
open Operators

[<RequireQualifiedAccess>]
module Calculus =

    open ExpressionPatterns

    [<CompiledName("Differentiate")>]
    let rec differentiate symbol = function
        | x when x = symbol -> one
        | Undefined as x -> x
        | Number _ | Approximation _ | Identifier _ | Constant _ | Infinity | ComplexInfinity -> zero
        | Sum xs -> sum <| List.map (differentiate symbol) xs
        | Product [x] -> differentiate symbol x
        | Product (x::xs) ->
            let dx = differentiate symbol x
            let dxs = differentiate symbol (Product xs)
            x*dxs + (product xs)*dx
        | Power (r, e) as p ->
            let dr = differentiate symbol r
            let de = differentiate symbol e
            de*ln(r)*p + e*dr*(r**(e-1))
        | Function (Exp, x) as f -> (differentiate symbol x) * f
        | Function (Ln, x) -> (differentiate symbol x) / x
        | Function (Sin, x) -> (differentiate symbol x) * cos(x)
        | Function (Cos, x) -> -(differentiate symbol x) * sin(x)
        | Function (Tan, x) -> 2*(differentiate symbol x) / (cos(2*x)+1)
        | Function (Cosh, x) -> (differentiate symbol x) * sinh (x)
        | Function (Sinh, x) -> (differentiate symbol x) * cosh (x)
        | Function (Tanh, x) -> 2*(differentiate symbol x) / (cosh(2*x)+1)
        | Function (ArcSin, x) -> (1Q/sqrt(1Q+pow x 2Q)) * (differentiate symbol x)
        | Function (ArcCos, x) -> (-1Q/sqrt(1Q+pow x 2Q)) * (differentiate symbol x)
        | Function (ArcTan, x) -> (1Q/1Q+pow x 2Q) * (differentiate symbol x)
        | FunctionN (f, xs) -> failwith "not supported"
        | Function (Abs, _) -> failwith "not supported"
        | Product [] -> failwith "invalid expression"

    /// Differentiate expression to symbol and substitute symbol with value
    [<CompiledName("DifferentiateAt")>]
    let differentiateAt symbol value expression =
        expression |> differentiate symbol |> Structure.substitute symbol value

    /// Taylor expansion of expression(symbol) at symbol=value of the first k terms
    [<CompiledName("Taylor")>]
    let taylor (k:int) symbol value expression =
        let rec impl n nf acc dxn =
            if n = k then acc else
            impl (n+1) (nf*(n+1)) (acc + (dxn |> Structure.substitute symbol value)/nf*(symbol-value)**n) (differentiate symbol dxn)
        impl 0 1 zero expression |> Algebraic.expand

    /// Find tangent line function for expression(symbol) at symbol=value
    [<CompiledName("TangentLine")>]
    let tangentLine symbol value expression =
        let slope = expression |> differentiate symbol |> Structure.substitute symbol value
        let intercept = expression |> Structure.substitute symbol value
        slope*(symbol - value) + intercept |> Algebraic.expand

    /// Find normal line (perpendicular to tangent) function for expression(symbol) at symbol=value
    [<CompiledName("NormalLine")>]
    let normalLine symbol value expression =
        let slope = expression |> differentiate symbol |> Structure.substitute symbol value
        let intercept = expression |> Structure.substitute symbol value
        -(1/slope)*(symbol - value) + intercept |> Algebraic.expand
