namespace MathNet.Symbolics

open MathNet.Symbolics
open Operators

[<RequireQualifiedAccess>]
module Calculus =

    [<CompiledName("Differentiate")>]
    let rec differentiate symbol = function
        | x when x = symbol -> one
        | Number _ | Identifier _ -> zero
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
        | Function (Abs, _) | FunctionN _ -> failwith "not supported"
        | Product [] -> failwith "invalid expression"
        | PositiveInfinity | NegativeInfinity | ComplexInfinity | Undefined as x -> x
