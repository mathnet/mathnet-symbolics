namespace MathNet.Symbolics

open MathNet.Symbolics
open Operators

[<RequireQualifiedAccess>]
module Calculus =

    [<CompiledName("Differentiate")>]
    let rec differentiate symbol = function
        | x when x = symbol -> one
        | Undefined as x -> x
        | Number _ | Approximation _ | Identifier _ | Argument _ | Constant _ | ComplexInfinity | PositiveInfinity | NegativeInfinity -> zero
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
        | Function (Lg, x) -> (differentiate symbol ((ln x) / (ln 10Q)))
        | Function (Sin, x) -> (differentiate symbol x) * cos(x)
        | Function (Cos, x) -> -(differentiate symbol x) * sin(x)
        | Function (Tan, x) -> 2*(differentiate symbol x) / (cos(2Q * x) + 1Q)
        | Function (Cosh, x) -> (differentiate symbol x) * sinh(x)
        | Function (Sinh, x) -> (differentiate symbol x) * cosh(x)
        | Function (Tanh, x) -> 2*(differentiate symbol x) / (cosh(2Q * x) + 1)
        | Function (Asin, x) -> (1Q / sqrt(1Q - pow x 2Q)) * (differentiate symbol x)
        | Function (Acos, x) -> (-1Q / sqrt(1Q - pow x 2Q)) * (differentiate symbol x)
        | Function (Atan, x) -> (1Q / (1Q + pow x 2Q)) * (differentiate symbol x)
        | Function (Cot, x) -> (-1Q/(sin(x) * sin(x))) * (differentiate symbol x)
        | Function (Csc, x) -> (-cot(x) * csc(x)) * (differentiate symbol x)
        | Function (Sec, x) -> (tan(x) * sec(x)) * (differentiate symbol x)
        | Function (Acsc, x) -> -(differentiate symbol x) / ((sqrt(1Q - 1Q / pow x 2Q) * pow x 2Q))
        | Function (Asec, x) -> (differentiate symbol x) / ((sqrt(1Q - 1Q / pow x 2Q) * pow x 2Q))
        | Function (Acot, x) -> -(differentiate symbol x) / (1Q + pow x 2Q)
        | Function (Csch, x) -> (differentiate symbol x) * (-coth(x) * csch(x))
        | Function (Sech, x) -> (differentiate symbol x) * (-tanh(x) * sech(x))
        | Function (Coth, x) -> -2 * (differentiate symbol x) / (cosh(2Q * x) - 1Q)
        | Function (Asinh, x) -> (differentiate symbol x) / (sqrt(pow x 2Q + 1Q))
        | Function (Acosh, x) -> (differentiate symbol x) / (sqrt(x - 1Q) * sqrt(x + 1Q))
        | Function (Atanh, x) -> (differentiate symbol x) / (1Q - pow x 2Q)
        | Function (Acsch, x) -> -(differentiate symbol x) / (sqrt(1Q + 1Q / pow x 2Q) * pow x 2Q)
        | Function (Asech, x) -> (differentiate symbol x) * (sqrt((1Q - x) / (x + 1Q))) / ((x - 1Q) * x)
        | Function (Acoth, x) -> (differentiate symbol x) / (1Q - pow x 2Q)
        | Function (Abs, _) -> failwith "not supported"
        | Function (AiryAi, x) -> (differentiate symbol x) * airyaiprime(x)
        | Function (AiryAiPrime, x) -> (differentiate symbol x) * x * airyai(x)
        | Function (AiryBi, x) -> (differentiate symbol x) * airybiprime(x)
        | Function (AiryBiPrime, x) -> (differentiate symbol x) * x * airybi(x)
        | FunctionN (Min, _) -> failwith "not supported"
        | FunctionN (Max, _) -> failwith "not supported"
        | FunctionN (Atan2, [x; y]) -> differentiate symbol (tan (x / y))
        | FunctionN (Log, [b; x]) -> differentiate symbol ((ln x) / (ln b))
        | FunctionN (BesselJ, [nu; x]) -> (differentiate symbol x) * ((besselj (nu - 1Q) x) - (besselj (nu + 1Q) x)) / 2Q
        | FunctionN (BesselY, [nu; x]) -> (differentiate symbol x) * ((bessely (nu - 1Q) x) - (bessely (nu + 1Q) x)) / 2Q
        | FunctionN (BesselI, [nu; x]) -> (differentiate symbol x) * ((besseli (nu - 1Q) x) + (besseli (nu + 1Q) x)) / 2Q
        | FunctionN (BesselK, [nu; x]) -> (differentiate symbol x) * (-(besselk (nu - 1Q) x) - (besselk (nu + 1Q) x)) / 2Q
        | FunctionN (BesselIRatio, [nu; x]) -> (differentiate symbol x) * ((pow (besseli nu x) 2Q) + (besseli (nu + 2Q) x) * (besseli nu x) - (besseli (nu + 1Q) x) * (besseli (nu - 1Q) x) - (pow (besseli (nu + 1Q) x) 2Q)) / (2Q * (pow (besseli nu x) 2Q))
        | FunctionN (BesselKRatio, [nu; x]) -> -(differentiate symbol x) * ((pow (besselk nu x) 2Q) + (besselk (nu + 2Q) x) * (besselk nu x) - (besselk (nu + 1Q) x) * (besselk (nu - 1Q) x) - (pow (besselk (nu + 1Q) x) 2Q)) / (2Q * (pow (besselk nu x) 2Q))
        | FunctionN (HankelH1, [nu; x]) -> (differentiate symbol x) * ((hankelh1 (nu - 1Q) x) - (hankelh1 (nu + 1Q) x)) / 2Q
        | FunctionN (HankelH2, [nu; x]) -> (differentiate symbol x) * ((hankelh2 (nu - 1Q) x) - (hankelh2 (nu + 1Q) x)) / 2Q
        | FunctionN (_) -> failwith "not supported"
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
