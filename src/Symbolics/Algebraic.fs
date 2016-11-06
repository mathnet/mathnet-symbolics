namespace MathNet.Symbolics

open MathNet.Numerics

[<RequireQualifiedAccess>]
module Algebraic =

    open ExpressionPatterns
    open Operators

    [<CompiledName("Summands")>]
    let summands = function
        | Sum ax -> ax
        | x -> [x]

    [<CompiledName("Factors")>]
    let factors = function
        | Product ax -> ax
        | x -> [x]

    [<CompiledName("FactorsInteger")>]
    let factorsInteger x =
        let denom (n:BigRational) = Expression.FromIntegerFraction (1I, n.Denominator)
        match x with
        | Integer n -> (n.Numerator, [])
        | Number n -> (n.Numerator, [denom n])
        | Product ((Integer n)::ax) -> (n.Numerator, ax)
        | Product ((Number n)::ax) -> (n.Numerator, (denom n)::ax)
        | x -> (1I, [x])

    /// Splits a product into a tuple (free of a symbol, dependent on symbol)
    [<CompiledName("SeparateFactors")>]
    let separateFactors symbol x =
        match x with
        | Product ax -> let f, d = List.partition (Structure.freeOf symbol) ax in (product f, product d)
        | a when Structure.freeOf symbol a -> (a, one)
        | a -> (one, a)

    let rec private expandProduct x y =
        match x, y with
        | Sum ax, b | b, Sum ax -> sum <| List.map (expandProduct b) ax
        | a, b -> a*b

    let rec private expandPower x (y:int) =
        match x, y with
        | Sum [a], b -> a**(number b)
        | Sum (a::ax), n when n > 1 ->
            let e = int n
            [for k in 0 .. e -> (k, int <| SpecialFunctions.Binomial(e, k))]
            |> List.map (fun (k,c) -> expandProduct (c * a**number(e-k)) (expandPower (Sum ax) k))
            |> sum
        | a, b -> a**(number b)

    /// Algebraically expand the expression recursively
    [<CompiledName("Expand")>]
    let rec expand = function
        | Sum ax -> sum <| List.map expand ax
        | Product ax -> List.map expand ax |> List.reduce expandProduct
        | PosIntPower (r, Number n) -> expandPower (expand r) (int n)
        | x -> x

    /// Algebraically expand the main operator of the expression only
    [<CompiledName("ExpandMain")>]
    let expandMain = function
        | Product ax -> List.reduce expandProduct ax
        | PosIntPower (r, Number n) -> expandPower r (int n)
        | x -> x
