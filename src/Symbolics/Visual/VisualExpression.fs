namespace MathNet.Symbolics

open MathNet.Numerics
open MathNet.Symbolics
open Microsoft.FSharp.Reflection
open Operators

/// Expression tree structure focusing on visual aspects and as interchange format between different representations.
/// Intentionally open to allow also functions not supported by Expression, for visual-only or interchange use cases.
/// Not intended for algebraic manipulations, but can be converted from and to Expression.
[<StructuralEquality;NoComparison;RequireQualifiedAccess>]
type VisualExpression =
    | Symbol of name:string
    | PositiveInteger of value:BigInteger
    | PositiveFloatingPoint of value:float
    | Parenthesis of VisualExpression
    | Abs of VisualExpression
    | Negative of VisualExpression
    | Sum of VisualExpression list
    | Product of VisualExpression list
    | Fraction of VisualExpression * VisualExpression // a/b
    | Power of VisualExpression * VisualExpression // a^b
    | Root of VisualExpression * BigInteger // a^(1/b)
    | Function of name:string * power:BigInteger * VisualExpression
    | FunctionN of name:string * power:BigInteger * (VisualExpression list)
    | ComplexI
    | Infinity
    | ComplexInfinity
    | Undefined

type VisualExpressionStyle = {
    CompactPowersOfFunctions : bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VisualExpression =
    open Rational
    open ExpressionPatterns

    let private functionNameMap = FSharpType.GetUnionCases typeof<Function> |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> Function, case.Name.ToLowerInvariant()) |> Map.ofArray
    let private nameFunctionMap = functionNameMap |> Map.toList |> List.map (fun (f,n) -> (n,f)) |> Map.ofList
    let private functionName f = Map.find f functionNameMap
    let private nameFunction name = Map.find name nameFunctionMap

    let fromExpression (style:VisualExpressionStyle) expression =
        let compactPowersOfFunctions = style.CompactPowersOfFunctions
        let parenthesis priority threshold ve = if priority > threshold then VisualExpression.Parenthesis ve else ve
        let convertNumber priority (n:BigRational) =
            if n.IsNegative && n.IsInteger then
                VisualExpression.Negative (VisualExpression.PositiveInteger (-n.Numerator))
                |> parenthesis priority 0
            elif n.IsNegative then
                VisualExpression.Negative (VisualExpression.Fraction (VisualExpression.PositiveInteger (-n.Numerator), VisualExpression.PositiveInteger n.Denominator))
                |> parenthesis priority 1
            elif n.IsInteger then
                VisualExpression.PositiveInteger n.Numerator
            else
                VisualExpression.Fraction (VisualExpression.PositiveInteger n.Numerator, VisualExpression.PositiveInteger n.Denominator)
                |> parenthesis priority 1
        let convertApproximation priority (a:Approximation) =
            match a with
            | Approximation.Real fp when fp < 0.0 ->
                VisualExpression.Negative (VisualExpression.PositiveFloatingPoint (-fp))
                |> parenthesis priority 0
            | Approximation.Real fp ->
                VisualExpression.PositiveFloatingPoint fp
            | Approximation.Complex fp when fp.IsRealNonNegative() ->
                VisualExpression.PositiveFloatingPoint fp.Real
            | Approximation.Complex fp when fp.IsReal() ->
                VisualExpression.Negative (VisualExpression.PositiveFloatingPoint (-fp.Real))
                |> parenthesis priority 0
            | Approximation.Complex fp when fp.Real = 0.0 && fp.Imaginary = -1.0 ->
                VisualExpression.Negative VisualExpression.ComplexI
                |> parenthesis priority 0
            | Approximation.Complex fp when fp.Real = 0.0 && fp.Imaginary < 0.0 ->
                VisualExpression.Negative (VisualExpression.Product [ VisualExpression.PositiveFloatingPoint (-fp.Imaginary); VisualExpression.ComplexI ])
                |> parenthesis priority 0
            | Approximation.Complex fp when fp.Real = 0.0 && fp.Imaginary = 1.0 ->
                VisualExpression.ComplexI
            | Approximation.Complex fp when fp.Real = 0.0 ->
                VisualExpression.Product [ VisualExpression.PositiveFloatingPoint fp.Imaginary; VisualExpression.ComplexI ]
                |> parenthesis priority 2
            | Approximation.Complex fp when fp.Real < 0.0 && fp.Imaginary = -1.0 ->
                VisualExpression.Sum [VisualExpression.Negative (VisualExpression.PositiveFloatingPoint (-fp.Real)); VisualExpression.Negative VisualExpression.ComplexI]
                |> parenthesis priority 1
            | Approximation.Complex fp when fp.Real < 0.0 && fp.Imaginary < 0.0 ->
                VisualExpression.Sum [VisualExpression.Negative (VisualExpression.PositiveFloatingPoint (-fp.Real)); VisualExpression.Negative (VisualExpression.Product [ VisualExpression.PositiveFloatingPoint (-fp.Imaginary); VisualExpression.ComplexI ])]
                |> parenthesis priority 1
            | Approximation.Complex fp when fp.Real < 0.0 && fp.Imaginary = 1.0 ->
                VisualExpression.Sum [VisualExpression.Negative (VisualExpression.PositiveFloatingPoint (-fp.Real)); VisualExpression.ComplexI]
                |> parenthesis priority 1
            | Approximation.Complex fp when fp.Real < 0.0 ->
                VisualExpression.Sum [VisualExpression.Negative (VisualExpression.PositiveFloatingPoint (-fp.Real)); VisualExpression.Product [ VisualExpression.PositiveFloatingPoint fp.Imaginary; VisualExpression.ComplexI ]]
                |> parenthesis priority 1
            | Approximation.Complex fp when fp.Imaginary = -1.0 ->
                VisualExpression.Sum [VisualExpression.PositiveFloatingPoint fp.Real; VisualExpression.Negative VisualExpression.ComplexI]
                |> parenthesis priority 1
            | Approximation.Complex fp when fp.Imaginary < 0.0 ->
                VisualExpression.Sum [VisualExpression.PositiveFloatingPoint fp.Real; VisualExpression.Negative (VisualExpression.Product [ VisualExpression.PositiveFloatingPoint (-fp.Imaginary); VisualExpression.ComplexI ])]
                |> parenthesis priority 1
            | Approximation.Complex fp when fp.Imaginary = 1.0 ->
                VisualExpression.Sum [VisualExpression.PositiveFloatingPoint fp.Real; VisualExpression.ComplexI]
                |> parenthesis priority 1
            | Approximation.Complex fp ->
                VisualExpression.Sum [VisualExpression.PositiveFloatingPoint fp.Real; VisualExpression.Product [ VisualExpression.PositiveFloatingPoint fp.Imaginary; VisualExpression.ComplexI ]]
                |> parenthesis priority 1
        let rec convertFractionPart priority = function
            | Product xs -> VisualExpression.Product (xs |> List.map (convert 2)) |> parenthesis priority 2
            | x -> convert priority x
        and convertSummand = function
            | Number n as x when n.IsNegative ->
                VisualExpression.Negative (convert 1 (-x))
            | Approximation (Approximation.Real fp) as x when fp < 0.0 ->
                VisualExpression.Negative (convert 1 (-x))
            | Product ((Number n)::xs) when n.IsNegative ->
                VisualExpression.Negative (convert 2 (product ((Number -n)::xs)))
            | Product ((Approximation (Approximation.Real fp))::xs) when fp < 0.0 ->
                VisualExpression.Negative (convert 2 (product ((Approximation (Approximation.Real -fp))::xs)))
            | x -> convert 1 x
        and convert priority = function
            | Number number ->
                convertNumber priority number
            | Approximation approximation ->
                convertApproximation priority approximation
            | Identifier (Symbol s) ->
                VisualExpression.Symbol s
            | Argument (Symbol s) ->
                VisualExpression.Symbol s
            | Constant Pi ->
                VisualExpression.Symbol "pi"
            | Constant E ->
                VisualExpression.Symbol "e"
            | Constant I ->
                VisualExpression.ComplexI
            | ComplexInfinity ->
                VisualExpression.ComplexInfinity
            | PositiveInfinity ->
                VisualExpression.Infinity
            | NegativeInfinity ->
                VisualExpression.Negative VisualExpression.Infinity
                |> parenthesis priority 0
            | Undefined ->
                VisualExpression.Undefined
            | Sum xs ->
                VisualExpression.Sum (xs |> List.map convertSummand)
                |> parenthesis priority 1
            | Product (Number n::xs) when n.IsNegative ->
                VisualExpression.Negative (convert 2 (product ((Number -n)::xs)))
                |> parenthesis priority 0
            | Product _ as p ->
                match (numerator p), (denominator p) with
                | num, One ->
                    convertFractionPart 2 num
                    |> parenthesis priority 2
                | (Integer _ as num), (Integer _ as denom) ->
                    VisualExpression.Fraction (convert 3 num, convert 3 denom)
                    |> parenthesis priority 2
                | (Product (Integer n::xs) as num), (Integer _ as denom) ->
                    let prefix = VisualExpression.Fraction (convert 3 (Number n), convert 3 denom)
                    match convertFractionPart 2 (product xs) with
                    | VisualExpression.Product suffix -> VisualExpression.Product (prefix::suffix)
                    | suffix -> VisualExpression.Product [prefix; suffix]
                    |> parenthesis priority 2
                | (Product _ as num), (Integer _ as denom) ->
                    let prefix = VisualExpression.Fraction (VisualExpression.PositiveInteger (bigint 1), convert 3 denom)
                    match convertFractionPart 2 num with
                    | VisualExpression.Product suffix -> VisualExpression.Product (prefix::suffix)
                    | suffix -> VisualExpression.Product [prefix; suffix]
                    |> parenthesis priority 2
                | num, denom ->
                    VisualExpression.Fraction (convertFractionPart 3 num, convertFractionPart 3 denom)
                    |> parenthesis priority 2
            | NegIntPower (r, p) ->
                let d =
                    if p = Expression.MinusOne then convert 3 r
                    else VisualExpression.Power (convert 3 r, convert 3 (-p))
                VisualExpression.Fraction (VisualExpression.PositiveInteger BigInteger.One, d)
                |> parenthesis priority 2
            | PosIntPower (Function (f, x), Integer p) when f <> Abs && compactPowersOfFunctions ->
                VisualExpression.Function (functionName f, p.Numerator, convert 0 x)
                |> parenthesis priority 3
            | PosIntPower (FunctionN (f, xs), Integer p) when f <> Abs && compactPowersOfFunctions ->
                VisualExpression.FunctionN (functionName f, p.Numerator, xs |> List.map (convert 0))
                |> parenthesis priority 3
            | Power (r, Number n) when n.IsPositive && n.Numerator = BigInteger.One ->
                VisualExpression.Root (convert 4 r, n.Denominator)
                |> parenthesis priority 3
            | Power (r, Power(Integer n, minusOne)) when minusOne = Expression.MinusOne ->
                VisualExpression.Root (convert 4 r, n.Numerator)
                |> parenthesis priority 3
            | Power (r, p) ->
                VisualExpression.Power (convert 4 r, convert 4 p)
                |> parenthesis priority 3
            | Function (Abs, x) ->
                VisualExpression.Abs (convert 0 x)
            | Function (f, x) ->
                VisualExpression.Function (functionName f, BigInteger.One, convert 0 x)
                |> parenthesis priority 3
            | FunctionN (f, xs) ->
                VisualExpression.FunctionN (functionName f, BigInteger.One, xs |> List.map (convert 0))
                |> parenthesis priority 3
        convert 0 expression

    let toExpression visualExpression =
        let rec convert = function
            | VisualExpression.Symbol name -> symbol name
            | VisualExpression.PositiveInteger value -> fromInteger value
            | VisualExpression.PositiveFloatingPoint value -> fromReal value
            | VisualExpression.Parenthesis x -> convert x
            | VisualExpression.Abs x -> convert x |> abs
            | VisualExpression.Negative x -> convert x |> negate
            | VisualExpression.Sum xs -> xs |> List.map convert |> sum
            | VisualExpression.Product xs -> xs |> List.map convert |> product
            | VisualExpression.Fraction (numerator, denominator) -> (convert numerator)/(convert denominator)
            | VisualExpression.Power (radix, power) -> pow (convert radix) (convert power)
            | VisualExpression.Root (radix, power) -> root (fromInteger power) (convert radix)
            | VisualExpression.Function (fn, power, x) ->
                let applied = apply (nameFunction fn) (convert x)
                if power.IsOne then applied else pow applied (fromInteger power)
            | VisualExpression.FunctionN (fn, power, xs) ->
                let applied = applyN (nameFunction fn) (List.map convert xs)
                if power.IsOne then applied else pow applied (fromInteger power)
            | VisualExpression.ComplexI -> Expression.I
            | VisualExpression.Infinity -> PositiveInfinity
            | VisualExpression.ComplexInfinity -> ComplexInfinity
            | VisualExpression.Undefined -> Undefined
        convert visualExpression
