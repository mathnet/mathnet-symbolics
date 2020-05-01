namespace MathNet.Symbolics

open MathNet.Numerics
open MathNet.Symbolics

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

type IVisualStyle =
    abstract member CompactPowersOfFunctions : bool with get
    abstract member SemanticFunction: f:Function * power:BigInteger * e:VisualExpression -> VisualExpression
    abstract member SemanticFunctionN: f:Function * power:BigInteger * e:VisualExpression array -> VisualExpression
    abstract member VisualFunction: f:string * power:BigInteger * e:Expression -> Expression
    abstract member VisualFunctionN: f:string * power:BigInteger * e:Expression array -> Expression

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VisualExpression =
    open Rational
    open Operators
    open ExpressionPatterns

    let fromExpression (style:IVisualStyle) expression =
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
        let rec convertFractionPart style priority = function
            | Product xs -> VisualExpression.Product (xs |> List.map (convert style 2)) |> parenthesis priority 2
            | x -> convert style priority x
        and convertSummand style = function
            | Number n as x when n.IsNegative ->
                VisualExpression.Negative (convert style 1 (-x))
            | Approximation (Approximation.Real fp) as x when fp < 0.0 ->
                VisualExpression.Negative (convert style 1 (-x))
            | Product ((Number n)::xs) when n.IsNegative ->
                VisualExpression.Negative (convert style 2 (product ((Number -n)::xs)))
            | Product ((Approximation (Approximation.Real fp))::xs) when fp < 0.0 ->
                VisualExpression.Negative (convert style 2 (product ((Approximation (Approximation.Real -fp))::xs)))
            | x -> convert style 1 x
        and convert (style:IVisualStyle) priority = function
            | Number number ->
                convertNumber priority number
            | Approximation approximation ->
                convertApproximation priority approximation
            | Identifier (Symbol s) ->
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
                VisualExpression.Sum (xs |> List.map (convertSummand style))
                |> parenthesis priority 1
            | Product (Number n::xs) when n.IsNegative ->
                VisualExpression.Negative (convert style 2 (product ((Number -n)::xs)))
                |> parenthesis priority 0
            | Product _ as p ->
                match (numerator p), (denominator p) with
                | num, One ->
                    convertFractionPart style 2 num
                    |> parenthesis priority 2
                | (Integer _ as num), (Integer _ as denom) ->
                    VisualExpression.Fraction (convert style 3 num, convert style 3 denom)
                    |> parenthesis priority 2
                | (Product (Integer n::xs) as num), (Integer _ as denom) ->
                    let prefix = VisualExpression.Fraction (convert style 3 (Number n), convert style 3 denom)
                    match convertFractionPart style 2 (product xs) with
                    | VisualExpression.Product suffix -> VisualExpression.Product (prefix::suffix)
                    | suffix -> VisualExpression.Product [prefix; suffix]
                    |> parenthesis priority 2
                | (Product _ as num), (Integer _ as denom) ->
                    let prefix = VisualExpression.Fraction (VisualExpression.PositiveInteger (bigint 1), convert style 3 denom)
                    match convertFractionPart style 2 num with
                    | VisualExpression.Product suffix -> VisualExpression.Product (prefix::suffix)
                    | suffix -> VisualExpression.Product [prefix; suffix]
                    |> parenthesis priority 2
                | num, denom ->
                    VisualExpression.Fraction (convertFractionPart style 3 num, convertFractionPart style 3 denom)
                    |> parenthesis priority 2
            | NegIntPower (r, p) ->
                let d =
                    if p = Expression.MinusOne then convert style 3 r
                    else VisualExpression.Power (convert style 3 r, convert style 3 (-p))
                VisualExpression.Fraction (VisualExpression.PositiveInteger BigInteger.One, d)
                |> parenthesis priority 2
            | PosIntPower (Function (f, x), Integer p) when style.CompactPowersOfFunctions ->
                style.SemanticFunction (f, p.Numerator, convert style 0 x)
                |> parenthesis priority 3
            | PosIntPower (FunctionN (f, xs), Integer p) when style.CompactPowersOfFunctions ->
                style.SemanticFunctionN (f, p.Numerator, xs |> List.map (convert style 0) |> List.toArray)
                |> parenthesis priority 3
            | Power (r, Number n) when n.IsPositive && n.Numerator = BigInteger.One ->
                VisualExpression.Root (convert style 4 r, n.Denominator)
                |> parenthesis priority 3
            | Power (r, Power(Integer n, minusOne)) when minusOne = Expression.MinusOne ->
                VisualExpression.Root (convert style 4 r, n.Numerator)
                |> parenthesis priority 3
            | Power (r, p) ->
                VisualExpression.Power (convert style 4 r, convert style 4 p)
                |> parenthesis priority 3
            | Function (Abs, x) ->
                VisualExpression.Abs (convert style 0 x)
            | Function (f, x) ->
                style.SemanticFunction (f, BigInteger.One, convert style 0 x)
                |> parenthesis priority 3
            | FunctionN (f, xs) ->
                style.SemanticFunctionN (f, BigInteger.One, xs |> List.map (convert style 0) |> List.toArray)
                |> parenthesis priority 3

        convert style 0 expression


type DefaultVisualStyle() =

    let functionName = function
        | Abs -> "abs"
        | Ln -> "ln" | Log -> "log"
        | Exp -> "exp"
        | Sin -> "sin" | Cos -> "cos" | Tan -> "tan"
        | Csc -> "csc" | Sec -> "sec" | Cot -> "cot"
        | Sinh -> "sinh" | Cosh -> "cosh" | Tanh -> "tanh"
        | Csch -> "csch" | Sech -> "sech" | Coth -> "coth"
        | Asin -> "asin" | Acos -> "acos" | Atan -> "atan"
        | Acsc -> "acsc" | Asec -> "asec" | Acot -> "acot"
        | Acosh -> "acosh" | Asinh -> "asinh" | Atanh -> "atanh"
        | Acsch -> "acsch" | Asech -> "asech" | Acoth -> "acoth"
        | AiryAi -> "airyai"
        | AiryAiPrime -> "airyaiprime"
        | AiryBi -> "airybi"
        | AiryBiPrime -> "airybiprime"
        | BesselJ -> "besselj"
        | BesselY -> "bessely"
        | BesselI -> "besseli"
        | BesselK -> "besselk"
        | BesselIRatio -> "besseliratio"
        | BesselKRatio -> "besselkratio"
        | HankelH1 -> "hankelh1"
        | HankelH2 -> "hankelh2"

    interface IVisualStyle with
        member __.CompactPowersOfFunctions with get() = false
        member __.SemanticFunction (f:Function, power:BigInteger, e:VisualExpression) =
            match f with
            | Abs -> VisualExpression.Abs e
            | _ -> VisualExpression.Function (functionName f, power, e)
        member __.SemanticFunctionN (f:Function, power:BigInteger, e:VisualExpression array) =
            VisualExpression.FunctionN (functionName f, power, List.ofArray e)
        member __.VisualFunction (f:string, power:BigInteger, e:Expression) =
            failwith "TODO"
            Expression.Undefined
        member __.VisualFunctionN (f:string, power:BigInteger, e:Expression array) =
            failwith "TODO"
            Expression.Undefined
