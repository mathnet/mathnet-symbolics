namespace MathNet.Symbolics

open MathNet.Numerics
open MathNet.Symbolics

[<RequireQualifiedAccess>]
module Numbers =

    open ExpressionPatterns

    /// Represent the constant as a real number if possible
    let (|RealConstant|_|) = function
        | Approximation (Double r) -> Some r
        | Constant E -> Some Constants.E
        | Constant Pi -> Some Constants.Pi
        | Infinity -> Some System.Double.PositiveInfinity
        | ComplexInfinity -> Some System.Double.PositiveInfinity
        | NegativeInfinity -> Some System.Double.NegativeInfinity
        | _ -> None

    [<CompiledName("Compare")>]
    let compare x y =
        match x, y with
        | a, b when a = b -> 0
        | Number _, Infinity -> -1
        | Number _, ComplexInfinity -> -1
        | Number _, NegativeInfinity -> 1
        | Infinity, Number _ -> 1
        | ComplexInfinity, Number _ -> 1
        | NegativeInfinity, Number _ -> -1
        | Number a, Number b -> compare a b
        | Number a, RealConstant b -> compare (float a) b
        | RealConstant a, Number b -> compare a (float b)
        | RealConstant a, RealConstant b -> compare a b
        | _ -> failwith "only numbers and +/-infinity are supported"

    [<CompiledName("Max2")>]
    let max2 u v = if compare u v >= 0 then u else v

    [<CompiledName("Min2")>]
    let min2 u v = if compare u v <= 0 then u else v

    [<CompiledName("Max")>]
    let max ax = List.reduce max2 ax

    [<CompiledName("Min")>]
    let min ax = List.reduce min2 ax

    [<CompiledName("GreatestCommonDivisor2")>]
    let gcd2 u v =
        match u, v with
        | Number a, Number b when a.IsInteger && b.IsInteger ->
            Euclid.GreatestCommonDivisor(a.Numerator, b.Numerator) |> Expression.FromInteger
        | _ -> Undefined

    [<CompiledName("LeastCommonMultiple2")>]
    let lcm2 u v =
        match u, v with
        | Number a, Number b when a.IsInteger && b.IsInteger ->
            Euclid.LeastCommonMultiple(a.Numerator, b.Numerator) |> Expression.FromInteger
        | _ -> Undefined

    [<CompiledName("GreatestCommonDivisor")>]
    let gcd ax = List.reduce gcd2 ax

    [<CompiledName("LeastCommonMultiple")>]
    let lcm ax = List.reduce lcm2 ax


[<RequireQualifiedAccess>]
module Structure =

    open System.Collections.Generic
    open ExpressionPatterns
    open Operators

    [<CompiledName("NumberOfOperands")>]
    let numberOfOperands = function
        | Sum ax | Product ax -> List.length ax
        | Power _ -> 2
        | Function _ -> 1
        | FunctionN (_, xs) -> List.length xs
        | Number _ | Approximation _ | Identifier _ | Constant _ | Infinity | ComplexInfinity -> 0
        | Undefined -> 0

    [<CompiledName("Operand")>]
    let operand i = function
        | Sum ax | Product ax | FunctionN (_, ax) -> List.nth ax i
        | Power (r, _) when i = 0 -> r
        | Power (_, p) when i = 1 -> p
        | Function (_, x) when i = 0 -> x
        | _ -> failwith "no such operand"

    [<CompiledName("IsFreeOf")>]
    let rec freeOf symbol x =
        if symbol = x then false else
        match x with
        | Sum ax | Product ax | FunctionN (_, ax) -> List.forall (freeOf symbol) ax
        | Power (r, p) -> freeOf symbol r && freeOf symbol p
        | Function (_, x) -> freeOf symbol x
        | Number _ | Approximation _ | Identifier _ | Constant _ | Infinity | ComplexInfinity -> true
        | Undefined -> true

    [<CompiledName("IsFreeOfSet")>]
    let rec freeOfSet (symbols: HashSet<Expression>) x =
        if symbols.Contains(x) then false else
        match x with
        | Sum ax | Product ax | FunctionN (_, ax) -> List.forall (freeOfSet symbols) ax
        | Power (r, p) -> freeOfSet symbols r && freeOfSet symbols p
        | Function (_, x) -> freeOfSet symbols x
        | Number _ | Approximation _ | Identifier _ | Constant _ | Infinity | ComplexInfinity -> true
        | Undefined -> true

    [<CompiledName("Substitute")>]
    let rec substitute y r x =
        if y = x then r else
        match x with
        | Sum ax -> sum <| List.map (substitute y r) ax
        | Product ax -> product <| List.map (substitute y r) ax
        | Power (radix, p) -> (substitute y r radix) ** (substitute y r p)
        | Function (fn, x) -> apply fn (substitute y r x)
        | FunctionN (fn, xs) -> applyN fn (List.map (substitute y r) xs)
        | Number _ | Approximation _ | Identifier _ | Constant _ | Infinity | ComplexInfinity -> x
        | Undefined -> x

    [<CompiledName("Map")>]
    let map f = function
        | Sum ax -> sum <| List.map f ax
        | Product ax -> product <| List.map f ax
        | Power (r, p) -> (f r) ** (f p)
        | Function (fn, x) -> apply fn (f x)
        | FunctionN (fn, xs) -> applyN fn (List.map f xs)
        | x -> x

    [<CompiledName("Fold")>]
    let fold f s = function
        | Sum ax | Product ax | FunctionN (_, ax) -> List.fold f s ax
        | Power (r, p) -> List.fold f s [r;p]
        | Function (_, x) -> f s x
        | _ -> s

    /// Sort expressions in a list with standard expression ordering.
    [<CompiledName("SortList")>]
    let sortList list =
        List.sortWith (fun a b -> if a = b then 0 elif orderRelation a b then -1 else 1) list

    /// Applies the given function to the expression tree and returns the result
    /// for each node where the function returns Some with some value.
    /// Subexpressions of an expression are only examined if the function returns
    /// None when applied to the expression.
    /// The results are returned as a list in reverse depth-first order.
    [<CompiledName("Collect")>]
    let collect (chooser:Expression->'T option) x =
        let rec impl (acc:'T list) x =
            match chooser x with
            | Some result -> result::acc
            | None -> fold impl acc x
        impl [] x

    /// Like collect but returns each result at most once.
    [<CompiledName("CollectDistinct")>]
    let collectDistinct chooser x =
        collect chooser x |> Seq.distinct |> List.ofSeq //potential for optimization...

    /// Collects all identifers of an expressions and returns their distinct expressions.
    [<CompiledName("CollectIdentifiers")>]
    let collectIdentifiers x =
        x |> collectDistinct (function | Identifier _ as expression -> Some expression | _ -> None) |> sortList

    /// Collects all identifers of an expressions and returns their distinct symbols.
    [<CompiledName("CollectIdentifierSymbols")>]
    let collectIdentifierSymbols x =
        x |> collectDistinct (function | Identifier symbol -> Some symbol | _ -> None) |> List.sort

    /// Collects all numbers of an expressions and returns their distinct expressions.
    [<CompiledName("CollectNumbers")>]
    let collectNumbers x =
        x |> collectDistinct (function | Number _ as expression -> Some expression | _ -> None) |> sortList

    /// Collects all numbers of an expressions and returns their distinct values.
    [<CompiledName("CollectNumberValues")>]
    let collectNumberValues x =
        x |> collectDistinct (function | Number number -> Some number | _ -> None) |> List.sort

    /// Collects all constants of an expressions and returns their distinct expressions.
    [<CompiledName("CollectConstants")>]
    let collectConstants x =
        x |> collectDistinct (function | Constant _ as expression -> Some expression | _ -> None) |> sortList

    /// Collects all constants of an expressions and returns their distinct values.
    [<CompiledName("CollectConstantValues")>]
    let collectConstantValues x =
        x |> collectDistinct (function | Constant constant -> Some constant | _ -> None) |> List.sort

    /// Collects all functions of an expressions and returns their distinct expressions.
    [<CompiledName("CollectFunctions")>]
    let collectFunctions x =
        x |> collectDistinct (function | Function _ | FunctionN _ as expression -> Some expression | _ -> None) |> sortList

    /// Collects all functions of an expressions and returns their distinct function types.
    [<CompiledName("CollectFunctionTypes")>]
    let collectFunctionTypes x =
        x |> collectDistinct (function | Function (f, _) | FunctionN (f, _) -> Some f | _ -> None) |> List.sort


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
