namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics


module Operators =

    let symbol name = Identifier (Symbol name)
    let number (x:int) = Expression.FromInt32 x
    let zero = Expression.Zero
    let one = Expression.One
    let two = Expression.Two
    let minusOne = Expression.MinusOne

    let add (x:Expression) (y:Expression) = x + y
    let subtract (x:Expression) (y:Expression) = x - y
    let negate (x:Expression) = -x
    let plus (x:Expression) = +x
    let sum (xs:Expression list) = if xs.Length = 0 then zero else List.reduce (+) xs
    let sumSeq (xs:Expression seq) = Seq.fold (+) zero xs
    let multiply (x:Expression) (y:Expression) = x * y
    let divide (x:Expression) (y:Expression) = x / y
    let invert (x:Expression) = Expression.Invert(x)
    let product (xs:Expression list) = if xs.Length = 0 then one else List.reduce (*) xs
    let productSeq (xs:Expression seq) = Seq.fold (*) one xs
    let pow (x:Expression) (y:Expression) = x ** y

    let apply (f:Function) (x:Expression) = Expression.Apply (f, x)
    let applyN (f:Function) (xs:Expression list) = Expression.ApplyN (f, xs)

    let abs x = Expression.Abs x
    let exp x = Expression.Exp x
    let ln x = Expression.Ln x
    let sin x = Expression.Sin x
    let cos x = Expression.Cos x
    let tan x = Expression.Tan x
    let cot x = tan x |> invert
    let sec x = cos x |> invert
    let csc x = sin x |> invert


[<RequireQualifiedAccess>]
module Numbers =

    let max2 u v =
        match u, v with
        | Undefined, _ | _, Undefined -> Undefined
        | PositiveInfinity, _ | _, PositiveInfinity -> PositiveInfinity
        | NegativeInfinity, b | b, NegativeInfinity -> b
        | Number a, Number b -> Number (if b > a then b else a)
        | _ -> failwith "number expected"

    let min2 u v =
        match u, v with
        | Undefined, _ | _, Undefined -> Undefined
        | PositiveInfinity, b | b, PositiveInfinity -> b
        | NegativeInfinity, _ | _, NegativeInfinity -> NegativeInfinity
        | Number a, Number b -> Number (if b < a then b else a)
        | _ -> failwith "number expected"

    let max ax = List.reduce max2 ax
    let min ax = List.reduce min2 ax

    let compare x y =
        match x, y with
        | a, b when a = b -> 0
        | Number a, Number b -> compare a b
        | Number _, PositiveInfinity -> -1
        | Number _, NegativeInfinity -> 1
        | PositiveInfinity, Number _ -> 1
        | NegativeInfinity, Number _ -> -1
        | _ -> failwith "only numbers and +/-infinity are supported"


[<RequireQualifiedAccess>]
module Structure =

    open System.Collections.Generic
    open ExpressionPatterns
    open Operators

    let numberOfOperands = function
        | Sum ax | Product ax -> List.length ax
        | Power _ -> 2
        | Function _ -> 1
        | FunctionN (_, xs) -> List.length xs
        | Number _ | Identifier _ -> 0
        | PositiveInfinity | NegativeInfinity | ComplexInfinity | Undefined -> 0

    let operand i = function
        | Sum ax | Product ax | FunctionN (_, ax) -> List.nth ax i
        | Power (r, _) when i = 0 -> r
        | Power (_, p) when i = 1 -> p
        | Function (_, x) when i = 0 -> x
        | Number _ | Identifier _ -> failwith "numbers and identifiers have no operands"
        | _ -> failwith "no such operand"

    let rec freeOf symbol x =
        if symbol = x then false else
        match x with
        | Sum ax | Product ax | FunctionN (_, ax) -> List.forall (freeOf symbol) ax
        | Power (r, p) -> freeOf symbol r && freeOf symbol p
        | Function (_, x) -> freeOf symbol x
        | Number _ | Identifier _ -> true
        | PositiveInfinity | NegativeInfinity | ComplexInfinity | Undefined -> true

    let rec freeOfSet (symbols: HashSet<Expression>) x =
        if symbols.Contains(x) then false else
        match x with
        | Sum ax | Product ax | FunctionN (_, ax) -> List.forall (freeOfSet symbols) ax
        | Power (r, p) -> freeOfSet symbols r && freeOfSet symbols p
        | Function (_, x) -> freeOfSet symbols x
        | Number _ | Identifier _  -> true
        | PositiveInfinity | NegativeInfinity | ComplexInfinity | Undefined -> true

    let rec substitute y r x =
        if y = x then r else
        match x with
        | Sum ax -> sum <| List.map (substitute y r) ax
        | Product ax -> product <| List.map (substitute y r) ax
        | Power (radix, p) -> (substitute y r radix) ** (substitute y r p)
        | Function (fn, x) -> apply fn (substitute y r x)
        | FunctionN (fn, xs) -> applyN fn (List.map (substitute y r) xs)
        | Number _ | Identifier _ -> x
        | PositiveInfinity | NegativeInfinity | ComplexInfinity | Undefined -> x

    let map f = function
        | Sum ax -> sum <| List.map f ax
        | Product ax -> product <| List.map f ax
        | Power (r, p) -> (f r) ** (f p)
        | Function (fn, x) -> apply fn (f x)
        | FunctionN (fn, xs) -> applyN fn (List.map f xs)
        | _ as x -> x

    let fold f s = function
        | Sum ax | Product ax | FunctionN (_, ax) -> List.fold f s ax
        | Power (r, p) -> List.fold f s [r;p]
        | Function (_, x) -> f s x
        | _ -> s


[<RequireQualifiedAccess>]
module Algebraic =

    open ExpressionPatterns
    open Operators

    /// Splits a product into a tuple (free of a symbol, dependent on symbol)
    let separateFactors symbol x =
        match x with
        | Product ax -> let f, d = List.partition (Structure.freeOf symbol) ax in (product f, product d)
        | a when Structure.freeOf symbol a -> (a,one)
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
    let rec expand = function
        | Sum ax -> sum <| List.map expand ax
        | Product ax -> List.map expand ax |> List.reduce expandProduct
        | PosIntPower (r, Number n) -> expandPower (expand r) (int n)
        | x -> x

    /// Algebraically expand the main operator of the expression only
    let expandMain = function
        | Product ax -> List.reduce expandProduct ax
        | PosIntPower (r, Number n) -> expandPower r (int n)
        | x -> x
