namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics


[<AutoOpen>]
module Core =

    let symbol name = Identifier (Symbol name)
    let undefined = Identifier Undefined
    let infinity = Identifier Infinity
    let number (x:int) = Number (Integer (BigInteger(x)))
    let zero = Expression.Zero
    let one = Expression.One
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


module Numbers =

    let max2 a b =
        match a, b with
        | a, b | b, a when a = undefined -> a
        | a, b | b, a when a = infinity -> a
        | a, b | b, a when a = -infinity -> b
        | Number a, Number b -> Number (Number.Max(a, b))
        | _ -> failwith "number expected"

    let min2 a b =
        match a, b with
        | a, b | b, a when a = undefined -> a
        | a, b | b, a when a = infinity -> b
        | a, b | b, a when a = -infinity -> a
        | Number a, Number b -> Number (Number.Min(a, b))
        | _ -> failwith "number expected"

    let max ax = List.reduce max2 ax
    let min ax = List.reduce min2 ax


module Elementary =

    open System.Collections.Generic

    let numberOfOperands = function
        | Sum ax | Product ax -> List.length ax
        | Power _ -> 2
        | Number _ | Identifier _ -> 0

    let operand i = function
        | Sum ax | Product ax -> List.nth ax i
        | Power (r, p) -> if i = 0 then r else if i = 1 then p else failwith "no such operand"
        | Number _ | Identifier _ -> failwith "numbers and identifiers have no operands"

    let rec freeOf symbol x =
        if symbol = x then false else
        match x with
        | Sum ax | Product ax -> List.forall (freeOf symbol) ax
        | Power (r, p) -> freeOf symbol r && freeOf symbol p
        | Number _ | Identifier _ -> true

    let rec freeOfSet (symbols: Set<Expression>) x =
        if symbols.Contains(x) then false else
        match x with
        | Sum ax | Product ax -> List.forall (freeOfSet symbols) ax
        | Power (r, p) -> freeOfSet symbols r && freeOfSet symbols p
        | Number _ | Identifier _ -> true

    let rec map f = function
        | Sum ax -> sum <| List.map f ax
        | Product ax -> product <| List.map f ax
        | Power (r, p) -> (f r) ** (f p)
        | _ as x -> x

    let rec substitute y r x =
        if y = x then r else
        match x with
        | Sum ax -> sum <| List.map (substitute y r) ax
        | Product ax -> product <| List.map (substitute y r) ax
        | Power (radix, p) -> (substitute y r radix) ** (substitute y r p)
        | Number _ | Identifier _ -> x

    let rec numerator = function
        | Product ax -> product <| List.map numerator ax
        | Power (r, Number (Integer n)) when n < BigInteger.Zero -> one
        | z -> z

    let rec denominator = function
        | Product ax -> product <| List.map denominator ax
        | Power (r, (Number (Integer n) as p)) when n < BigInteger.Zero -> r ** -p
        | _ -> one
