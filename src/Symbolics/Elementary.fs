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
    let multiply (x:Expression) (y:Expression) = x * y
    let divide (x:Expression) (y:Expression) = x / y
    let invert (x:Expression) = Expression.Invert(x)
    let product (xs:Expression list) = if xs.Length = 0 then one else List.reduce (*) xs
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

    let numberOfOperands = function
        | Sum ax | Product ax -> List.length ax
        | Power _ -> 2
        | Number _ | Identifier _ -> 0

    let operand i = function
        | Sum ax | Product ax -> List.nth ax i
        | Power (r, p) -> if i = 0 then r else if i = 1 then p else failwith "no such operand"
        | Number _ | Identifier _ -> failwith "numbers and identifiers have no operands"

    let rec freeOf y x =
        if y = x then false else
        match x with
        | Sum ax | Product ax -> List.forall (freeOf y) ax
        | Power (r, p) -> freeOf y r && freeOf y p
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


/// Single-Value Polynomial (2*x+3*x^2)
module Polynomials =

    open Numbers
    open Elementary

    let rec isMonomial symbol = function
        | x when x = symbol -> true
        | Number _ -> true
        | Power (r, (Number (Integer n) as p)) when r = symbol && n > BigInteger.One -> true
        | Product ax -> List.forall (isMonomial symbol) ax
        | _ -> false

    let isPolynomial symbol = function
        | x when isMonomial symbol x -> true
        | Sum ax -> List.forall (isMonomial symbol) ax
        | _ -> false

    let rec degreeMonomial symbol = function
        | x when x = zero -> -infinity
        | x when x = symbol -> one
        | Number _ -> zero
        | Power (r, (Number (Integer n) as p)) when r = symbol && n > BigInteger.One -> p
        | Product ax -> sum <| List.map (degreeMonomial symbol) ax
        | _ -> undefined

    let rec coefficientMonomial symbol = function
        | x when x = symbol -> one
        | Number _ as x -> x
        | Power (r, (Number (Integer n) as p)) when r = symbol && n > BigInteger.One -> one
        | Product ax -> product <| List.map (coefficientMonomial symbol) ax
        | _ -> undefined

    let rec coefficientDegreeMonomial symbol = function
        | x when x = zero -> x, -infinity
        | x when x = symbol -> one, one
        | Number _ as x -> x, zero
        | Power (r, (Number (Integer n) as p)) when r = symbol && n > BigInteger.One -> one, p
        | Product ax ->
            let cds = List.map (coefficientDegreeMonomial symbol) ax
            product <| List.map fst cds, sum <| List.map snd cds
        | _ -> undefined, undefined

    let degree symbol x =
        let d = degreeMonomial symbol x
        if d <> undefined then d else
        match x with
        | Sum ax -> max <| List.map (degreeMonomial symbol) ax
        | _ -> undefined

    let coefficient symbol (k:int) x =
        let ke = number k
        let c, d = coefficientDegreeMonomial symbol x
        if d = ke then c else
        match x with
        | Sum ax -> List.map (coefficientDegreeMonomial symbol) ax |> List.filter (fun (_, d) -> d = ke) |> List.map fst |> sum
        | _ -> undefined

    let leadingCoefficientDegree symbol x =
        let c, d = coefficientDegreeMonomial symbol x
        if d <> undefined then c, d else
        match x with
        | Sum ax ->
            let cds = List.map (coefficientDegreeMonomial symbol) ax
            let degree = max <| List.map snd cds
            cds |> List.filter (fun (_, d) -> d = degree) |> List.map fst |> sum, degree
        | _ -> undefined, undefined

    let leadingCoefficient symbol x = leadingCoefficientDegree symbol x |> fst

    let coefficients symbol x =
        let rec collect symbol = function
            | x when x = symbol -> [1, one]
            | Number _ as a -> [0, a]
            | Power (r, (Number (Integer n) as p)) when r = symbol && n > BigInteger.One -> [int n, one]
            | Sum ax -> List.collect (collect symbol) ax
            | Product ax -> List.map (collect symbol) ax |> List.reduce (fun a b -> a |> List.fold (fun s (o1, e1) -> b |> List.fold (fun s (o2, e2) -> (o1+o2,e1*e2)::s) s) [])
            | _ -> []
        let c = collect symbol x
        let degree = c |> Seq.map fst |> Seq.max
        c |> List.fold (fun (s:Expression[]) (o,e) -> s.[o] <- s.[o] + e; s) (Array.create (degree+1) zero)