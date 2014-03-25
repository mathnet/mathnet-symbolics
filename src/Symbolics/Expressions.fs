namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

type Symbol =
    | Symbol of string
    | Undefined
    | Infinity

type Expression =
    | Number of Number
    | Identifier of Symbol
    | Sum of Expression list
    | Product of Expression list
    | Power of Expression * Expression

    static member One = Number (Number.One)
    static member MinusOne = Number (Number.MinusOne)
    static member Zero = Number (Number.Zero)
    static member Undefined = Identifier Undefined
    static member Infinity = Identifier Infinity

    static member ( ~+ ) (x:Expression) = x
    static member ( ~- ) (x:Expression) = Expression.MinusOne * x
    static member ( - ) ((x:Expression), (y:Expression)) = x + (-y)
    static member ( / ) (x, y) = x * Expression.Invert y

    static member ( + ) (x, y) =
        // none of the summands is allowed to be a sum
        // only the first summand is allowed to be a number
        match x, y with
        | a, b | b, a when a = Expression.Zero -> b
        | a, b | b, a when a = Expression.Undefined -> Expression.Undefined
        | Sum ((Number a)::ax), Sum ((Number b)::bx) -> Sum (Number (a+b)::(ax @ bx))
        | Sum ((Number a)::ax), Sum bx | Sum bx, Sum ((Number a)::ax) -> Sum (Number a::(ax @ bx))
        | Sum ((Number a)::ax), Number b | Number b, Sum ((Number a)::ax) -> Sum (Number (a+b)::ax)
        | Sum ((Number a)::ax), b | b, Sum ((Number a)::ax) -> Sum (Number a::b::ax)
        | Sum ax, b | b, Sum ax -> Sum (b::ax)
        | Number a, Number b -> Number (a+b)
        | Number a, b | b, Number a -> Sum [Number a; b]
        | a, b -> Sum [a; b]

    static member ( * ) (x, y) =
        // none of the factors is allowed to be a product
        // only the first factor is allowed to be a number
        match x, y with
        | a, b | b, a when a = Expression.One -> b
        | a, b | b, a when a = Expression.Zero -> Expression.Zero
        | a, b | b, a when a = Expression.Undefined -> Expression.Undefined
        | Product ((Number a)::ax), Product ((Number b)::bx) -> Product (Number (a*b)::(ax @ bx))
        | Product ((Number a)::ax), Product bx | Product bx, Product ((Number a)::ax) -> Product (Number a::(ax @ bx))
        | Product ((Number a)::ax), Number b | Number b, Product ((Number a)::ax) -> Product (Number (a*b)::ax)
        | Product ((Number a)::ax), b | b, Product ((Number a)::ax) -> Product (Number a::b::ax)
        | Product ax, b | b, Product ax -> Product (b::ax)
        | Number a, Number b -> Number (a*b)
        | Number a, b | b, Number a -> Product [Number a; b]
        | a, b -> Product [a; b]

    static member Pow (x, y) =
        // if power is a number, radix must not be an integer, fraction, product or power
        match x, y with
        | a, b when b = Expression.One -> a
        | a, b | b, a when a = Expression.Undefined -> Expression.Undefined
        | Number a, Number (Integer b) -> Number (a ** int b)
        | Product ax, Number (Integer b) -> Product (ax |> List.map (fun z -> Expression.Pow(z,y)))
        | Power (r, p), Number (Integer b) -> Power (r, p*y)
        | a, b -> Power(a, b)

    static member Invert (x) =
        match x with
        | a when a = Expression.Undefined -> Expression.Undefined
        | a when a = Expression.Infinity -> Expression.Zero
        | a when a = Expression.Zero -> Expression.Undefined // no direction
        | Number a -> Number (Number.Invert a)
        | Product ax -> Product (ax |> List.map (Expression.Invert))
        | Power (r, p) -> Power (r, -p)
        | x -> Power (x, Expression.MinusOne)

    // Simpler usage
    static member ( + ) (x, (y:int)) = x + Number (Integer (BigInteger(y)))
    static member ( + ) ((x:int), y) = Number (Integer (BigInteger(x))) + y
    static member ( - ) (x, (y:int)) = x - Number (Integer (BigInteger(y)))
    static member ( - ) ((x:int), y) = Number (Integer (BigInteger(x))) - y
    static member ( * ) (x, (y:int)) = x * Number (Integer (BigInteger(y)))
    static member ( * ) ((x:int), y) = Number (Integer (BigInteger(x))) * y
    static member ( / ) (x, (y:int)) = x / Number (Integer (BigInteger(y)))
    static member ( / ) ((x:int), y) = Number (Integer (BigInteger(x))) / y
    static member Pow (x, (y:int)) = Expression.Pow(x, Number (Integer (BigInteger(y))))
