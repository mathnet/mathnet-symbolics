namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

[<StructuralEquality;NoComparison>]
type Expression =
    | Number of BigRational
    | Identifier of Symbol
    | Sum of Expression list
    | Product of Expression list
    | Power of Expression * Expression
    | Function of Function * Expression
    | FunctionN of Function * (Expression list)

    static member Zero = Number BigRational.Zero
    static member One = Number BigRational.One
    static member Two = Number (BigRational.FromInt 2)
    static member MinusOne = Number (BigRational.FromInt -1)
    static member Undefined = Identifier Undefined
    static member PositiveInfinity = Identifier PositiveInfinity
    static member NegativeInfinity = Identifier NegativeInfinity
    static member ComplexInfinity = Identifier ComplexInfinity
    static member OfInt32 (x:int) = Number (BigRational.FromInt x)
    static member OfInteger (x:BigInteger) = Number (BigRational.FromBigInt x)
    static member OfRational (x:BigRational) = Number x


    static member private OrderRelation (x:Expression) (y:Expression) =
        let rec compare a b =
            match a, b with
            | Number x, Number y -> x < y
            | Identifier x, Identifier y -> x < y
            | Sum xs, Sum ys | Product xs, Product ys -> compareZip (List.rev xs) (List.rev ys)
            | Power (xr,xp), Power (yr,yp) -> if xr <> yr then compare xr yr else compare xp yp
            | Function (xf, x), Function (yf, y) -> if xf <> yf then xf < yf else compare x y
            | FunctionN (xf, xs), FunctionN (yf, ys) -> if xf <> yf then xf < yf else compareZip (List.rev xs) (List.rev ys)
            | Number _, _ -> true
            | _, Number _ -> false
            | Product xs, y -> compareZip (List.rev xs) [y]
            | x, Product ys -> compareZip [x] (List.rev ys)
            | Power (xr, xp), y -> if xr <> y then compare xr y else compare xp Expression.One
            | x, Power (yr, yp) -> if x <> yr then compare x yr else compare Expression.One yp
            | Sum xs, y -> compareZip (List.rev xs) [y]
            | x, Sum ys -> compareZip [x] (List.rev ys)
            | Function (xf, x), FunctionN (yf, ys) -> if xf <> yf then xf < yf else compareZip [x] (List.rev ys)
            | FunctionN (xf, xs), Function (yf, y) -> if xf <> yf then xf < yf else compareZip (List.rev xs) [y]
            | Function _, Identifier _ | FunctionN _, Identifier _ -> false
            | Identifier _, Function _ | Identifier _, FunctionN _ -> true
        and compareZip a b =
            match a, b with
            | x::xs, y::ys when x <> y -> compare x y
            | x::xs, y::ys -> compareZip xs ys
            | [], y::ys -> true
            | _, [] -> false
        compare x y


    static member ( ~+ ) (x:Expression) = x
    static member ( ~- ) (x:Expression) = Expression.MinusOne * x
    static member ( - ) ((x:Expression), (y:Expression)) = x + (-y)
    static member ( / ) (x, y) = x * Expression.Invert y

    static member ( + ) (x, y) =
        // none of the summands is allowed to be a sum
        // only the first summand is allowed to be a number

        /// Recognize terms of the form a*x -> (a,x) where a is a number
        let (|Term|_|) = function
            | Number _ -> None
            | Product [(Number a); b] -> Some (a, b)
            | Product ((Number a)::xs) -> Some (a, Product xs)
            | x -> Some (BigRational.One, x)

        let merge (xs:Expression list) (ys:Expression list) =
            let rec gen acc u v =
                match acc, u, v with
                | (Number n)::cc, _, _ when n = BigRational.Zero -> gen cc u v
                | Term(ac,at)::cc, Term(xc,xt)::xs, y | Term(ac,at)::cc, y, Term(xc,xt)::xs when at = xt ->
                    gen ((Number(ac+xc)*at)::cc) xs y
                | _, Term(xc,xt)::xs, Term(yc,yt)::ys when xt = yt ->
                    gen ((Number(xc+yc)*xt)::acc) xs ys
                | _, x::xs, y::ys ->
                    if Expression.OrderRelation x y then gen (x::acc) xs v
                    else gen (y::acc) u ys
                | _, x::xs, [] | _, [], x::xs -> gen (x::acc) xs []
                | _, [], [] -> acc
            match gen [] xs ys with
            | [x] -> x
            | [] -> Expression.Zero
            | x -> Sum (List.rev x)

        match x, y with
        | a, b | b, a when a = Expression.Zero -> b
        | Identifier Undefined, _ | _, Identifier Undefined -> Expression.Undefined
        | Identifier ComplexInfinity, _ | _, Identifier ComplexInfinity -> Expression.ComplexInfinity
        | Sum ((Number a)::ax), Sum ((Number b)::bx) -> (merge ax bx) + (Number (a+b))
        | Sum ((Number a)::ax), Sum bx | Sum bx, Sum ((Number a)::ax) -> (merge ax bx) + (Number a)
        | Sum ((Number a)::ax), Number b | Number b, Sum ((Number a)::ax) -> Sum (Number (a+b)::ax)
        | Sum ((Number a)::ax), b | b, Sum ((Number a)::ax) -> (merge ax [b]) + (Number a)
        | Sum ax, Sum bx -> merge ax bx
        | Sum ax, Number b | Number b, Sum ax -> Sum ((Number b)::ax)
        | Sum ax, b -> merge ax [b]
        | a, Sum bx -> merge [a] bx
        | Number a, Number b -> Number (a+b)
        | Number a, b | b, Number a -> Sum [Number a; b]
        | a, b -> merge [a] [b]

    static member ( * ) (x, y) =
        // none of the factors is allowed to be a product
        // only the first factor is allowed to be a number

        /// Recognize terms of the form r^p -> (r,p)
        let (|Term|_|) = function
            | Number _ -> None
            | Power (r,p) -> Some (r, p)
            | x -> Some (x, Expression.One)

        let merge (xs:Expression list) (ys:Expression list) =
            let rec gen acc u v =
                match acc, u, v with
                | (Number n)::cc, _, _ when n = BigRational.One -> gen cc u v
                | Term(ab,ae)::cc, Term(xb,xe)::xs, y | Term(ab,ae)::cc, y, Term(xb,xe)::xs when ab = xb ->
                    gen ((ab**(ae+xe))::cc) xs y
                | _, Term(xb,xe)::xs, Term(yb,ye)::ys when xb = yb ->
                    gen ((xb**(xe+ye))::acc) xs ys
                | _, x::xs, y::ys ->
                    if Expression.OrderRelation x y then gen (x::acc) xs v
                    else gen (y::acc) u ys
                | _, x::xs, y -> gen (x::acc) xs y
                | _, [], y::ys -> gen (y::acc) ys []
                | _, [], [] -> acc
            match gen [] xs ys with
            | [x] -> x
            | [] -> Expression.One
            | x -> Product (List.rev x)

        match x, y with
        | a, b | b, a when a = Expression.One -> b
        | a, _ | _, a when a = Expression.Zero -> Expression.Zero
        | Identifier Undefined, _ | _, Identifier Undefined -> Expression.Undefined
        | Identifier ComplexInfinity, _ | _, Identifier ComplexInfinity -> Expression.ComplexInfinity
        | Product ((Number a)::ax), Product ((Number b)::bx) -> (merge ax bx) * (Number (a*b))
        | Product ((Number a)::ax), Product bx | Product bx, Product ((Number a)::ax) -> (merge ax bx) * (Number a)
        | Product ((Number a)::ax), Number b | Number b, Product ((Number a)::ax) -> Product (Number (a*b)::ax)
        | Product ((Number a)::ax), b | b, Product ((Number a)::ax) -> (merge ax [b]) * (Number a)
        | Product ax, Product bx -> merge ax bx
        | Product ax, Number b | Number b, Product ax -> Product ((Number b)::ax)
        | Product ax, b -> merge ax [b]
        | a, Product bx -> merge [a] bx
        | Number a, Number b -> Number (a*b)
        | Number a, b | b, Number a -> Product [Number a; b]
        | a, b -> merge [a] [b]

    static member Pow (x, y) =
        // if power is a number, radix must not be an integer, fraction, product or power
        match x, y with
        | a, b when b = Expression.Zero && a = Expression.Zero -> Expression.Undefined
        | _, b when b = Expression.Zero -> Expression.One
        | a, b when b = Expression.One -> a
        | a, _ when a = Expression.One -> Expression.One
        | Identifier Undefined, _ | _, Identifier Undefined -> Expression.Undefined
        | Number a, Number b when b.IsInteger -> Number (BigRational.Pow(a, int(b.Numerator)))
        | Product ax, Number b when b.IsInteger -> Product (ax |> List.map (fun z -> Expression.Pow(z,y)))
        | Power (r, p), Number b when b.IsInteger -> Expression.Pow(r, p*y)
        | a, b -> Power(a, b)

    static member Invert (x) =
        match x with
        | Identifier Undefined -> Expression.Undefined
        | Identifier PositiveInfinity | Identifier NegativeInfinity | Identifier ComplexInfinity -> Expression.Zero
        | Number a when a.IsZero -> Expression.ComplexInfinity // no direction
        | Number a -> Number (BigRational.Reciprocal a)
        | Product ax -> Product (ax |> List.map (Expression.Invert))
        | Power (r, p) -> Power (r, -p)
        | x -> Power (x, Expression.MinusOne)

    // Simpler usage
    static member ( + ) (x, (y:int)) = x + Number (BigRational.FromInt y)
    static member ( + ) ((x:int), y) = Number (BigRational.FromInt x) + y
    static member ( - ) (x, (y:int)) = x - Number (BigRational.FromInt y)
    static member ( - ) ((x:int), y) = Number (BigRational.FromInt x) - y
    static member ( * ) (x, (y:int)) = x * Number (BigRational.FromInt y)
    static member ( * ) ((x:int), y) = Number (BigRational.FromInt x) * y
    static member ( / ) (x, (y:int)) = x / Number (BigRational.FromInt y)
    static member ( / ) ((x:int), y) = Number (BigRational.FromInt x) / y
    static member Pow (x, (y:int)) = Expression.Pow(x, Number (BigRational.FromInt y))



module ExpressionPatterns =

    let (|PosIntPower|_|) = function
        | Power (r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | _ -> None

    let (|NegIntPower|_|) = function
        | Power (r, (Number n as p)) when n.IsInteger && n.IsNegative -> Some (r, p)
        | _ -> None

    let (|NegRationalPower|_|) = function
        | Power (r, (Number n as p)) when n.IsNegative -> Some (r, p)
        | _ -> None

    let (|Infinity|_|) = function
        | Identifier PositiveInfinity -> Some Infinity
        | Identifier ComplexInfinity -> Some Infinity
        | Identifier NegativeInfinity -> Some Infinity
        | _ -> None
