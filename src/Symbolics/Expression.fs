namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

open ConstantPatterns

[<StructuralEquality;NoComparison>]
type Expression =
    | Number of BigRational
    | Identifier of Symbol
    | Constant of Constant
    | Sum of Expression list
    | Product of Expression list
    | Power of Expression * Expression
    | Function of Function * Expression
    | FunctionN of Function * (Expression list)
    | Infinity
    | ComplexInfinity
    | Undefined

    static member Zero = Number BigRational.Zero
    static member One = Number BigRational.One
    static member Two = Number (BigRational.FromInt 2)
    static member MinusOne = Number (BigRational.FromInt -1)
    static member FromInt32 (x:int) = Number (BigRational.FromInt x)
    static member FromInt64 (x:int64) = Number (BigRational.FromBigInt (BigInteger(x)))
    static member FromInteger (x:BigInteger) = Number (BigRational.FromBigInt x)
    static member FromIntegerFraction (n:BigInteger, d:BigInteger) = Number (BigRational.FromBigIntFraction (n, d))
    static member FromRational (x:BigRational) = Number x
    static member Symbol (name:string) = Identifier (Symbol name)
    static member NegativeInfinity = Product [Expression.MinusOne; Infinity]
    static member Real (floatingPoint:float) =
        if Double.IsPositiveInfinity floatingPoint then Infinity
        elif Double.IsNegativeInfinity floatingPoint then Expression.NegativeInfinity
        elif Double.IsNaN floatingPoint then Undefined
        else Constant (Real floatingPoint)

    static member I = Constant I
    static member E = Constant E
    static member Pi = Constant Pi

    static member private OrderRelation (x:Expression) (y:Expression) =
        let rec compare a b =
            match a, b with
            | Number x, Number y -> x < y
            | Identifier x, Identifier y -> x < y
            | Constant x, Constant y -> x < y
            | Sum xs, Sum ys | Product xs, Product ys -> compareZip (List.rev xs) (List.rev ys)
            | Power (xr,xp), Power (yr,yp) -> if xr <> yr then compare xr yr else compare xp yp
            | Function (xf, x), Function (yf, y) -> if xf <> yf then xf < yf else compare x y
            | FunctionN (xf, xs), FunctionN (yf, ys) -> if xf <> yf then xf < yf else compareZip (List.rev xs) (List.rev ys)
            | Number _, _ -> true
            | _, Number _ -> false
            | Constant _, _ -> true
            | _, Constant _ -> false
            | Product xs, y -> compareZip (List.rev xs) [y]
            | x, Product ys -> compareZip [x] (List.rev ys)
            | Power (xr, xp), y -> if xr <> y then compare xr y else compare xp Expression.One
            | x, Power (yr, yp) -> if x <> yr then compare x yr else compare Expression.One yp
            | Sum xs, y -> compareZip (List.rev xs) [y]
            | x, Sum ys -> compareZip [x] (List.rev ys)
            | Function (xf, x), FunctionN (yf, ys) -> if xf <> yf then xf < yf else compareZip [x] (List.rev ys)
            | FunctionN (xf, xs), Function (yf, y) -> if xf <> yf then xf < yf else compareZip (List.rev xs) [y]
            | Identifier _, _ -> true
            | _, Identifier _ -> false
            | Infinity, _ -> true
            | _, Infinity -> false
            | ComplexInfinity, _ -> true
            | _, ComplexInfinity -> false
            | Undefined, _ -> false
            | _, Undefined -> true
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

        let rec numAdd n x =
            match x with
            | Number b -> Number (n+b)
            | Sum [] -> Number n
            | Sum [Number a] -> Number (a+n)
            | Sum [a] -> if n.IsZero then a else Sum [Number n; a]
            | Sum ((Number a)::ax) -> numAdd (a+n) (Sum ax)
            | Sum ax -> if n.IsZero then x else Sum (Number n::ax)
            | x -> if n.IsZero then x else Sum [Number n; x]

        match x, y with
        | a, b | b, a when a = Expression.Zero -> b
        | Undefined, _ | _, Undefined -> Undefined
        | ComplexInfinity, ComplexInfinity -> Undefined
        | ComplexInfinity, (Infinity | Product [Number _; Infinity]) | (Infinity | Product [Number _; Infinity]), ComplexInfinity -> Undefined
        | ComplexInfinity, _ | _, ComplexInfinity -> ComplexInfinity
        | Infinity, Product [Number n; Infinity] | Product [Number n; Infinity], Infinity when n.IsNegative -> Undefined
        | Infinity, _ | _, Infinity -> Infinity
        | Constant (Real a), Constant (Real b) -> Expression.Real (a+b)
        | Constant (Real r), Number n | Number n, Constant (Real r) -> Expression.Real (r + float n)
        | Number a, b | b, Number a -> numAdd a b
        | Sum ((Number a)::ax), Sum ((Number b)::bx) -> numAdd (a+b) (merge ax bx)
        | Sum ((Number a)::ax), Sum bx | Sum bx, Sum ((Number a)::ax) -> numAdd a (merge ax bx)
        | Sum ((Number a)::ax), b | b, Sum ((Number a)::ax) -> numAdd a (merge ax [b])
        | Sum ax, Sum bx -> merge ax bx
        | Sum ax, b -> merge ax [b]
        | a, Sum bx -> merge [a] bx
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

        /// Multiply a number with an expression (potentially a denormalized product)
        let rec numMul (n:BigRational) x =
            if n.IsZero then Expression.Zero else
            match x with
            | Number b -> Number (n*b)
            | Product [] -> Number n
            | Product [Number a] -> Number (a*n)
            | Product [a] -> if n.IsOne then a else Product [Number n; a]
            | Product ((Number a)::ax) -> numMul (a*n) (Product ax)
            | Product ax -> if n.IsOne then x else Product (Number n::ax)
            | x -> if n.IsOne then x else Product [Number n; x]

        match x, y with
        | a, b | b, a when a = Expression.One -> b
        | a, _ | _, a when a = Expression.Zero -> Expression.Zero
        | Undefined, _ | _, Undefined -> Undefined
        | ComplexInfinity, (Number _ | Constant _ | Infinity | ComplexInfinity) | (Number _ | Constant _ | Infinity | ComplexInfinity), ComplexInfinity -> ComplexInfinity
        | Infinity, Number n | Number n, Infinity -> if n.IsNegative then Expression.NegativeInfinity else Infinity
        | Infinity, (Constant PositiveReal | Infinity) | (Constant PositiveReal | Infinity), Infinity -> Infinity
        | Infinity, Constant NegativeReal | Constant NegativeReal, Infinity -> Expression.NegativeInfinity
        | Product [Number n; Infinity], (Constant PositiveReal | Infinity) | (Constant PositiveReal | Infinity), Product [Number n; Infinity] when n.IsNegative -> Expression.NegativeInfinity
        | Product [Number n; Infinity], Constant NegativeReal | Constant NegativeReal, Product [Number n; Infinity] when n.IsNegative -> Infinity
        | Constant (Real a), Constant (Real b) -> Expression.Real (a*b)
        | Constant (Real r), Number n | Number n, Constant (Real r) -> Expression.Real (r * float n)
        | Number a, b | b, Number a -> numMul a b
        | Product ((Number a)::ax), Product ((Number b)::bx) -> numMul (a*b) (merge ax bx)
        | Product ((Number a)::ax), Product bx | Product bx, Product ((Number a)::ax) -> numMul a (merge ax bx)
        | Product ((Number a)::ax), b | b, Product ((Number a)::ax) -> numMul a (merge ax [b])
        | Product ax, Product bx -> merge ax bx
        | Product ax, b -> merge ax [b]
        | a, Product bx -> merge [a] bx
        | a, b -> merge [a] [b]

    static member Pow (x, y) =
        // if power is a number, radix must not be an integer, fraction, product or power
        match x, y with
        | a, b when a = Expression.Zero && b = Expression.Zero -> Undefined
        | _, b when b = Expression.Zero -> Expression.One
        | a, b when b = Expression.One -> a
        | a, _ when a = Expression.One -> Expression.One
        | Undefined, _ | _, Undefined -> Undefined
        | Number a, Number b when b.IsInteger ->
            if b.IsNegative then
                if a.IsZero then ComplexInfinity
                // workaround bug in BigRational with negative powers - drop after upgrading to > v3.0.0-alpha9
                else Number (BigRational.Pow(BigRational.Reciprocal a, -int(b.Numerator)))
            else Number (BigRational.Pow(a, int(b.Numerator)))
        | Product ax, Number b when b.IsInteger -> Product (ax |> List.map (fun z -> Expression.Pow(z,y)))
        | Power (r, p), Number b when b.IsInteger -> Expression.Pow(r, p*y)
        | a, b -> Power(a, b)

    static member Invert (x) =
        match x with
        | Undefined -> Undefined
        | Infinity | ComplexInfinity -> Expression.Zero
        | Number a when a.IsZero -> ComplexInfinity // no direction
        | Number a -> Number (BigRational.Reciprocal a)
        | Product ax -> Product (ax |> List.map (Expression.Invert))
        | Power (r, p) -> Expression.Pow(r, -p)
        | x -> Power (x, Expression.MinusOne)

    static member Abs (x) =
        match x with
        | Number n when n.IsNegative -> Number -n
        | Number n -> x
        | Product ((Number n)::ax) when n.IsNegative -> Function (Abs, (Number -n) * Product ax)
        | x -> Function (Abs, x)

    static member Exp (x) = if x = Expression.Zero then Expression.One else Function (Exp, x)
    static member Ln (x) = if x = Expression.One then Expression.Zero else Function (Ln, x)

    static member Sin (x) =
        match x with
        | Number n when n.IsZero -> Expression.Zero
        | Number n when n.IsNegative -> -Function (Sin, Number -n)
        | Product ((Number n)::ax) when n.IsNegative -> -Function (Sin, (Number -n) * Product ax)
        | x -> Function (Sin, x)

    static member Cos (x) =
        match x with
        | Number n when n.IsZero -> Expression.One
        | Number n when n.IsNegative -> Function (Cos, Number -n)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Cos, (Number -n) * Product ax)
        | x -> Function (Cos, x)

    static member Tan (x) =
        match x with
        | Number n when n.IsZero -> Expression.Zero
        | Number n when n.IsNegative -> -Function (Tan, Number -n)
        | Product ((Number n)::ax) when n.IsNegative -> -Function (Tan, (Number -n) * Product ax)
        | x -> Function (Tan, x)

    static member Apply (f, x) =
        match f with
        | Abs -> Expression.Abs x
        | Exp -> Expression.Exp x
        | Ln -> Expression.Ln x
        | Sin -> Expression.Sin x
        | Cos -> Expression.Cos x
        | Tan -> Expression.Tan x

    static member ApplyN (f, xs) = FunctionN (f, xs)


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

    // Simpler usage in C#
    static member op_Implicit (x:int) = Expression.FromInt32(x)
    static member op_Implicit (x:int64) = Expression.FromInt64(x)
    static member op_Implicit (x:BigInteger) = Expression.FromInteger(x)
    static member op_Implicit (x:BigRational) = Expression.FromRational(x)
    static member op_Implicit (name:string) = Expression.Symbol(name)


[<RequireQualifiedAccess>]
module NumericLiteralQ =
    let FromZero () = Expression.Zero
    let FromOne () = Expression.One
    let FromInt32 (x:int) = Expression.FromInt32 x
    let FromInt64 (x:int64) = Expression.FromInt64 x
    let FromString str = Expression.FromRational (BigRational.Parse str)


module ExpressionPatterns =

    let (|Integer|_|) = function
        | Number n when n.IsInteger -> Some (n)
        | _ -> None

    let (|Zero|One|MinusOne|Other|) = function
        | Number n when n.IsZero -> Zero
        | Number n when n.IsOne -> One
        | Number n when n.IsInteger && n.Numerator = BigInteger.MinusOne -> MinusOne
        | _ -> Other

    let (|NegativeInfinity|_|) = function
        | Product [MinusOne; Infinity] -> Some NegativeInfinity
        | _ -> None

    let (|PosIntPower|_|) = function
        | Power (r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | _ -> None

    let (|NegIntPower|_|) = function
        | Power (r, (Number n as p)) when n.IsInteger && n.IsNegative -> Some (r, p)
        | _ -> None

    let (|NegRationalPower|_|) = function
        | Power (r, (Number n as p)) when n.IsNegative -> Some (r, p)
        | _ -> None

    /// Terminal node, either a number, identifier/symbol or constant (including infinity).
    /// Warning: Undefined is *not* included.
    let (|Terminal|_|) = function
        | Number _ | Identifier _ | Constant _ as t -> Some t
        | _ -> None

    /// Recognizes a sin or cos expression
    let (|SinCos|_|) = function
        | Function (Sin, _) | Function (Cos, _) as t -> Some t
        | _ -> None

    let (|SinCosPosIntPower|_|) = function
        | Function (Sin, _) | Function (Cos, _) as r -> Some (r, Expression.One)
        | Power (Function (Sin, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | Power (Function (Cos, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | _ -> None
