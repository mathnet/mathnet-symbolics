namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

[<StructuralEquality;NoComparison>]
type Expression =
    | Number of BigRational
    | Approximation of Approximation
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


module ExpressionPatterns =

    let (|Zero|_|) = function
        | Number n when n.IsZero -> Some Zero
        | Approximation (Double x) when x = 0.0 -> Some Zero
        | _ -> None

    let (|One|_|) = function
        | Number n when n.IsOne -> Some One
        | Approximation (Double x) when x = 1.0 -> Some One
        | _ -> None

    let (|MinusOne|_|) = function
        | Number n when n.IsInteger && n.Numerator = BigInteger.MinusOne -> Some MinusOne
        | Approximation (Double x) when x = -1.0 -> Some MinusOne
        | _ -> None

    let (|NegativeInfinity|_|) = function
        | Product [Number n; Infinity] when n.IsNegative -> Some NegativeInfinity
        | _ -> None

    let (|Negative|_|) = function
        | Number n when n.IsNegative -> Some Negative
        | Approximation (Double x) when x < 0.0 -> Some Negative
        | NegativeInfinity -> Some Negative
        | _ -> None

    let (|Positive|_|) = function
        | Number n when n.IsPositive -> Some Positive
        | Constant E | Constant Pi -> Some Positive
        | Approximation (Double x) when x > 0.0 -> Some Positive
        | Infinity -> Some Positive
        | _ -> None

    let (|Integer|_|) = function
        | Number n when n.IsInteger -> Some (n)
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
        | Function (Sin, _) | Function (Cos, _) as r -> Some (r, Number BigRational.One)
        | Power (Function (Sin, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | Power (Function (Cos, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | _ -> None


module Approx =

    let sum = function | Double a, Double b -> Double (a+b)
    let product = function | Double a, Double b -> Double (a*b)

    let fromRational (x:BigRational) = Double (float x)


module Operators =

    open ExpressionPatterns

    let zero = Number BigRational.Zero
    let one = Number BigRational.One
    let two = Number (BigRational.FromInt 2)
    let minusOne = Number (BigRational.FromInt -1)
    let pi = Constant Pi

    let symbol (name:string) = Identifier (Symbol name)

    let undefined = Expression.Undefined
    let infinity = Expression.Infinity
    let complexInfinity = Expression.ComplexInfinity
    let negativeInfinity = Product [minusOne; Infinity]

    let approximation = function
        | Double x ->
            if Double.IsPositiveInfinity x then Infinity
            elif Double.IsNegativeInfinity x then negativeInfinity
            elif Double.IsNaN x then Undefined
            else Approximation (Double x)

    let real floatingPoint = approximation (Double floatingPoint)

    let fromInt32 (x:int) = Number (BigRational.FromInt x)
    let fromInt64 (x:int64) = Number (BigRational.FromBigInt (BigInteger(x)))
    let fromInteger (x:BigInteger) = Number (BigRational.FromBigInt x)
    let fromIntegerFraction (n:BigInteger) (d:BigInteger) = Number (BigRational.FromBigIntFraction (n, d))
    let fromRational (x:BigRational) = Number x

    let number = fromInt32

    let isZero = function | Zero -> true | _ -> false
    let isOne = function | One -> true | _ -> false
    let isMinusOne = function | MinusOne -> true | _ -> false
    let isPositive = function | Positive -> true | _ -> false
    let isNegative = function | Negative -> true | _ -> false
    let isPositiveInfinity = function | Infinity -> true | _ -> false
    let isNegativeInfinity = function | NegativeInfinity -> true | _ -> false
    let isComplexInfinity = function | ComplexInfinity -> true | _ -> false
    let isInfinity = function | Infinity | ComplexInfinity | NegativeInfinity -> true | _ -> false

    let internal orderRelation (x:Expression) (y:Expression) =
        let rec compare a b =
            match a, b with
            | Number x, Number y -> x < y
            | Approximation x, Approximation y -> x < y
            | Identifier x, Identifier y -> x < y
            | Constant x, Constant y -> x < y
            | Sum xs, Sum ys | Product xs, Product ys -> compareZip (List.rev xs) (List.rev ys)
            | Power (xr,xp), Power (yr,yp) -> if xr <> yr then compare xr yr else compare xp yp
            | Function (xf, x), Function (yf, y) -> if xf <> yf then xf < yf else compare x y
            | FunctionN (xf, xs), FunctionN (yf, ys) -> if xf <> yf then xf < yf else compareZip (List.rev xs) (List.rev ys)
            | Number _, _ -> true
            | _, Number _ -> false
            | Approximation _, _ -> true
            | _, Approximation _ -> false
            | Constant _, _ -> true
            | _, Constant _ -> false
            | Product xs, y -> compareZip (List.rev xs) [y]
            | x, Product ys -> compareZip [x] (List.rev ys)
            | Power (xr, xp), y -> if xr <> y then compare xr y else compare xp one
            | x, Power (yr, yp) -> if x <> yr then compare x yr else compare one yp
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

    let rec add x y =
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
                | Zero::cc, _, _ -> gen cc u v
                | Term(ac,at)::cc, Term(xc,xt)::xs, y | Term(ac,at)::cc, y, Term(xc,xt)::xs when at = xt ->
                    gen ((multiply (Number(ac+xc)) at)::cc) xs y
                | _, Term(xc,xt)::xs, Term(yc,yt)::ys when xt = yt ->
                    gen ((multiply (Number(xc+yc)) xt)::acc) xs ys
                | _, x::xs, y::ys ->
                    if orderRelation x y then gen (x::acc) xs v
                    else gen (y::acc) u ys
                | _, x::xs, [] | _, [], x::xs -> gen (x::acc) xs []
                | _, [], [] -> acc
            match gen [] xs ys with
            | [x] -> x
            | [] -> zero
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
        | Zero, b | b, Zero -> b
        | Undefined, _ | _, Undefined -> undefined
        | ComplexInfinity, ComplexInfinity -> undefined
        | ComplexInfinity, (Infinity | NegativeInfinity) | (Infinity | NegativeInfinity), ComplexInfinity -> undefined
        | ComplexInfinity, _ | _, ComplexInfinity -> complexInfinity
        | Infinity, NegativeInfinity | NegativeInfinity, Infinity -> undefined
        | Infinity, _ | _, Infinity -> infinity
        | Approximation a, Approximation b -> Approx.sum (a, b) |> approximation
        | Approximation r, Number n | Number n, Approximation r -> Approx.sum (r, Approx.fromRational n) |> approximation
        | Number a, b | b, Number a -> numAdd a b
        | Sum ((Number a)::ax), Sum ((Number b)::bx) -> numAdd (a+b) (merge ax bx)
        | Sum ((Number a)::ax), Sum bx | Sum bx, Sum ((Number a)::ax) -> numAdd a (merge ax bx)
        | Sum ((Number a)::ax), b | b, Sum ((Number a)::ax) -> numAdd a (merge ax [b])
        | Sum ax, Sum bx -> merge ax bx
        | Sum ax, b -> merge ax [b]
        | a, Sum bx -> merge [a] bx
        | a, b -> merge [a] [b]

    and multiply x y =
        // none of the factors is allowed to be a product
        // only the first factor is allowed to be a number

        /// Recognize terms of the form r^p -> (r,p)
        let (|Term|_|) = function
            | Number _ -> None
            | Power (r,p) -> Some (r, p)
            | x -> Some (x, one)

        let merge (xs:Expression list) (ys:Expression list) =
            let rec gen acc u v =
                match acc, u, v with
                | One::cc, _, _ -> gen cc u v
                | Term(ab,ae)::cc, Term(xb,xe)::xs, y | Term(ab,ae)::cc, y, Term(xb,xe)::xs when ab = xb ->
                    gen ((pow ab (add ae xe))::cc) xs y
                | _, Term(xb,xe)::xs, Term(yb,ye)::ys when xb = yb ->
                    gen ((pow xb (add xe ye))::acc) xs ys
                | _, x::xs, y::ys ->
                    if orderRelation x y then gen (x::acc) xs v
                    else gen (y::acc) u ys
                | _, x::xs, y -> gen (x::acc) xs y
                | _, [], y::ys -> gen (y::acc) ys []
                | _, [], [] -> acc
            match gen [] xs ys with
            | [x] -> x
            | [] -> one
            | x -> Product (List.rev x)

        /// Multiply a number with an expression (potentially a denormalized product)
        let rec numMul (n:BigRational) x =
            if n.IsZero then zero else
            match x with
            | Number b -> Number (n*b)
            | Product [] -> Number n
            | Product [Number a] -> Number (a*n)
            | Product [a] -> if n.IsOne then a else Product [Number n; a]
            | Product ((Number a)::ax) -> numMul (a*n) (Product ax)
            | Product ax -> if n.IsOne then x else Product (Number n::ax)
            | x -> if n.IsOne then x else Product [Number n; x]

        match x, y with
        | One, b | b, One -> b
        | Zero, _ | _, Zero -> zero
        | Undefined, _ | _, Undefined -> undefined
        | ComplexInfinity, (Number _ | Constant _ | Infinity | ComplexInfinity) | (Number _ | Constant _ | Infinity | ComplexInfinity), ComplexInfinity -> complexInfinity
        | Infinity, Number n | Number n, Infinity -> if n.IsNegative then negativeInfinity else infinity
        | Infinity, Positive | Positive, Infinity -> infinity
        | Infinity, Negative | Negative, Infinity -> negativeInfinity
        | NegativeInfinity, Positive | Positive, NegativeInfinity -> negativeInfinity
        | NegativeInfinity, Negative | Negative, NegativeInfinity -> infinity
        | Approximation a, Approximation b -> Approx.product (a, b) |> approximation
        | Approximation r, Number n | Number n, Approximation r -> Approx.product (r, Approx.fromRational n) |> approximation
        | Number a, b | b, Number a -> numMul a b
        | Product ((Number a)::ax), Product ((Number b)::bx) -> numMul (a*b) (merge ax bx)
        | Product ((Number a)::ax), Product bx | Product bx, Product ((Number a)::ax) -> numMul a (merge ax bx)
        | Product ((Number a)::ax), b | b, Product ((Number a)::ax) -> numMul a (merge ax [b])
        | Product ax, Product bx -> merge ax bx
        | Product ax, b -> merge ax [b]
        | a, Product bx -> merge [a] bx
        | a, b -> merge [a] [b]

    and pow x y =
        // if power is a number, radix must not be an integer, fraction, product or power
        match x, y with
        | Zero, Zero -> undefined
        | _, Zero -> one
        | a, One -> a
        | One, _ -> one
        | Undefined, _ | _, Undefined -> undefined
        | Number a, Number b when b.IsInteger ->
            if b.IsNegative then
                if a.IsZero then complexInfinity
                // workaround bug in BigRational with negative powers - drop after upgrading to > v3.0.0-alpha9
                else Number (BigRational.Pow(BigRational.Reciprocal a, -int(b.Numerator)))
            else Number (BigRational.Pow(a, int(b.Numerator)))
        | Product ax, Number b when b.IsInteger -> Product (ax |> List.map (fun z -> pow z y))
        | Power (r, p), Number b when b.IsInteger -> pow r (multiply p y)
        | a, b -> Power(a, b)

    let plus x = x
    let negate x = multiply minusOne x
    let subtract x y = add x (negate y)

    let rec invert = function
        | Undefined -> undefined
        | Infinity | ComplexInfinity -> zero
        | Number a when a.IsZero -> complexInfinity // no direction
        | Number a -> Number (BigRational.Reciprocal a)
        | Product ax -> Product (ax |> List.map invert)
        | Power (r, p) -> pow r (negate p)
        | x -> Power (x, minusOne)

    let divide x y = multiply x (invert y)

    let sum (xs:Expression list) = if List.isEmpty xs then zero else List.reduce add xs
    let sumSeq (xs:Expression seq) = Seq.fold add zero xs
    let product (xs:Expression list) = if List.isEmpty xs then one else List.reduce multiply xs
    let productSeq (xs:Expression seq) = Seq.fold multiply one xs

    let root n x = Power (x, Power(n, minusOne))
    let sqrt x = root two x

    let abs = function
        | Number n when n.IsNegative -> Number -n
        | Number n as x -> x
        | Product ((Number n)::ax) when n.IsNegative -> Function (Abs, multiply (Number -n) (Product ax))
        | x -> Function (Abs, x)

    let exp = function
        | Zero -> one
        | x -> Function (Exp, x)
    let ln = function
        | One -> zero
        | x -> Function (Ln, x)
    let log basis x = divide (ln x) (ln basis)

    let sin = function
        | Zero -> zero
        | Number n when n.IsNegative -> negate (Function (Sin, Number -n))
        | Product ((Number n)::ax) when n.IsNegative -> negate (Function (Sin, multiply (Number -n) (Product ax)))
        | x -> Function (Sin, x)
    let cos = function
        | Zero -> one
        | Number n when n.IsNegative -> Function (Cos, Number -n)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Cos, multiply (Number -n) (Product ax))
        | x -> Function (Cos, x)
    let tan = function
        | Zero -> zero
        | Number n when n.IsNegative -> negate (Function (Tan, Number -n))
        | Product ((Number n)::ax) when n.IsNegative -> negate (Function (Tan, multiply (Number -n) (Product ax)))
        | x -> Function (Tan, x)
    let cot x = tan x |> invert
    let sec x = cos x |> invert
    let csc x = sin x |> invert
    let cosh x = Function (Cosh, x)
    let sinh x = Function (Sinh, x)
    let tanh x = Function (Tanh, x)
    let arcsin x = Function (ArcSin, x)
    let arccos x = Function (ArcCos, x)
    let arctan x = Function (ArcTan, x)

    let apply f x =
        match f with
        | Abs -> abs x
        | Exp -> exp x
        | Ln -> ln x
        | Sin -> sin x
        | Cos -> cos x
        | Tan -> tan x
        | Cosh -> cosh x
        | Sinh -> sinh x
        | Tanh -> tanh x
        | ArcSin -> arcsin x
        | ArcCos -> arccos x
        | ArcTan -> arctan x

    let applyN (f: Function) (xs: Expression list) = failwith "not supported yet"


type Expression with

    static member Zero = Operators.zero
    static member One = Operators.one
    static member Two = Operators.two
    static member MinusOne = Operators.minusOne
    static member FromInt32 (x:int) = Operators.fromInt32 x
    static member FromInt64 (x:int64) = Operators.fromInt64 x
    static member FromInteger (x:BigInteger) = Operators.fromInteger x
    static member FromIntegerFraction (n:BigInteger, d:BigInteger) = Operators.fromIntegerFraction n d
    static member FromRational (x:BigRational) = Operators.fromRational x
    static member Symbol (name:string) = Operators.symbol name
    static member NegativeInfinity = Operators.negativeInfinity
    static member Real (floatingPoint:float) = Operators.real floatingPoint

    static member I = Constant I
    static member E = Constant E
    static member Pi = Operators.pi

    static member ( ~+ ) (x:Expression) = Operators.plus x
    static member ( ~- ) (x:Expression) = Operators.negate x
    static member ( + ) ((x:Expression), (y:Expression)) = Operators.add x y
    static member ( - ) ((x:Expression), (y:Expression)) = Operators.subtract x y
    static member ( * ) ((x:Expression), (y:Expression)) = Operators.multiply x y
    static member ( / ) ((x:Expression), (y:Expression)) = Operators.divide x y

    static member Pow (x, y) = Operators.pow x y
    static member Invert (x) = Operators.invert x

    static member Abs (x) = Operators.abs x

    static member Root (n, x) = Operators.root n x
    static member Sqrt (x) = Operators.sqrt x

    static member Exp (x) = Operators.exp x
    static member Ln (x) = Operators.ln x
    static member Log (basis, x) = Operators.log basis x

    static member Sin (x) = Operators.sin x
    static member Cos (x) = Operators.cos x
    static member Tan (x) = Operators.tan x
    static member Cosh (x) = Operators.cosh x
    static member Sinh (x) = Operators.sinh x
    static member Tanh (x) = Operators.tanh x
    static member ArcSin (x) = Operators.arcsin x
    static member ArcCos (x) = Operators.arccos x
    static member ArcTan (x) = Operators.arctan x

    static member Apply (f, x) = Operators.apply f x
    static member ApplyN (f, xs) = Operators.applyN f xs

    // Simpler usage - numbers
    static member ( + ) (x, (y:int)) = x + (Operators.number y)
    static member ( + ) ((x:int), y) = (Operators.number x) + y
    static member ( - ) (x, (y:int)) = x - (Operators.number y)
    static member ( - ) ((x:int), y) = (Operators.number x) - y
    static member ( * ) (x, (y:int)) = x * (Operators.number y)
    static member ( * ) ((x:int), y) = (Operators.number x) * y
    static member ( / ) (x, (y:int)) = x / (Operators.number y)
    static member ( / ) ((x:int), y) = (Operators.number x) / y
    static member Pow (x, (y:int)) = Operators.pow x (Operators.number y)

    // Simpler usage in C#
    static member op_Implicit (x:int) = Operators.fromInt32(x)
    static member op_Implicit (x:int64) = Operators.fromInt64(x)
    static member op_Implicit (x:BigInteger) = Operators.fromInteger(x)
    static member op_Implicit (x:BigRational) = Operators.fromRational(x)
    static member op_Implicit (name:string) = Operators.symbol(name)


[<RequireQualifiedAccess>]
module NumericLiteralQ =
    let FromZero () = Expression.Zero
    let FromOne () = Expression.One
    let FromInt32 (x:int) = Expression.FromInt32 x
    let FromInt64 (x:int64) = Expression.FromInt64 x
    let FromString str = Expression.FromRational (BigRational.Parse str)
