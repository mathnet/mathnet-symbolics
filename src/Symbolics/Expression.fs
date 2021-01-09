namespace MathNet.Symbolics

open MathNet.Numerics
open MathNet.Symbolics

[<StructuralEquality;NoComparison>]
type Expression =
    | Number of BigRational
    | Approximation of Approximation
    | Identifier of Symbol
    | Argument of Symbol
    | Constant of Constant
    | Sum of Expression list
    | Product of Expression list
    | Power of Expression * Expression
    | Function of Function * Expression
    | FunctionN of FunctionN * (Expression list)
    //| FunctionDef of Symbol * (Symbol list) * Expression
    | ComplexInfinity
    | PositiveInfinity
    | NegativeInfinity
    | Undefined

[<RequireQualifiedAccess>]
module Values =

    let (|Value|_|) = function
        | Number n -> Some (Value.Number n)
        | Approximation a -> Some (Value.Approximation a)
        | ComplexInfinity -> Some Value.ComplexInfinity
        | PositiveInfinity -> Some Value.PositiveInfinity
        | NegativeInfinity -> Some Value.NegativeInfinity
//        | Undefined -> Some Value.Undefined
        | _ -> None

    let unpack = function
        | Value.Number n -> Number n
        | Value.Approximation a -> Approximation a
        | Value.ComplexInfinity -> ComplexInfinity
        | Value.PositiveInfinity -> PositiveInfinity
        | Value.NegativeInfinity -> NegativeInfinity
        | Value.Undefined -> Undefined

    let negate a = Value.negate a |> unpack
    let abs a = Value.abs a |> unpack

    let sum (a, b) = Value.sum (a, b) |> unpack
    let product (a, b) = Value.product (a, b) |> unpack
    let invert a = Value.invert a |> unpack
    let power (a, b) = Value.power (a, b) |> unpack

    let apply f x = Value.apply f x |> unpack


module ExpressionPatterns =

    let (|Zero|_|) = function
        | Number n when n.IsZero -> Some Zero
        | _ -> None

    let (|One|_|) = function
        | Number n when n.IsOne -> Some One
        | _ -> None

    let (|MinusOne|_|) = function
        | Number n when n.IsInteger && n.Numerator = BigInteger.MinusOne -> Some MinusOne
        | _ -> None

    let (|Negative|_|) = function
        | Number n when n.IsNegative -> Some Negative
        | Approximation x when Approximation.isNegative x -> Some Negative
        | NegativeInfinity -> Some Negative
        | _ -> None

    let (|Positive|_|) = function
        | Number n when n.IsPositive -> Some Positive
        | Constant E | Constant Pi -> Some Positive
        | Approximation x when Approximation.isPositive x -> Some Positive
        | PositiveInfinity -> Some Positive
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

    let (|NegPower|_|) = function
        | Power (r, (Negative _ as p))-> Some (r, p)
        | _ -> None

    /// Terminal node, either a number, identifier/symbol or constant (including infinity).
    /// Warning: Undefined is *not* included.
    let (|Terminal|_|) = function
        | Number _ | Identifier _ | Argument _ | Constant _ as t -> Some t
        | _ -> None

    /// Recognizes a sin or cos expression
    let (|SinCos|_|) = function
        | Function (Sin, _) | Function (Cos, _) as t -> Some t
        | Function (Sinh, _) | Function (Cosh, _) as t -> Some t
        | _ -> None

    let (|SinCosPosIntPower|_|) = function
        | Function (Sin, _) | Function (Cos, _) as r -> Some (r, Number BigRational.One)
        | Function (Sinh, _) | Function (Cosh, _) as r -> Some (r, Number BigRational.One)
        | Power (Function (Sin, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | Power (Function (Cos, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | Power (Function (Sinh, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | Power (Function (Cosh, _) as r, (Number n as p)) when n.IsInteger && n.IsPositive -> Some (r, p)
        | _ -> None


module Operators =

    open ExpressionPatterns

    let zero = Number BigRational.Zero
    let one = Number BigRational.One
    let two = Number (BigRational.FromInt 2)
    let private four = Number (BigRational.FromInt 4)
    let minusOne = Number (BigRational.FromInt -1)
    let pi = Constant Pi

    let symbol (name:string) = Identifier (Symbol name)

    let undefined = Expression.Undefined
    let infinity = Expression.PositiveInfinity
    let complexInfinity = Expression.ComplexInfinity
    let negativeInfinity = Expression.NegativeInfinity

    let fromReal floatingPoint = Value.fromReal floatingPoint |> Values.unpack
    let fromReal32 floatingPoint = Value.fromReal32 floatingPoint |> Values.unpack
    let fromComplex floatingPoint = Value.fromComplex floatingPoint |> Values.unpack
    let fromComplex32 floatingPoint = Value.fromComplex32 floatingPoint |> Values.unpack

    let fromDecimal (x:decimal) = Number (BigRational.FromDecimal x)

    let fromInt32 (x:int) = Number (BigRational.FromInt x)
    let fromInt64 (x:int64) = Number (BigRational.FromBigInt (BigInteger(x)))
    let fromInteger (x:BigInteger) = Number (BigRational.FromBigInt x)
    let fromIntegerFraction (n:BigInteger) (d:BigInteger) = Number (BigRational.FromBigIntFraction (n, d))
    let fromRational (x:BigRational) = Number x

    let real = fromReal
    let number = fromInt32

    let isZero = function | Zero -> true | _ -> false
    let isOne = function | One -> true | _ -> false
    let isMinusOne = function | MinusOne -> true | _ -> false
    let isPositive = function | Positive -> true | _ -> false
    let isNegative = function | Negative -> true | _ -> false
    let isPositiveInfinity = function | PositiveInfinity -> true | _ -> false
    let isNegativeInfinity = function | NegativeInfinity -> true | _ -> false
    let isComplexInfinity = function | ComplexInfinity -> true | _ -> false
    let isInfinity = function | PositiveInfinity | ComplexInfinity | NegativeInfinity -> true | _ -> false

    let isApproximateZero = function | Zero -> true | Approximation (Real r) when r = 0.0 -> true | _ -> false

    let internal orderRelation (x:Expression) (y:Expression) =
        let rec compare a b =
            match a, b with
            | Number x, Number y -> x < y
            | Approximation x, Approximation y -> Approximation.orderRelation x y
            | Identifier x, Identifier y -> x < y
            | Argument x, Argument y -> x < y
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
            | Function _, FunctionN _ -> true
            | FunctionN _, Function _ -> false
            | Identifier _, _ -> true
            | _, Identifier _ -> false
            | Argument _, _ -> true
            | _, Argument _ -> false
            | ComplexInfinity, _ -> true
            | _, ComplexInfinity -> false
            | PositiveInfinity, _ -> true
            | _, PositiveInfinity -> false
            | NegativeInfinity, _ -> true
            | _, NegativeInfinity -> false
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

        /// Recognize terms of the form a*x -> (v,x) where a is a value
        let (|Term|_|) = function
            | Number _ -> None
            | Approximation _ -> None
            | Product [(Values.Value v); b] -> Some (v, b)
            | Product ((Values.Value v)::xs) -> Some (v, Product xs)
            | x -> Some (Value.one, x)

        let merge (xs:Expression list) (ys:Expression list) =
            let rec gen acc u v =
                match acc, u, v with
                | Zero::cc, _, _ -> gen cc u v
                | Term(ac,at)::cc, Term(xc,xt)::xs, y | Term(ac,at)::cc, y, Term(xc,xt)::xs when at = xt ->
                    gen ((multiply (Value.sum(ac,xc) |> Values.unpack) at)::cc) xs y
                | _, Term(xc,xt)::xs, Term(yc,yt)::ys when xt = yt ->
                    gen ((multiply (Value.sum(xc,yc) |> Values.unpack) xt)::acc) xs ys
                | _, x::xs, y::ys ->
                    if orderRelation x y then gen (x::acc) xs v
                    else gen (y::acc) u ys
                | _, x::xs, [] | _, [], x::xs -> gen (x::acc) xs []
                | _, [], [] -> acc
            match gen [] xs ys with
            | [x] -> x
            | [] -> zero
            | x -> Sum (List.rev x)

        let rec valueAdd (v:Value) x =
            match x with
            | Values.Value a | Sum [Values.Value a] -> Values.sum (v, a)
            | Sum [] -> Values.unpack v
            | Sum [a] -> if Value.isZero v then a else Sum [Values.unpack v; a]
            | Sum ((Values.Value a)::ax) -> valueAdd (Value.sum (a,v)) (Sum ax)
            | Sum ax -> if Value.isZero v then x else Sum (Values.unpack v::ax)
            | x -> if Value.isZero v then x else Sum [Values.unpack v; x]

        match x, y with
        | Undefined, _ | _, Undefined -> undefined
        | Zero, b | b, Zero -> b
        | ComplexInfinity, oo | oo, ComplexInfinity when isInfinity oo -> undefined
        | ComplexInfinity, _ | _, ComplexInfinity -> complexInfinity
        | PositiveInfinity, PositiveInfinity -> infinity
        | PositiveInfinity, oo | oo, PositiveInfinity when isInfinity oo -> undefined
        | PositiveInfinity, _ | _, PositiveInfinity -> infinity
        | NegativeInfinity, NegativeInfinity -> negativeInfinity
        | NegativeInfinity, _ | _, NegativeInfinity -> negativeInfinity
        | Values.Value a, Values.Value b -> Values.sum (a, b)
        | Values.Value a, b | b, Values.Value a -> valueAdd a b
        | Sum ((Values.Value a)::ax), Sum ((Values.Value b)::bx) -> valueAdd (Value.sum (a, b)) (merge ax bx)
        | Sum ((Values.Value a)::ax), Sum bx | Sum bx, Sum ((Values.Value a)::ax) -> valueAdd a (merge ax bx)
        | Sum ((Values.Value a)::ax), b | b, Sum ((Values.Value a)::ax) -> valueAdd a (merge ax [b])
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
            | Approximation _ -> None
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
        let rec valueMul (v:Value) x =
            if Value.isZero v then zero else
            match x with
            | Values.Value a | Product [Values.Value a] -> Values.product (v, a)
            | Product [] -> Values.unpack v
            | Product [a] -> if Value.isOne v then a else Product [Values.unpack v; a]
            | Product ((Values.Value a)::ax) -> valueMul (Value.product (a,v)) (Product ax)
            | Product ax -> if Value.isOne v then x else Product (Values.unpack v::ax)
            | x -> if Value.isOne v then x else Product [Values.unpack v; x]

        match x, y with
        | Undefined, _ | _, Undefined -> undefined
        | One, b | b, One -> b
        | Zero, oo | oo, Zero when isInfinity oo -> undefined
        | Zero, _ | _, Zero -> zero
        | Approximation (Real a), oo | oo, Approximation (Real a) when a = 0.0 && isInfinity oo -> undefined
        | Approximation (Real a), _ | _, Approximation (Real a) when a = 0.0 -> Approximation (Real 0.0)
        | ComplexInfinity, _ | _, ComplexInfinity -> complexInfinity
        | PositiveInfinity, Positive | Positive, PositiveInfinity -> infinity
        | PositiveInfinity, Negative | Negative, PositiveInfinity -> negativeInfinity
        | PositiveInfinity, _ | _, PositiveInfinity -> infinity
        | NegativeInfinity, Positive | Positive, NegativeInfinity -> negativeInfinity
        | NegativeInfinity, Negative | Negative, NegativeInfinity -> infinity
        | NegativeInfinity, _ | _, NegativeInfinity -> negativeInfinity
        | Values.Value a, Values.Value b -> Values.product (a, b)
        | Values.Value a, b | b, Values.Value a -> valueMul a b
        | Product ((Values.Value a)::ax), Product ((Values.Value b)::bx) -> valueMul (Value.product (a, b)) (merge ax bx)
        | Product ((Values.Value a)::ax), Product bx | Product bx, Product ((Values.Value a)::ax) -> valueMul a (merge ax bx)
        | Product ((Values.Value a)::ax), b | b, Product ((Values.Value a)::ax) -> valueMul a (merge ax [b])
        | Product ax, Product bx -> merge ax bx
        | Product ax, b -> merge ax [b]
        | a, Product bx -> merge [a] bx
        | a, b -> merge [a] [b]

    and pow x y =
        // if power is a number, radix must not be an integer, fraction, product or power
        match x, y with
        | Undefined, _ | _, Undefined -> undefined
        | Zero, Zero -> undefined
        | Zero, (ComplexInfinity | PositiveInfinity) -> zero
        | Zero, NegativeInfinity -> complexInfinity
        | Zero, Positive -> zero
        | Zero, Negative -> complexInfinity
        | oo, Zero when isInfinity oo -> undefined
        | oo, PositiveInfinity when isInfinity oo -> complexInfinity
        | oo, Number b when isInfinity oo && b.IsNegative -> zero
        | ComplexInfinity, Positive -> complexInfinity
        | PositiveInfinity, Positive -> infinity
        | NegativeInfinity, Number b when b.IsPositive && b.IsInteger ->
            if (b.Numerator % 2I).IsZero then infinity else negativeInfinity
        | One, oo | MinusOne, oo when isInfinity oo -> undefined
        | _, Zero | One, _ -> one
        | a, One -> a
        | Positive, PositiveInfinity -> infinity
        | Negative, PositiveInfinity -> complexInfinity
        | _, NegativeInfinity -> zero
        | _, ComplexInfinity -> undefined
        | Number a, Number b when not (b.IsInteger) -> Power (x,y)
        | Values.Value a, Values.Value b -> Values.power (a, b)
        | Product ax, Number b when b.IsInteger -> Product (ax |> List.map (fun z -> pow z y))
        | Power (r, p), Number b when b.IsInteger -> pow r (multiply p y)
        | a, b -> Power(a, b)

    let plus x = x
    let negate x = multiply minusOne x
    let subtract x y = add x (negate y)

    let rec invert = function
        | Undefined -> undefined
        | Zero -> complexInfinity
        | oo when isInfinity oo -> zero
        | Values.Value v -> Values.invert v
        | Product ax -> Product (ax |> List.map invert)
        | Power (r, p) -> pow r (negate p)
        | x -> Power (x, minusOne)

    let divide x y = multiply x (invert y)

    let sum (xs:Expression list) = if List.isEmpty xs then zero else List.reduce add xs
    let sumSeq (xs:Expression seq) = Seq.fold add zero xs
    let product (xs:Expression list) = if List.isEmpty xs then one else List.reduce multiply xs
    let productSeq (xs:Expression seq) = Seq.fold multiply one xs

    let root n x = pow x (pow n minusOne)
    let sqrt x = root two x

    let abs = function
        | Undefined -> undefined
        | oo when isInfinity oo -> infinity
        | Constant I -> one
        | Values.Value v -> Values.abs v
        | Product ((Values.Value v)::ax) when Value.isNegative v -> Function (Abs, multiply (Values.abs v) (Product ax))
        | x -> Function (Abs, x)

    let exp = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> infinity
        | NegativeInfinity -> zero
        | Zero -> one
        | One -> Constant E
        | MinusOne -> invert (Constant E)
        | Product [Constant Pi; Constant I;] -> minusOne // exp(n*pi*j) for ...-1, -1/2, 0, 1/2, 1,...
        | Product [Number n; Constant Pi; Constant I;] when n.IsInteger
            -> if n.Numerator.IsEven then one else minusOne
        | Product [Number n; Constant Pi; Constant I;] when (n*2N).IsInteger
            -> if (n + 1N/2N).Numerator.IsEven then negate (Constant I) else Constant I
        | Function (Ln, x') -> x' // exp(ln(x)) = x
        | x -> Function (Exp, x)
    let rec ln = function
        | Undefined -> undefined
        | oo when isInfinity oo -> infinity
        | Zero -> negativeInfinity
        | One -> zero
        | MinusOne -> multiply pi (Constant I) // ln(-1) = pi*j
        | Constant E -> one
        | Constant I -> divide (multiply pi (Constant I)) two // ln(j) = 1/2*pi*j
        | Number n when n.Numerator.Equals(1I) && n.IsPositive
            -> Function (Ln, fromInteger n.Denominator) |> negate // ln(1/x) = -ln(x) for positive x
        | Power (x', Number n) when n.Equals(-1N) && isPositive x'
            -> ln x' |> negate
        | x -> Function (Ln, x)
    let lg = function
        | Undefined -> undefined
        | Zero -> negativeInfinity
        | One -> zero
        | Number n when n.Equals(10N) -> one
        | oo when isInfinity oo -> infinity
        | x -> Function (Lg, x)
    let log basis x = FunctionN (Log, [basis; x])

    let sin = function
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined
        | Zero -> zero
        | Constant Pi -> zero // sin(n*pi) = 0 for integer n
        | Constant I -> multiply (Constant I) (Function (Sinh, one))  // sin(j) = j*sinh(1), sin(j*x) = j*sinh(x)
        | Number n when n.IsNegative -> negate (Function (Sin, Number -n))
        | Product ((Number n)::ax) when n.IsNegative -> negate (Function (Sin, multiply (Number -n) (Product ax)))
        | Function (Asin, x') -> x' // sin(asin(x)) = x
        | Function (Acos, x') -> sqrt (subtract one (pow x' two)) // sin(acos(x)) = sqrt(1 - x^2)
        | Function (Atan, x') -> divide x' (sqrt (add one (pow x' two))) // sin(atan(x)) = x/sqrt(x^2 + 1)
        | Function (Acsc, x') -> invert x' // sin(acsc(x)) = 1/x
        | Function (Asec, x') -> sqrt (subtract one (invert (pow x' two))) // sin(asec(x)) = sqrt(1 - 1/x^2)
        | Function (Acot, x') -> invert (multiply x' (sqrt (add one (invert (pow x' two))))) // sin(acot(x)) = 1/(x*sqrt(1 + 1/x^2))
        | x -> Function (Sin, x)
    let cos = function
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined
        | Zero -> one
        | Constant Pi -> minusOne // cos(pi) = -1
        | Constant I -> Function (Cosh, one) // cos(j) = cosh(1), cos(j*x) = cosh(x)
        | Number n when n.IsNegative -> Function (Cos, Number -n)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Cos, multiply (Number -n) (Product ax))
        | Function (Asin, x') -> sqrt (subtract one (pow x' two)) // cos(asin(x)) = sqrt(1 - x^2)
        | Function (Acos, x') -> x' // cos(acos(x)) = x
        | Function (Atan, x') -> invert (sqrt (add one (pow x' two))) // cos(atan(x)) = 1/sqrt(1 + x^2)
        | Function (Acsc, x') -> sqrt (subtract one (invert (pow x' two))) // cos(acsc(x)) = sqrt(1 - 1/x^2)
        | Function (Asec, x') -> invert x' // cos(asec(x)) = 1/x
        | Function (Acot, x') -> invert (sqrt (add one (invert (pow x' two)))) // cos(acot(x)) = 1/sqrt(1/x^2 + 1)
        | x -> Function (Cos, x)
    let tan = function
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined
        | Zero -> zero
        | Constant Pi -> zero // tan(pi) = 0
        | Constant I -> multiply (Constant I) (Function (Tanh, one)) // tan(j) = j*tanh(1), tan(j*x) = j*tanh(x)
        | Number n when n.IsNegative -> negate (Function (Tan, Number -n))
        | Product ((Number n)::ax) when n.IsNegative -> negate (Function (Tan, multiply (Number -n) (Product ax)))
        | Function (Asin, x') -> divide x' (sqrt (subtract one (pow x' two))) // tan(asin(x)) = x/sqrt(1 - x^2)
        | Function (Acos, x') -> divide (sqrt (subtract one (pow x' two))) x' // tan(acos(x)) = sqrt(1 - x^2)/x
        | Function (Atan, x') -> x' // tan(atan(x)) = x
        | Function (Acsc, x') -> invert (multiply x' (sqrt (subtract one (invert (pow x' two))))) // tan(acsc(x)) = 1/(sqrt(1 - 1/x^2)*x)
        | Function (Asec, x') -> multiply x' (sqrt (subtract one (invert (pow x' two)))) // tan(asec(x)) = x*sqrt(1 - 1/x^2)
        | Function (Acot, x') -> invert x' // tan(acot(x)) = 1/x
        | x -> Function (Tan, x)
    let csc = function
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined
        | Zero -> complexInfinity // csc(0) = coo
        | Constant Pi -> complexInfinity // csc(pi) = coo
        | Constant I -> Function (Csch, one) |> multiply (Constant I) |> negate // csc(j) = -j*csch(1), csc(j*x) = -j*csch(x)
        | Number n when n.IsNegative -> Function (Csc, Number -n) |> negate // csc(-x) = -csc(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Csc, multiply (Number -n) (Product ax)) |> negate
        | Function (Asin, x') -> invert x' // csc(asin(x)) = 1/x
        | Function (Acos, x') -> invert (sqrt (subtract one (pow x' two))) // csc(acos(x)) = 1/sqrt(1 - x^2)
        | Function (Atan, x') -> divide (sqrt (add one (pow x' two))) x' // csc(atan(x)) = sqrt(1 + x^2)/x
        | Function (Acsc, x') -> x' // csc(acsc(x)) = x
        | Function (Asec, x') -> invert (sqrt (subtract one (invert (pow x' two)))) // csc(asec(x)) = 1/sqrt(1 - 1/x^2)
        | Function (Acot, x') -> multiply x' (sqrt (add one (invert (pow x' two)))) // csc(acot(x)) = (x*sqrt(1 + 1/x^2))
        | x -> Function (Csc, x)
    let sec = function
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined
        | Zero -> one // sec(0) = 1
        | Constant Pi -> minusOne // sec(pi) = -1
        | Constant I -> Function (Sech, one) // sec(j) = sech(1), sec(j*x) = sech(x)
        | Number n when n.IsNegative -> Function (Sec, Number -n) // sec(-x) = sec(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Sec, multiply (Number -n) (Product ax))
        | Function (Asin, x') -> invert (sqrt (subtract one (pow x' two))) // sec(asin(x)) = 1/sqrt(1 - x^2)
        | Function (Acos, x') -> invert x' // sec(acos(x)) = 1/x
        | Function (Atan, x') -> sqrt (add one (pow x' two)) // sec(atan(x)) = sqrt(1 + x^2)
        | Function (Acsc, x') -> invert (sqrt (subtract one (invert (pow x' two)))) // sec(acsc(x)) = 1/sqrt(1 - 1/x^2)
        | Function (Asec, x') -> x' // sec(asec(x)) = x
        | Function (Acot, x') -> sqrt (add one (invert (pow x' two))) // sec(acot(x)) = sqrt(1 + 1/x^2)
        | x -> Function (Sec, x)
    let cot = function
        | Undefined -> undefined
        | oo when isInfinity oo -> undefined
        | Zero -> complexInfinity // cot(0) = coo
        | Constant Pi -> complexInfinity // cot(pi) = coo
        | Constant I -> Function (Coth, one) |> multiply (Constant I) |> negate // cot(j) = -j*coth(1), cot(j*x) = -j*coth(x)
        | Number n when n.IsNegative -> Function (Cot, Number -n) |> negate // cot(-x) = -cot(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Cot, multiply (Number -n) (Product ax)) |> negate
        | Function (Asin, x') -> divide (sqrt (subtract one (pow x' two))) x' // cot(asin(x)) = sqrt(1 - x^2)/x
        | Function (Acos, x') -> divide x' (sqrt (subtract one (pow x' two))) // cot(acos(x)) = x/sqrt(1 - x^2)
        | Function (Atan, x') -> invert x' // cot(atan(x)) = 1/x
        | Function (Acsc, x') -> multiply x' (sqrt (subtract one (invert (pow x' two)))) // cot(acsc(x)) = x*sqrt(1 - 1/x^2)
        | Function (Asec, x') -> invert (multiply x' (sqrt (subtract one (invert (pow x' two))))) // cot(asec(x)) = 1/(x*sqrt(1 - 1/x^2))
        | Function (Acot, x') -> x' // cot(acot(x)) = x
        | x -> Function (Cot, x)

    let sinh = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> infinity // sinh(oo) = oo
        | NegativeInfinity -> negativeInfinity // sinh(-oo) = -oo
        | Zero -> zero // sinh(0) = 0
        | Constant I -> Function (Sin, one) |> multiply (Constant I) // sinh(j) = j*sin(1), sinh(j*x) = j*sin(x)
        | Number n when n.IsNegative -> Function (Sinh, Number -n) |> negate // sinh(-x) = -sinh(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Sinh, multiply (Number -n) (Product ax)) |> negate
        | Function (Asinh, x') -> x' // sinh(asinh(x)) = x
        | Function (Acosh, x') -> multiply (add one x') (sqrt (divide (subtract x' one) (add x' one))) // sinh(acosh(x)) = (x + 1)*sqrt((x - 1)/(x + 1))
        | Function (Atanh, x') -> divide x' (sqrt (subtract one (pow x' two))) // sinh(atanh(x)) = x/sqrt(1 - x^2)
        | Function (Acsch, x') -> invert x' // sinh(acsch(x)) = 1/x
        | Function (Asech, x') -> divide (multiply (add x' one) (sqrt (divide (subtract one x') (add x' one)))) x' // sinh(asech(x)) = ((x + 1)*sqrt((1 - x)/(x + 1)))/x
        | Function (Acoth, x') -> invert (multiply x' (sqrt (subtract one (invert (pow x' two))))) // sinh(acoth(x)) = 1/(x*sqrt(1 - 1/x^2))
        | x -> Function (Sinh, x)
    let cosh = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> infinity // cosh(oo) = cosh(-oo) = oo
        | Zero -> one // cosh(0) = 1
        | Constant I -> Function (Cos, one) // cosh(j) = cos(1), cosh(j*x) = cos(x)
        | Number n when n.IsNegative -> Function (Cosh, Number -n) // cosh(-x) = cosh(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Cosh, multiply (Number -n) (Product ax))
        | Function (Asinh, x') -> sqrt (add (pow x' two) one) // cosh(asinh(x)) = sqrt(x^2 + 1)
        | Function (Acosh, x') -> x' // cosh(acosh(x)) = x
        | Function (Atanh, x') -> invert (sqrt (subtract one (pow x' two))) // cosh(atanh(x)) = 1/sqrt(1 - x^2)
        | Function (Acsch, x') -> sqrt (add (invert (pow x' two)) one) // cosh(acsch(x)) = sqrt(1/x^2 + 1)
        | Function (Asech, x') -> invert x' // cosh(asech(x)) = 1/x
        | Function (Acoth, x') -> invert (sqrt (subtract one (invert (pow x' two)))) // cosh(acoth(x)) = 1/sqrt(1 - 1/x^2)
        | x -> Function (Cosh, x)
    let tanh = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> one // tanh(oo) = 1, tanh(-oo) = -1
        | NegativeInfinity -> minusOne
        | Zero -> zero // tanh(0) = 0
        | Constant I -> Function (Tan, one) |> multiply (Constant I) // tanh(j) = j*tan(1), tanh(j*x) = j*tan(x)
        | Number n when n.IsNegative -> Function (Tanh, Number -n) |> negate // tanh(-x) = -tanh(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Tanh, multiply (Number -n) (Product ax)) |> negate
        | Function (Asinh, x') -> divide x' (sqrt (add (pow x' two) one)) // tanh(asinh(x)) = x/sqrt(x^2 + 1)
        | Function (Acosh, x') -> divide (multiply (add x' one) (sqrt (divide (subtract x' one) (add x' one)))) x' // tanh(acosh(x)) = ((x + 1)*sqrt((x - 1)/(x + 1)))/x
        | Function (Atanh, x') -> x' // tanh(atanh(x)) = x
        | Function (Acsch, x') -> invert (multiply x' (sqrt (add (invert (pow x' two)) one))) // tanh(acsch(x)) = 1/(x*sqrt(1/x^2 + 1))
        | Function (Asech, x') -> multiply (add x' one) (sqrt(divide (subtract one x') (add x' one))) // tanh(asech(x)) = (x + 1)*sqrt((1 - x)/(x + 1))
        | Function (Acoth, x') -> invert x' // tanh(acoth(x)) = 1/x
        | x -> Function (Tanh, x)
    let csch = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> zero // csch(oo) = csch(-oo) = oo
        | Zero -> complexInfinity // csch(0) = coo
        | Constant I -> Function (Csc, one) |> multiply (Constant I) |> negate // csch(j) = -j*csc(1), csch(j*x) = -j*csc(x)
        | Number n when n.IsNegative -> Function (Csch, Number -n) |> negate // csch(-x) = -csch(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Csch, multiply (Number -n) (Product ax)) |> negate
        | Function (Asinh, x') -> invert x' // csch(asinh(x)) = 1/x
        | Function (Acosh, x') -> invert (multiply (add one x') (sqrt (divide (subtract x' one) (add x' one)))) // csch(acosh(x)) = 1/((x + 1)*sqrt((x - 1)/(x + 1)))
        | Function (Atanh, x') -> divide (sqrt (subtract one (pow x' two))) x' // csch(atanh(x)) = sqrt(1 - x^2)/x
        | Function (Acsch, x') -> x' // csch(acsch(x)) = x
        | Function (Asech, x') -> divide x' (multiply (add x' one) (sqrt (divide (subtract one x') (add x' one)))) // csch(asech(x)) = x/((x + 1)*sqrt((1 - x)/(x + 1)))
        | Function (Acoth, x') -> multiply x' (sqrt (subtract one (invert (pow x' two)))) // csch(acoth(x)) = x*sqrt(1 - 1/x^2)
        | x -> Function (Csch, x)
    let sech = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> zero // sech(oo) = sech(-oo) = 0
        | Zero -> one // sech(0) = 1
        | Constant I -> Function (Sec, one) // sech(j*x) = sec(x)
        | Number n when n.IsNegative -> Function (Sech, Number -n) // sech(-x) = sech(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Sech, multiply (Number -n) (Product ax))
        | Function (Asinh, x') -> invert (sqrt (add (pow x' two) one)) // sech(asinh(x)) = 1/sqrt(x^2 + 1)
        | Function (Acosh, x') -> invert x' // sech(acosh(x)) = 1/x
        | Function (Atanh, x') -> sqrt (subtract one (pow x' two)) // sech(atanh(x)) = sqrt(1 - x^2)
        | Function (Acsch, x') -> invert (sqrt (add (invert (pow x' two)) one)) // sech(acsch(x)) = 1/sqrt(1/x^2 + 1)
        | Function (Asech, x') -> x' // sech(asech(x)) = x
        | Function (Acoth, x') -> sqrt (subtract one (invert (pow x' two))) // sech(acoth(x)) = sqrt(1 - 1/x^2)
        | x -> Function (Sech, x)
    let coth = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> one
        | Zero -> complexInfinity
        | Constant I -> Function (Cot, one) |> multiply (Constant I) |> negate // coth(j*x) = -j*cot(x)
        | Number n when n.IsNegative -> Function (Coth, Number -n) |> negate // coth(-x) = -coth(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Coth, multiply (Number -n) (Product ax)) |> negate
        | Function (Asinh, x') -> divide (sqrt (add (pow x' two) one)) x' // coth(asinh(x)) = sqrt(x^2 + 1)/x
        | Function (Acosh, x') -> divide x' (multiply (add x' one) (sqrt (divide (subtract x' one) (add x' one)))) // coth(acosh(x)) = x/((x + 1)*sqrt((x - 1)/(x + 1)))
        | Function (Atanh, x') -> invert x' // coth(atanh(x)) = 1/x
        | Function (Acsch, x') -> multiply x' (sqrt (add (invert (pow x' two)) one)) // coth(acsch(x)) = (x*sqrt(1/x^2 + 1))
        | Function (Asech, x') -> invert (multiply (add x' one) (sqrt(divide (subtract one x') (add x' one)))) // coth(asech(x)) = 1/((x + 1)*sqrt((1 - x)/(x + 1)))
        | Function (Acoth, x') -> x' // coth(acoth(x)) = x
        | x -> Function (Coth, x)

    let arcsin = function
        | Undefined | ComplexInfinity -> undefined
        | Zero -> zero // asin(0) = 0
        | One -> divide pi two // asin(1) = pi/2
        | MinusOne -> divide pi two |> negate // asin(-1) = -pi/2
        | Constant I -> multiply (Constant I) (Function (Asinh, one)) // asin(j) = j*asinh(1)
        | Number n when n.IsNegative -> Function (Asin, Number -n) |> negate // arcsin(-x) = -arcsin(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Asin, multiply (Number -n) (Product ax)) |> negate
        | x -> Function (Asin, x)
    let arccos = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> multiply infinity (Constant I) // acos(oo) = oo*j, acos(-oo) = -oo*j
        | NegativeInfinity -> multiply negativeInfinity (Constant I)
        | Zero -> divide pi two // acos(0) = pi/2
        | One -> zero // acos(1) = 0
        | MinusOne -> pi // acos(-1) = pi
        | x -> Function (Acos, x)
    let arctan = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> divide pi two // atan(oo) = pi/2, atan(-oo) = -pi/2
        | Zero -> zero // atan(0) = 0
        | One -> divide pi four // atan(1) = pi/4
        | MinusOne -> divide pi four |> negate // atan(-1) = -pi/4
        | Constant I -> multiply (Constant I) infinity // atan(j) = oo*j
        | Number n when n.IsNegative -> Function (Atan, Number -n) |> negate // atan(-x) = -atan(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Atan, multiply (Number -n) (Product ax)) |> negate
        | x -> Function (Atan, x)
    let arctan2 x y = FunctionN (Atan2, [x;y])
    let arccsc = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> zero // acsc(oo) = acsc(-oo) = 0
        | Zero -> complexInfinity // acsc(0) = coo
        | One -> divide pi two // acsc(1) = pi/2, acsc(-1) = -pi/2
        | MinusOne -> divide pi two |> negate
        | Constant I -> multiply (Constant I) (Function (Acsch, one)) |> negate // acsc(j) = -j*acsch(1)
        | x -> Function (Acsc, x)
    let arcsec = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> divide pi two // asec(oo) = asec(-oo) = pi/2
        | Zero -> complexInfinity // asec(0) = coo
        | One -> zero // asec(1) = 0, asec(-1) = pi
        | MinusOne -> pi
        | x -> Function (Asec, x)
    let arccot = function
        | Undefined -> undefined
        | oo when isInfinity oo -> zero // acot(coo) = acot(oo) = acot(-oo) = 0
        | Zero -> divide pi two // acot(0) = pi/2
        | One -> divide pi four // acot(1) = pi/4, acot(-1) = -pi/4
        | MinusOne -> divide pi four |> negate
        | Constant I -> multiply (Constant I) negativeInfinity // atan(j) = -oo*j
        | Number n when n.IsNegative -> Function (Acot, Number -n) |> negate // acot(-x) = -acot(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Acot, multiply (Number -n) (Product ax)) |> negate
        | x -> Function (Acot, x)

    let arcsinh = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> infinity // asinh(oo) = oo, asinh(-oo) = -oo
        | NegativeInfinity -> negativeInfinity
        | Zero -> zero // asinh(0) = 0
        | Constant I -> divide (multiply pi (Constant I)) two // asinh(j) = pi*j/2, asinh(n*j) = j*asin(n)
        | Number n when n.IsNegative -> Function (Asinh, Number -n) |> negate // asinh(-x) = -asinh(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Asinh, multiply (Number -n) (Product ax)) |> negate
        | x -> Function (Asinh, x)
    let arccosh = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> infinity // acosh(oo) = acosh(-oo) = oo
        | Zero -> divide (multiply pi (Constant I)) two // acosh(0) = pi*j/2
        | One -> zero // acosh(1) = 0
        | MinusOne -> multiply pi (Constant I) // acosh(-1) = pi*j
        | x -> Function (Acosh, x)
    let arctanh = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> divide  (multiply pi (Constant I)) two |> negate // atanh(oo) = - pi*j/2, atanh(-oo) = pi*j/2
        | NegativeInfinity -> divide  (multiply pi (Constant I)) two
        | Zero -> zero // atanh(0) = 0
        | One -> infinity // atanh(1) = oo, atanh(-1) = -oo
        | MinusOne -> negativeInfinity
        | Constant I -> divide (multiply pi (Constant I)) four // atanh(j) = pi*j/4
        | Number n when n.IsNegative -> Function (Atanh, Number -n) |> negate // atanh(-x) = -atanh(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Atanh, multiply (Number -n) (Product ax)) |> negate
        | x -> Function (Atanh, x)
    let arccsch = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> zero // acsch(oo) = acsch(-oo) = 0
        | Zero | One | MinusOne -> complexInfinity // acsch(0) = coo
        | Constant I -> divide (multiply pi (Constant I)) two |> negate // acsch(j) = -pi*j/2
        | Number n when n.IsNegative -> Function (Acsch, Number -n) |> negate // acsch(-x) = -acsch(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Acsch, multiply (Number -n) (Product ax)) |> negate
        | x -> Function (Acsch, x)
    let arcsech = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> divide (multiply pi (Constant I)) two // asech(oo) = asech(-oo) = pi*j/2
        | Zero -> infinity // asech(0) = oo
        | One -> zero // asech(1) = 0
        | MinusOne -> multiply pi (Constant I) // asech(-1) = pi*j
        | x -> Function (Asech, x)
    let arccoth = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> zero // acoth(oo) = acoth(-oo) = 0
        | Zero -> divide (multiply pi (Constant I)) two // acoth(0) = pi*j/2
        | One -> infinity // acoth(1) = oo, acoth(-1) = -oo
        | MinusOne -> negativeInfinity
        | Constant I -> divide (multiply pi (Constant I)) four |> negate // atanh(j) = -pi*j/4
        | Number n when n.IsNegative -> Function (Acoth, Number -n) |> negate // acoth(-x) = -acoth(x)
        | Product ((Number n)::ax) when n.IsNegative -> Function (Acoth, multiply (Number -n) (Product ax)) |> negate
        | x -> Function (Acoth, x)

    let airyai = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity | NegativeInfinity -> zero // Ai(oo) = Ai(-oo) = 0
        //| Zero -> divide (pow three (invert three)) (multiply three (gamma (divide two three)))) // Ai(0) = 3^(1/3)/(3*Gamma(2/3))
        | x -> Function (AiryAi, x)
    let airyaiprime = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> zero // Ai'(oo) = 0
        //| Zero -> pow three (invert three)) |> multiply (gamma (invert three)) |> invert |> negate // Ai'(0) = -1/(3^(1/3)*Gamma(1/3))
        | x -> Function (AiryAiPrime, x)
    let airybi = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> infinity // Bi(oo) = oo
        | NegativeInfinity -> zero // Bi(-oo) = 0
        //| Zero -> pow three (invert six)) |> multiply (gamma (divide two three)) |> invert // Bi(0) = 1/(3^(1/6)*Gamma(2/3))
        | x -> Function (AiryBi, x)
    let airybiprime = function
        | Undefined | ComplexInfinity -> undefined
        | PositiveInfinity -> infinity // Bi'(oo) = oo
        | NegativeInfinity -> zero // Bi'(-oo) = 0
        //| Zero -> divide (pow three (invert six)) (gamma (invert three)) // Bi'(0) = 3^(1/6)/Gamma(1/3)
        | x -> Function (AiryBiPrime, x)

    let rec besselj nu x =
        match nu, x with
        | Undefined, _ -> undefined
        | _, Undefined -> undefined
        | Zero, Zero -> one // J(0, 0) = 1
        | Positive, Zero -> zero // J(n, 0) = 0 for n > 0
        | Number n, _  when n.IsNegative -> (pow minusOne (Number -n)) |> multiply (besselj (Number -n) x) // J(-n, x) = pow(-1, n) * J(n, x)
        | Product ((Number n)::ax), _ when n.IsNegative -> (pow minusOne (multiply (Number -n) (Product ax))) |> multiply (besselj (multiply (Number -n) (Product ax)) x)
        | _, PositiveInfinity -> zero // J(nu, oo) = 0
        | _, NegativeInfinity -> zero // J(nu, -oo) = 0
        | _, _ -> FunctionN (BesselJ, [nu; x])
    let rec bessely nu x =
        match nu, x with
        | Undefined, _ -> undefined
        | _, Undefined -> undefined
        | Zero, Zero -> negativeInfinity // Y(0, 0) = -oo
        | Positive, Zero -> complexInfinity // Y(n, 0) = ⧝ for n > 0
        | Number n, _  when n.IsNegative -> (pow minusOne (Number -n)) |> multiply (bessely (Number -n) x) // Y(-n, x) = pow(-1, n) * Y(n, x)
        | Product ((Number n)::ax), _ when n.IsNegative -> (pow minusOne (multiply (Number -n) (Product ax))) |> multiply (bessely (multiply (Number -n) (Product ax)) x)
        | _, PositiveInfinity -> zero // Y(nu, oo) = 0
        | _, NegativeInfinity -> zero // Y(nu, -oo) = 0
        | _, _ -> FunctionN (BesselY, [nu; x])
    let rec besseli nu x =
        match nu, x with
        | Undefined, _ -> undefined
        | _, Undefined -> undefined
        | Zero, Zero -> one // I(0, 0) = 1
        | Positive, Zero -> zero // I(n, 0) = 0 for n > 0
        | Number n, _  when n.IsNegative -> besseli (Number -n) x // I(-n, x) = I(n, x)
        | Product ((Number n)::ax), _ when n.IsNegative -> besseli (multiply (Number -n) (Product ax)) x
        | _, _ -> FunctionN (BesselI, [nu; x])
    let rec besselk nu x =
        match nu, x with
        | Undefined, _ -> undefined
        | _, Undefined -> undefined
        | Zero, Zero -> infinity // K(0, 0) = oo
        | Positive, Zero -> complexInfinity // K(n, 0) = ⧝ for n > 0
        | Number n, _  when n.IsNegative -> besselk (Number -n) x // K(-n, x) = K(n, x)
        | Product ((Number n)::ax), _ when n.IsNegative -> besselk (multiply (Number -n) (Product ax)) x
        | _, _ -> FunctionN (BesselK, [nu; x])
    let rec besseliratio nu x =
        match nu, x with
        | Undefined, _ -> undefined
        | _, Undefined -> undefined
        | Zero, Zero -> zero // I(1, 0) / I(0, 0) = 0
        | Number n, _ when n.Numerator = -1I && n.Denominator = 2I -> tanh x // I(1/2, x) / I(-1/2, x) = tanh(x)
        | Number n, _ when n.Numerator = 1I && n.Denominator = 2I -> subtract (coth x) (invert x) // I(3/2, x) / I(1/2, x) = coth(x) - 1/x
        | _, _ -> FunctionN (BesselIRatio, [nu; x])
    let rec besselkratio nu x =
        match nu, x with
        | Undefined, _ -> undefined
        | _, Undefined -> undefined
        | Zero, Zero -> undefined // K(1, 0) / K(0, 0) = NaN
        | Number n, _ when n.Numerator = -1I && n.Denominator = 2I -> one // K(1/2, x) / K(-1/2, x) = 1
        | Number n, _ when n.Numerator = 1I && n.Denominator = 2I -> add (invert x) one  // K(3/2, x) / K(1/2, x) = 1/x + 1
        | _, Zero -> undefined
        | _, _ -> FunctionN (BesselKRatio, [nu; x])
    let rec hankelh1 nu x =
        match nu, x with
        | Undefined, _ -> undefined
        | _, Undefined -> undefined
        | _, Zero -> complexInfinity // H1(n, 0) = ⧝
        | Number n, _  when n.IsNegative -> (pow minusOne (Number -n)) |> multiply (hankelh1 (Number -n) x) // H1(-n, x) = pow(-1, n) * H1(n, x)
        | Product ((Number n)::ax), _ when n.IsNegative -> (pow minusOne (multiply (Number -n) (Product ax))) |> multiply (hankelh1 (multiply (Number -n) (Product ax)) x)
        | _, _ -> FunctionN (HankelH1, [nu; x])
    let rec hankelh2 nu x =
        match nu, x with
        | Undefined, _ -> undefined
        | _, Undefined -> undefined
        | _, Zero -> complexInfinity // H2(n, 0) = ⧝
        | Number n, _  when n.IsNegative -> (pow minusOne (Number -n)) |> multiply (hankelh2 (Number -n) x) // H2(-n, x) = pow(-1, n) * H2(n, x)
        | Product ((Number n)::ax), _ when n.IsNegative -> (pow minusOne (multiply (Number -n) (Product ax))) |> multiply (hankelh2 (multiply (Number -n) (Product ax)) x)
        | _, _ -> FunctionN (HankelH2, [nu; x])

    let apply (f: Function) x =
        match f with
        | Abs -> abs x
        | Exp -> exp x
        | Ln -> ln x
        | Lg -> lg x
        | Sin -> sin x
        | Cos -> cos x
        | Tan -> tan x
        | Csc -> csc x
        | Cot -> cot x
        | Sec -> sec x
        | Sinh -> sinh x
        | Cosh -> cosh x
        | Tanh -> tanh x
        | Csch -> csch x
        | Sech -> sech x
        | Coth -> coth x
        | Asin -> arcsin x
        | Acos -> arccos x
        | Atan -> arctan x
        | Acsc -> arccsc x
        | Asec -> arcsec x
        | Acot -> arccot x
        | Asinh -> arcsinh x
        | Acosh -> arccosh x
        | Atanh -> arctanh x
        | Acsch -> arccsch x
        | Asech -> arcsech x
        | Acoth -> arccoth x
        | AiryAi -> airyai x
        | AiryAiPrime -> airyaiprime x
        | AiryBi -> airybi x
        | AiryBiPrime -> airybiprime x

    let applyN (f: FunctionN) (xs: Expression list) =
        match f, xs with
        | Atan2, [x;y] -> arctan2 x y
        | Log, [b; x] -> log b x
        | BesselJ, [nu; x] -> besselj nu x
        | BesselY, [nu; x] -> bessely nu x
        | BesselI, [nu; x] -> besseli nu x
        | BesselK, [nu; x] -> besselk nu x
        | BesselIRatio, [nu; x] -> besseliratio nu x
        | BesselKRatio, [nu; x] -> besselkratio nu x
        | HankelH1, [nu; x] -> hankelh1 nu x
        | HankelH2, [nu; x] -> hankelh2 nu x
        | _ -> failwith "not supported"


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

    static member FromDecimal (x:decimal) = Operators.fromDecimal x

    static member FromReal (floatingPoint:float) = Operators.fromReal floatingPoint
    static member FromReal32 (floatingPoint:float32) = Operators.fromReal32 floatingPoint
    static member FromComplex (floatingPoint:complex) = Operators.fromComplex floatingPoint
    static member FromComplex32 (floatingPoint:complex32) = Operators.fromComplex32 floatingPoint

    static member Symbol (name:string) = Operators.symbol name

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
    static member Log(x) = Operators.lg x
    static member Log (basis, x) = Operators.log basis x

    static member Sin (x) = Operators.sin x
    static member Cos (x) = Operators.cos x
    static member Tan (x) = Operators.tan x
    static member Csc (x) = Operators.csc x
    static member Sec (x) = Operators.sec x
    static member Cot (x) = Operators.cot x

    static member Sinh (x) = Operators.sinh x
    static member Cosh (x) = Operators.cosh x
    static member Tanh (x) = Operators.tanh x
    static member Coth (x) = Operators.coth x
    static member Csch (x) = Operators.csch x
    static member Sech (x) = Operators.sech x

    static member ArcSin (x) = Operators.arcsin x
    static member ArcCos (x) = Operators.arccos x
    static member ArcTan (x) = Operators.arctan x
    static member ArcCsc (x) = Operators.arccsc x
    static member ArcSec (x) = Operators.arcsec x
    static member ArcCot (x) = Operators.arccot x

    static member ArcSinh (x) = Operators.arcsinh x
    static member ArcCosh (x) = Operators.arccosh x
    static member ArcTanh (x) = Operators.arctanh x
    static member ArcCsch (x) = Operators.arccsch x
    static member ArcSech (x) = Operators.arcsech x
    static member ArcCoth (x) = Operators.arccoth x

    static member AiryAi (x) = Operators.airyai x
    static member AiryAiPrime (x) = Operators.airyaiprime x
    static member AiryBi (x) = Operators.airybi x
    static member AiryBiPrime (x) = Operators.airybiprime x

    static member BesselJ (n, x) = Operators.besselj n x // Bessel function of the first kind
    static member BesselY (n, x) = Operators.bessely n x // Bessel function of the second kind
    static member BesselI (n, x) = Operators.besseli n x // Modified Bessel function of the first kind
    static member BesselK (n, x) = Operators.besselk n x // Modified Bessel function of the second kind
    static member BesselIRatio (n, x) = Operators.besseliratio n x // Ratio of modified Bessel function of the first kind
    static member BesselKRatio (n, x) = Operators.besselkratio n x // Ratio of modified Bessel function of the second kind

    static member HankelH1 (n, x) = Operators.hankelh1 n x // Hankel Function of the First Kind
    static member HankelH2 (n, x) = Operators.hankelh2 n x // Hankel Function of the Second Kind

    static member Apply (f, x) = Operators.apply f x
    static member ApplyN (f, xs) = Operators.applyN f xs

    // Simpler usage - numbers
    static member ( + ) (x, (y:int)) = x + (Operators.fromInt32 y)
    static member ( + ) ((x:int), y) = (Operators.fromInt32 x) + y
    static member ( - ) (x, (y:int)) = x - (Operators.fromInt32 y)
    static member ( - ) ((x:int), y) = (Operators.fromInt32 x) - y
    static member ( * ) (x, (y:int)) = x * (Operators.fromInt32 y)
    static member ( * ) ((x:int), y) = (Operators.fromInt32 x) * y
    static member ( / ) (x, (y:int)) = x / (Operators.fromInt32 y)
    static member ( / ) ((x:int), y) = (Operators.fromInt32 x) / y
    static member Pow (x, (y:int)) = Operators.pow x (Operators.fromInt32 y)

    // Simpler usage - approximations
    static member ( + ) (x, (y:float)) = x + (Operators.fromReal y)
    static member ( + ) ((x:float), y) = (Operators.fromReal x) + y
    static member ( - ) (x, (y:float)) = x - (Operators.fromReal y)
    static member ( - ) ((x:float), y) = (Operators.fromReal x) - y
    static member ( * ) (x, (y:float)) = x * (Operators.fromReal y)
    static member ( * ) ((x:float), y) = (Operators.fromReal x) * y
    static member ( / ) (x, (y:float)) = x / (Operators.fromReal y)
    static member ( / ) ((x:float), y) = (Operators.fromReal x) / y

    static member ( + ) (x, (y:float32)) = x + (Operators.fromReal32 y)
    static member ( + ) ((x:float32), y) = (Operators.fromReal32 x) + y
    static member ( - ) (x, (y:float32)) = x - (Operators.fromReal32 y)
    static member ( - ) ((x:float32), y) = (Operators.fromReal32 x) - y
    static member ( * ) (x, (y:float32)) = x * (Operators.fromReal32 y)
    static member ( * ) ((x:float32), y) = (Operators.fromReal32 x) * y
    static member ( / ) (x, (y:float32)) = x / (Operators.fromReal32 y)
    static member ( / ) ((x:float32), y) = (Operators.fromReal32 x) / y

    static member ( + ) (x, (y:complex)) = x + (Operators.fromComplex y)
    static member ( + ) ((x:complex), y) = (Operators.fromComplex x) + y
    static member ( - ) (x, (y:complex)) = x - (Operators.fromComplex y)
    static member ( - ) ((x:complex), y) = (Operators.fromComplex x) - y
    static member ( * ) (x, (y:complex)) = x * (Operators.fromComplex y)
    static member ( * ) ((x:complex), y) = (Operators.fromComplex x) * y
    static member ( / ) (x, (y:complex)) = x / (Operators.fromComplex y)
    static member ( / ) ((x:complex), y) = (Operators.fromComplex x) / y

    static member ( + ) (x, (y:complex32)) = x + (Operators.fromComplex32 y)
    static member ( + ) ((x:complex32), y) = (Operators.fromComplex32 x) + y
    static member ( - ) (x, (y:complex32)) = x - (Operators.fromComplex32 y)
    static member ( - ) ((x:complex32), y) = (Operators.fromComplex32 x) - y
    static member ( * ) (x, (y:complex32)) = x * (Operators.fromComplex32 y)
    static member ( * ) ((x:complex32), y) = (Operators.fromComplex32 x) * y
    static member ( / ) (x, (y:complex32)) = x / (Operators.fromComplex32 y)
    static member ( / ) ((x:complex32), y) = (Operators.fromComplex32 x) / y

    static member ( + ) (x, (y:decimal)) = x + (Operators.fromDecimal y)
    static member ( + ) ((x:decimal), y) = (Operators.fromDecimal x) + y
    static member ( - ) (x, (y:decimal)) = x - (Operators.fromDecimal y)
    static member ( - ) ((x:decimal), y) = (Operators.fromDecimal x) - y
    static member ( * ) (x, (y:decimal)) = x * (Operators.fromDecimal y)
    static member ( * ) ((x:decimal), y) = (Operators.fromDecimal x) * y
    static member ( / ) (x, (y:decimal)) = x / (Operators.fromDecimal y)
    static member ( / ) ((x:decimal), y) = (Operators.fromDecimal x) / y

    // Simpler usage in C#
    static member op_Implicit (x:int) = Operators.fromInt32 x
    static member op_Implicit (x:int64) = Operators.fromInt64 x
    static member op_Implicit (x:BigInteger) = Operators.fromInteger x
    static member op_Implicit (x:BigRational) = Operators.fromRational x
    static member op_Implicit (x:float) = Operators.fromReal x
    static member op_Implicit (x:float32) = Operators.fromReal32 x
    static member op_Implicit (x:complex) = Operators.fromComplex x
    static member op_Implicit (x:complex32) = Operators.fromComplex32 x
    static member op_Implicit (x:decimal) = Operators.fromDecimal x


[<RequireQualifiedAccess>]
module NumericLiteralQ =

    open Operators

    let FromZero () = zero
    let FromOne () = one
    let FromInt32 (x:int) = fromInt32 x
    let FromInt64 (x:int64) = fromInt64 x
    let FromString str = fromRational (BigRational.Parse str)
