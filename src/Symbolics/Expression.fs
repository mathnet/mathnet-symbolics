namespace MathNet.Symbolics

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

    let real (x:float) = Value.real x |> unpack
    let complex (x:Complex) = Value.complex x |> unpack
    let rational (x:BigRational) = Number x

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
        | Approximation x when Approximation.isZero x -> Some Zero
        | _ -> None

    let (|One|_|) = function
        | Number n when n.IsOne -> Some One
        | Approximation x when Approximation.isOne x -> Some One
        | _ -> None

    let (|MinusOne|_|) = function
        | Number n when n.IsInteger && n.Numerator = BigInteger.MinusOne -> Some MinusOne
        | Approximation x when Approximation.isMinusOne x -> Some MinusOne
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
        | Number _ | Identifier _ | Constant _ as t -> Some t
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
    let minusOne = Number (BigRational.FromInt -1)
    let pi = Constant Pi

    let symbol (name:string) = Identifier (Symbol name)

    let undefined = Expression.Undefined
    let infinity = Expression.PositiveInfinity
    let complexInfinity = Expression.ComplexInfinity
    let negativeInfinity = Expression.NegativeInfinity

    let real floatingPoint = Values.real floatingPoint

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
    let isPositiveInfinity = function | PositiveInfinity -> true | _ -> false
    let isNegativeInfinity = function | NegativeInfinity -> true | _ -> false
    let isComplexInfinity = function | ComplexInfinity -> true | _ -> false
    let isInfinity = function | PositiveInfinity | ComplexInfinity | NegativeInfinity -> true | _ -> false

    let internal orderRelation (x:Expression) (y:Expression) =
        let rec compare a b =
            match a, b with
            | Number x, Number y -> x < y
            | Approximation x, Approximation y -> Approximation.orderRelation x y
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
        | Zero -> one
        | One -> Constant E
        | PositiveInfinity -> infinity
        | NegativeInfinity -> zero
        | x -> Function (Exp, x)
    let ln = function
        | Undefined -> undefined
        | Zero -> negativeInfinity
        | One -> zero
        | Constant E -> one
        | oo when isInfinity oo -> infinity
        | x -> Function (Ln, x)
    let log10 = function
        | Undefined -> undefined
        | Zero -> negativeInfinity
        | One -> zero
        | Number n when n.Equals(10N) -> one
        | oo when isInfinity oo -> infinity
        | x -> Function (Log, x)
    let log basis x = FunctionN (Log, [basis; x])

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
    let csc x = Function (Csc, x)
    let sec x = Function (Sec, x)
    let cot x = Function (Cot, x)

    let sinh x = Function (Sinh, x)
    let cosh x = Function (Cosh, x)    
    let tanh x = Function (Tanh, x)
    let csch x = Function (Csch, x)
    let sech x = Function (Sech, x)
    let coth x = Function (Coth, x)

    let arcsin x = Function (Asin, x)
    let arccos x = Function (Acos, x)
    let arctan x = Function (Atan, x)
    let arctan2 x y = FunctionN (Atan, [x;y])
    let arccsc x = Function (Acsc, x)
    let arcsec x = Function (Asec, x)
    let arccot x = Function (Acot, x)

    let arcsinh x = Function (Asinh, x)
    let arccosh x = Function (Acosh, x)
    let arctanh x = Function (Atanh, x)
    let arccsch x = Function (Acsch, x)
    let arcsech x = Function (Asech, x)
    let arccoth x = Function (Acoth, x)

    let apply f x =
        match f with
        | Abs -> abs x
        | Exp -> exp x
        | Ln -> ln x
        | Log -> log10 x
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
        

    let applyN (f: Function) (xs: Expression list) =
        match f, xs with
        | Atan, [x;y] -> arctan2 x y
        | Log, [b; x] -> log b x
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
    static member Symbol (name:string) = Operators.symbol name
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
    static member Log(x) = Operators.log10 x
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

    // Simpler usage - approximations
    static member ( + ) (x, (y:float)) = x + (Operators.real y)
    static member ( + ) ((x:float), y) = (Operators.real x) + y
    static member ( - ) (x, (y:float)) = x - (Operators.real y)
    static member ( - ) ((x:float), y) = (Operators.real x) - y
    static member ( * ) (x, (y:float)) = x * (Operators.real y)
    static member ( * ) ((x:float), y) = (Operators.real x) * y
    static member ( / ) (x, (y:float)) = x / (Operators.real y)
    static member ( / ) ((x:float), y) = (Operators.real x) / y

    // Simpler usage in C#
    static member op_Implicit (x:int) = Operators.fromInt32(x)
    static member op_Implicit (x:int64) = Operators.fromInt64(x)
    static member op_Implicit (x:BigInteger) = Operators.fromInteger(x)
    static member op_Implicit (x:BigRational) = Operators.fromRational(x)
    static member op_Implicit (x:float) = Operators.real x


[<RequireQualifiedAccess>]
module NumericLiteralQ =
    let FromZero () = Expression.Zero
    let FromOne () = Expression.One
    let FromInt32 (x:int) = Expression.FromInt32 x
    let FromInt64 (x:int64) = Expression.FromInt64 x
    let FromString str = Expression.FromRational (BigRational.Parse str)
