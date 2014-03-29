namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics

//[<CustomComparison>]
//type Number =
//    | Integer of BigInteger
//    | Rational of BigRational
//
//    static member One = Integer (BigInteger.One)
//    static member MinusOne = Integer (BigInteger.MinusOne)
//    static member Zero = Integer (BigInteger.Zero)
//
//    static member OfInt32 (x:int) = Integer (BigInteger(x))
//
//    static member Reduce (x:BigRational) =
//        if x.Denominator = BigInteger.One then Integer x.Numerator
//        else Rational x
//
//    static member ( ~+ ) (x:Number) = x
//    static member ( + ) (x, y) =
//        match (x, y) with
//        | Integer a, Integer b -> Integer (a + b)
//        | Integer a, Rational b | Rational b, Integer a -> Number.Reduce (BigRational.FromBigInt a + b)
//        | Rational a, Rational b -> Number.Reduce (a + b)
//    static member ( ~- ) (x:Number) =
//        match x with
//        | Integer a -> Integer (-a)
//        | Rational a -> Rational (-a)
//    static member ( - ) (x, y) =
//        match (x, y) with
//        | Integer a, Integer b -> Integer (a - b)
//        | Integer a, Rational b -> Number.Reduce (BigRational.FromBigInt a - b)
//        | Rational a, Integer b -> Number.Reduce (a - BigRational.FromBigInt b)
//        | Rational a, Rational b -> Number.Reduce (a - b)
//    static member ( * ) (x, y) =
//        match (x, y) with
//        | Integer a, Integer b -> Integer (a * b)
//        | Integer a, Rational b | Rational b, Integer a -> Number.Reduce (BigRational.FromBigInt a * b)
//        | Rational a, Rational b -> Number.Reduce (a * b)
//    static member ( / ) (x, y) =
//        match (x, y) with
//        | Integer a, Integer b -> Number.Reduce (BigRational.FromBigInt a / BigRational.FromBigInt b)
//        | Integer a, Rational b -> Number.Reduce (BigRational.FromBigInt a / b)
//        | Rational a, Integer b -> Number.Reduce (a / BigRational.FromBigInt b)
//        | Rational a, Rational b -> Number.Reduce (a / b)
//
//    static member Pow (x, (n:int)) =
//        match x with
//        | Integer a -> Integer (BigInteger.Pow (a, n))
//        | Rational a -> Number.Reduce (BigRational.PowN (a, n))
//
//    static member Invert (x) =
//        match x with
//        | Integer a -> Number.Reduce (BigRational.One / BigRational.FromBigInt(a))
//        | Rational a -> Number.Reduce (BigRational.One / a)
//
//    interface System.IComparable<Number> with
//        member x.CompareTo y =
//            match (x, y) with
//            | Integer a, Integer b -> compare a b
//            | Integer a, Rational b -> compare (BigRational.FromBigInt a) b
//            | Rational a, Integer b -> compare a (BigRational.FromBigInt b)
//            | Rational a, Rational b -> compare a b
//
////    interface System.IComparable with
////        member x.CompareTo y =
////            match y with | :? Number as n -> x.CompareTo()
//
////    override x.Equals(other) = equalsOn
////        match x, other with
////        | Integer a, (:? Number (Integer n) as b) -> a < b
////        | Integer a, Rational b -> BigRational.FromBigInt a < b
////        | Rational a, Integer b -> a < BigRational.FromBigInt b
////        | Rational a, Rational b -> a < b
//       
//
//    static member op_LessThan (x, y) =
//        failwith "test"
//        match (x, y) with
//        | Integer a, Integer b -> a < b
//        | Integer a, Rational b -> BigRational.FromBigInt a < b
//        | Rational a, Integer b -> a < BigRational.FromBigInt b
//        | Rational a, Rational b -> a < b
//
//    static member op_GreaterThan (x, y) =
//        failwith "test"
//        match (x, y) with
//        | Integer a, Integer b -> a > b
//        | Integer a, Rational b -> BigRational.FromBigInt a > b
//        | Rational a, Integer b -> a > BigRational.FromBigInt b
//        | Rational a, Rational b -> a > b
//
//    static member Max (x, y) =
//        match (x, y) with
//        | Integer a, Integer b -> Integer (BigInteger.Max(a, b))
//        | Integer a, Rational b -> let aa = BigRational.FromBigInt a in if aa > b then x else y
//        | Rational a, Integer b -> let bb = BigRational.FromBigInt b in if bb > a then y else x
//        | Rational a, Rational b -> if a > b then x else y
//
//    static member Min (x, y) =
//        match (x, y) with
//        | Integer a, Integer b -> Integer (BigInteger.Min(a, b))
//        | Integer a, Rational b -> let aa = BigRational.FromBigInt a in if aa < b then x else y
//        | Rational a, Integer b -> let bb = BigRational.FromBigInt b in if bb < a then y else x
//        | Rational a, Rational b -> if a < b then x else y


//#nowarn "25"
//module Integers =
//
//    let quot (Integer a) (Integer b) = Integer (a / b)
//    let remainder (Integer a) (Integer b) = Integer (a % b)
//    let modulo (Integer a) (Integer b) = Integer (((a % b) + b) % b)
//
//    let gcd2 (Integer a) (Integer b) = Integer (Euclid.GreatestCommonDivisor(a, b))
//    let gcde (Integer a) (Integer b) = let g, x, y = Euclid.ExtendedGreatestCommonDivisor(a, b) in (Integer g, Integer x, Integer y)
//    let lcm2 (Integer a) (Integer b) = Integer (Euclid.LeastCommonMultiple(a, b))
//
//    // TODO: proper BigInteger implementation (supporting large numbers)
//    let factorial (Integer a) = Number.OfInt32(int(SpecialFunctions.Factorial(int a)))
//    let binomial (Integer n) (Integer k) = Number.OfInt32(int(SpecialFunctions.Binomial(int n, int k)))
