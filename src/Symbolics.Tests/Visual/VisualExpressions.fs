namespace MathNet.Symbolics.Tests.Visual

open NUnit.Framework
open MathNet.Symbolics
open Operators

module VisualExpressions =

    [<Test>]
    let ``Convert Expression to VisualExpression`` () =

        let convert = VisualExpression.fromExpression { CompactPowersOfFunctions = false }
        let convertFP = VisualExpression.fromExpression { CompactPowersOfFunctions = true }

        convert (1Q) --> VisualExpression.PositiveInteger (bigint 1)
        convert (0Q) --> VisualExpression.PositiveInteger (bigint 0)
        convert (-1Q) --> VisualExpression.Negative (VisualExpression.PositiveInteger (bigint 1))

        convert (1Q/2) --> VisualExpression.Fraction ((VisualExpression.PositiveInteger (bigint 1)), (VisualExpression.PositiveInteger (bigint 2)))
        convert (-1Q/2) --> VisualExpression.Negative (VisualExpression.Fraction ((VisualExpression.PositiveInteger (bigint 1)), (VisualExpression.PositiveInteger (bigint 2))))

        convert (real 1.0) --> VisualExpression.PositiveFloatingPoint 1.0
        convert (real 0.0) --> VisualExpression.PositiveFloatingPoint 0.0
        convert (real -1.0) --> VisualExpression.Negative (VisualExpression.PositiveFloatingPoint 1.0)

        convert (PositiveInfinity) --> VisualExpression.Infinity
        convert (NegativeInfinity) --> VisualExpression.Negative VisualExpression.Infinity
        convert (ComplexInfinity) --> VisualExpression.ComplexInfinity
        convert (Undefined) --> VisualExpression.Undefined

        convert (-x*y) --> VisualExpression.Negative (VisualExpression.Product [ VisualExpression.Symbol "x"; VisualExpression.Symbol "y"])
        convert (x*(1-y)) --> VisualExpression.Product [VisualExpression.Symbol "x"; VisualExpression.Parenthesis (VisualExpression.Sum [ VisualExpression.PositiveInteger (bigint 1); VisualExpression.Negative (VisualExpression.Symbol "y")])]

        convert (abs x) --> VisualExpression.Abs (VisualExpression.Symbol "x")
        convert (-(abs x)) --> VisualExpression.Negative (VisualExpression.Abs (VisualExpression.Symbol "x"))
        convert (1-(abs x)) --> VisualExpression.Sum [ VisualExpression.PositiveInteger (bigint 1); VisualExpression.Negative (VisualExpression.Abs (VisualExpression.Symbol "x"))]
        convert (-1-(abs x)) --> VisualExpression.Sum [ VisualExpression.Negative (VisualExpression.PositiveInteger (bigint 1)); VisualExpression.Negative (VisualExpression.Abs (VisualExpression.Symbol "x"))]
        convert (abs (2*x)) --> VisualExpression.Abs (VisualExpression.Product [ VisualExpression.PositiveInteger (bigint 2); VisualExpression.Symbol "x"])

        convert (x**(2Q)) --> VisualExpression.Power (VisualExpression.Symbol "x", VisualExpression.PositiveInteger (bigint 2))
        convert (x**(-1Q)) --> VisualExpression.Fraction (VisualExpression.PositiveInteger (bigint 1), VisualExpression.Symbol "x")
        convert (x**(-2Q)) --> VisualExpression.Fraction (VisualExpression.PositiveInteger (bigint 1), VisualExpression.Power (VisualExpression.Symbol "x", VisualExpression.PositiveInteger (bigint 2)))

        convert (sin x) --> VisualExpression.Function ("sin", bigint 1, VisualExpression.Symbol "x")
        convert (pow (sin x) 2Q) --> VisualExpression.Power (VisualExpression.Parenthesis (VisualExpression.Function ("sin", bigint 1, VisualExpression.Symbol "x")), VisualExpression.PositiveInteger (bigint 2))
        convertFP (pow (sin x) 2Q) --> VisualExpression.Function ("sin", bigint 2, VisualExpression.Symbol "x")

    [<Test>]
    let ``Convert VisualExpression to Expression`` () =

        let convert = VisualExpression.toExpression

        convert (VisualExpression.PositiveInteger (bigint 1)) --> 1Q
        convert (VisualExpression.PositiveInteger (bigint 0)) --> 0Q
        convert (VisualExpression.Negative (VisualExpression.PositiveInteger (bigint 1))) --> -1Q

        convert (VisualExpression.Fraction ((VisualExpression.PositiveInteger (bigint 1)), (VisualExpression.PositiveInteger (bigint 2)))) --> 1Q/2
        convert (VisualExpression.Negative (VisualExpression.Fraction ((VisualExpression.PositiveInteger (bigint 1)), (VisualExpression.PositiveInteger (bigint 2))))) --> -1Q/2

        convert (VisualExpression.PositiveFloatingPoint 1.0) --> real 1.0
        convert (VisualExpression.PositiveFloatingPoint 0.0) --> real 0.0
        convert (VisualExpression.Negative (VisualExpression.PositiveFloatingPoint 1.0)) --> real -1.0

        convert (VisualExpression.Infinity) --> PositiveInfinity
        convert (VisualExpression.Negative VisualExpression.Infinity) --> NegativeInfinity
        convert (VisualExpression.ComplexInfinity) --> ComplexInfinity
        convert (VisualExpression.Undefined) --> Undefined

        convert (VisualExpression.Negative (VisualExpression.Product [ VisualExpression.Symbol "x"; VisualExpression.Symbol "y"])) --> (-x*y)
        convert (VisualExpression.Product [VisualExpression.Symbol "x"; VisualExpression.Parenthesis (VisualExpression.Sum [ VisualExpression.PositiveInteger (bigint 1); VisualExpression.Negative (VisualExpression.Symbol "y")])]) --> (x*(1-y))

        convert (VisualExpression.Abs (VisualExpression.Symbol "x")) --> abs x
        convert (VisualExpression.Negative (VisualExpression.Abs (VisualExpression.Symbol "x"))) --> -(abs x)
        convert (VisualExpression.Sum [ VisualExpression.PositiveInteger (bigint 1); VisualExpression.Negative (VisualExpression.Abs (VisualExpression.Symbol "x"))]) --> (1Q-(abs x))
        convert (VisualExpression.Sum [ VisualExpression.Negative (VisualExpression.PositiveInteger (bigint 1)); VisualExpression.Negative (VisualExpression.Abs (VisualExpression.Symbol "x"))]) --> (-1Q-(abs x))
        convert (VisualExpression.Abs (VisualExpression.Product [ VisualExpression.PositiveInteger (bigint 2); VisualExpression.Symbol "x"])) --> abs (2*x)

        convert (VisualExpression.Power (VisualExpression.Symbol "x", VisualExpression.PositiveInteger (bigint 2))) --> (x**(2Q))
        convert (VisualExpression.Fraction (VisualExpression.PositiveInteger (bigint 1), VisualExpression.Symbol "x")) --> (x**(-1Q))
        convert (VisualExpression.Fraction (VisualExpression.PositiveInteger (bigint 1), VisualExpression.Power (VisualExpression.Symbol "x", VisualExpression.PositiveInteger (bigint 2)))) --> (x**(-2Q))

        convert (VisualExpression.Function ("sin", bigint 1, VisualExpression.Symbol "x")) --> sin x
        convert (VisualExpression.Function ("sin", bigint 2, VisualExpression.Symbol "x")) --> pow (sin x) 2Q
