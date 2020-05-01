namespace MathNet.Symbolics.Tests.Visual

open NUnit.Framework
open MathNet.Symbolics
open Operators

module VisualExpressions =

    [<Test>]
    let ``Convert Expression to VisualExpression`` () =

        let style = DefaultVisualStyle()
        let convert = VisualExpression.fromExpression style

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
