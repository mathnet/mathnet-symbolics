namespace MathNet.Symbolics.Tests.Visual

open NUnit.Framework
open FsUnit
open FsUnitTyped
open MathNet.Symbolics
open Operators

module Infix =

    [<Test>]
    let ``Print infix expressions`` () =
        Infix.format (1/(a*b)) --> "1/(a*b)"

    [<Test>]
    let ``Parse infix expressions`` () =
        Infix.parseOrUndefined "-3" ==> "-3"
        Infix.parseOrUndefined "1234567890123456789012345678901234567890" ==> "1234567890123456789012345678901234567890"
        Infix.parseOrUndefined "x" ==> "x"
        Infix.parseOrUndefined "-x" ==> "-x"
        Infix.parseOrUndefined "x-" ==> "Undefined"
        Infix.parseOrUndefined "y*x" ==> "x*y"
        Infix.parseOrUndefined "  y  *  x  " ==> "x*y"
        Infix.parseOrUndefined "a*b*cde*f" ==> "a*b*cde*f"
        Infix.parseOrUndefined "a+b+cde+f" ==> "a + b + cde + f"

        Infix.parseOrUndefined "sin(x)" ==> "sin(x)"
        Infix.parseOrUndefined " sin (x) " ==> "sin(x)"
        Infix.parseOrUndefined " sin ( - x ) " ==> "-sin(x)"
        Infix.parseOrUndefined "sin x" ==> "Undefined"
        Infix.parseOrUndefined "sin x-1" ==> "Undefined"
        Infix.parseOrUndefined "sin -x" ==> "sin - x"
        Infix.parseOrUndefined "sin" ==> "sin"

        Infix.parseOrUndefined "atan2(x,y)" ==> "atan2(x,y)"
        Infix.parseOrUndefined "atan2 ( x , y )"  ==> "atan2(x,y)"
        Infix.parseOrUndefined " atan2 ( - x, - y ) " ==> "atan2(-x,-y)"

        Infix.parseOrUndefined "lg(x)" ==> "lg(x)"
        Infix.parseOrUndefined "log(x,y)" ==> "log(x,y)"
        Infix.parseOrUndefined "log(x,10)" ==> "log(x,10)"

        Infix.parseOrThrow "1/(a*b)" ==> "1/(a*b)"
        Infix.parseOrThrow "exp(a)^exp(b)" ==> "(exp(a))^(exp(b))"
        Infix.parseOrThrow "a^b^c" ==> "a^(b^c)"
        Infix.parseOrThrow "|a-2|-1" ==> "-1 + |-2 + a|"

        Infix.parseOrThrow "(y-1)*10 + 2" ==> "2 + 10*(-1 + y)"

        Infix.parseOrThrow "2*x^(2*y) + e^(3*y)" ==> "e^(3*y) + 2*x^(2*y)"

        Infix.parseOrThrow "15" ==> "15"
        Infix.parseOrThrow "1.5" ==> "1.5"
        Infix.parseOrThrow "0.25" ==> "0.25"
        Infix.parseOrThrow "0.0250" ==> "0.025"
        Infix.parseOrThrow "2.25" ==> "2.25"
        Infix.parseOrThrow "2.250" ==> "2.25"
        Infix.parseOrThrow "0.001" ==> "0.001"
        Infix.parseOrThrow "2.00" ==> "2"

        Infix.parseOrThrow "1.5*a + o" ==> "1.5*a + o"

        Infix.parseOrThrow ".001" ==> "0.001"
        Infix.parseOrThrow ".001" --> Expression.Real(0.001)
        Infix.parseOrThrow "1." ==> "1"
        Infix.parseOrThrow "1." --> Expression.Real(1.0)
        Infix.parseOrThrow "1" ==> "1"
        Infix.parseOrThrow "1" --> Expression.FromInt32(1)

        Infix.parseOrThrow "pi" --> Expression.Pi
        Infix.parseOrThrow "π" --> Expression.Pi
        Infix.parseOrThrow "∞" --> Expression.PositiveInfinity
        Infix.parseOrThrow "inf" --> Expression.PositiveInfinity
        Infix.parseOrThrow "-∞" --> Expression.NegativeInfinity
        Infix.parseOrThrow "⧝" --> Expression.ComplexInfinity

    [<Test>]
    let ``Pseudo Function Test`` () =
        Infix.parseOrUndefined "sqrt(x)" ==> "sqrt(x)"
        Infix.parseOrUndefined "pow(x,3)" ==> "x^3"
        Infix.parseOrUndefined "pow(3*x,10*sin(x))" ==> "(3*x)^(10*sin(x))"
        Infix.parseOrUndefined "sqrt(pow(x,1/2))" ==> "sqrt(sqrt(x))"

    [<Test>]
    let ``Function Powers`` () =
        VisualExpression.Function ("sin", BigInteger.One, [VisualExpression.Symbol "x"]) |> Infix.formatVisual |> shouldEqual "sin(x)"
        VisualExpression.Function ("sin", bigint 2, [VisualExpression.Symbol "x"]) |> Infix.formatVisual |> shouldEqual "sin^2(x)"
        Infix.parseOrThrow "sin^2(x)" ==> "(sin(x))^2"
        Infix.format (sin(x)*sin(x)) --> "(sin(x))^2"

    [<Test>]
    let ``Exponential notation parsing`` () =
        let expr = Infix.parseOrUndefined "(-6.40869140625E-05)*x"
        expr ==> "(-6.40869140625E-05)*x"

        let expr2 = Infix.parseOrUndefined "1.5e7"
        expr2 ==> "15000000"

        let expr3 = Infix.parseOrUndefined "-.5e7"
        expr3 ==> "-5000000"

        let expr4 = Infix.parseOrUndefined "58E-3"
        expr4 ==> "0.058"

    [<Test>]
    let ``Parse Infix to VisualExpression`` () =

        Infix.parseVisual "1" --> Ok (VisualExpression.PositiveInteger (bigint 1))
        Infix.parseVisual "0" --> Ok (VisualExpression.PositiveInteger (bigint 0))
        Infix.parseVisual "-1" --> Ok (VisualExpression.Negative (VisualExpression.PositiveInteger (bigint 1)))

        Infix.parseVisual "1/2" --> Ok (VisualExpression.Fraction ((VisualExpression.PositiveInteger (bigint 1)), (VisualExpression.PositiveInteger (bigint 2))))
        Infix.parseVisual "-1/2" --> Ok (VisualExpression.Fraction ((VisualExpression.Negative (VisualExpression.PositiveInteger (bigint 1))), (VisualExpression.PositiveInteger (bigint 2))))

        Infix.parseVisual "1.0" --> Ok (VisualExpression.PositiveFloatingPoint 1.0)
        Infix.parseVisual "0.0" --> Ok (VisualExpression.PositiveFloatingPoint 0.0)
        Infix.parseVisual "-1.0" --> Ok (VisualExpression.Negative (VisualExpression.PositiveFloatingPoint 1.0))

        Infix.parseVisual "\u221E" --> Ok (VisualExpression.Infinity)
        Infix.parseVisual "oo" --> Ok (VisualExpression.Infinity)
        Infix.parseVisual "-\u221E" --> Ok (VisualExpression.Negative VisualExpression.Infinity)
        Infix.parseVisual "-oo" --> Ok (VisualExpression.Negative VisualExpression.Infinity)
        Infix.parseVisual "\u29DD" --> Ok (VisualExpression.ComplexInfinity)

        Infix.parseVisual "-x*y" --> Ok (VisualExpression.Product [VisualExpression.Negative (VisualExpression.Symbol "x"); VisualExpression.Symbol "y"])
        Infix.parseVisual "x*z*y" --> Ok (VisualExpression.Product [ VisualExpression.Symbol "x"; VisualExpression.Symbol "z"; VisualExpression.Symbol "y"])
        Infix.parseVisual "x+z+y" --> Ok (VisualExpression.Sum [ VisualExpression.Symbol "x"; VisualExpression.Symbol "z"; VisualExpression.Symbol "y"])
        Infix.parseVisual "x*(1-y)" --> Ok (VisualExpression.Product [VisualExpression.Symbol "x"; VisualExpression.Parenthesis (VisualExpression.Sum [ VisualExpression.PositiveInteger (bigint 1); VisualExpression.Negative (VisualExpression.Symbol "y")])])

        Infix.parseVisual "|x|" --> Ok (VisualExpression.Abs (VisualExpression.Symbol "x"))
        Infix.parseVisual "-|x|" --> Ok (VisualExpression.Negative (VisualExpression.Abs (VisualExpression.Symbol "x")))
        Infix.parseVisual "1-|x|" --> Ok (VisualExpression.Sum [ VisualExpression.PositiveInteger (bigint 1); VisualExpression.Negative (VisualExpression.Abs (VisualExpression.Symbol "x"))])
        Infix.parseVisual "-1-|x|" --> Ok (VisualExpression.Sum [ VisualExpression.Negative (VisualExpression.PositiveInteger (bigint 1)); VisualExpression.Negative (VisualExpression.Abs (VisualExpression.Symbol "x"))])
        Infix.parseVisual "|2*x|" --> Ok (VisualExpression.Abs (VisualExpression.Product [ VisualExpression.PositiveInteger (bigint 2); VisualExpression.Symbol "x"]))

        Infix.parseVisual "x^2" --> Ok (VisualExpression.Power (VisualExpression.Symbol "x", VisualExpression.PositiveInteger (bigint 2)))
        Infix.parseVisual "x^-1" --> Ok (VisualExpression.Power (VisualExpression.Symbol "x", VisualExpression.Negative (VisualExpression.PositiveInteger (bigint 1))))
        Infix.parseVisual "x^-2" --> Ok (VisualExpression.Power (VisualExpression.Symbol "x", VisualExpression.Negative (VisualExpression.PositiveInteger (bigint 2))))

        Infix.parseVisual "sin(x)" --> Ok (VisualExpression.Function ("sin", bigint 1, [VisualExpression.Symbol "x"]))

        Infix.parseVisual "sqrt(x)" --> Ok (VisualExpression.Root (VisualExpression.Symbol "x", bigint 2))
        Infix.parseVisual "pow(sin(x),2)" --> Ok (VisualExpression.Power (VisualExpression.Parenthesis (VisualExpression.Function ("sin", bigint 1, [VisualExpression.Symbol "x"])), VisualExpression.PositiveInteger (bigint 2)))


    [<Test>]
    [<TestCase("en-US")>]
    [<TestCase("tr-TR")>]
    [<TestCase("de-DE")>]
    [<TestCase("de-CH")>]
    [<TestCase("he-IL")>]
    let ``Culture Invariant Infix Expressions`` (cultureName:string) =
        let original = System.Threading.Thread.CurrentThread.CurrentCulture
        try
            System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo(cultureName)
            Infix.parseOrThrow "0.25 + 0.1" ==> "0.35"
        finally
            System.Threading.Thread.CurrentThread.CurrentCulture <- original
