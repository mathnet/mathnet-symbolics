namespace MathNet.Symbolics.Tests.Visual

open NUnit.Framework
open MathNet.Symbolics
open Operators

module Infix =

    [<Test>]
    let ``Print infix expressions`` () =
        Infix.format (1/(a*b)) --> "1/(a*b)"
        Infix.formatStrict (1/(a*b)) --> "a^(-1)*b^(-1)"

    [<Test>]
    let ``Parse infix expressions`` () =
        Infix.parseOrUndefined "-3" ==> "-3"
        Infix.parseOrUndefined "1234567890123456789012345678901234567890" ==> "1234567890123456789012345678901234567890"
        Infix.parseOrUndefined "x" ==> "x"
        Infix.parseOrUndefined "-x" ==> "-x"
        Infix.parseOrUndefined "x-" ==> "Undefined"
        Infix.parseOrUndefined "y*x" ==> "x*y"
        Infix.parseOrUndefined "  y  *  x  " ==> "x*y"

        Infix.parseOrUndefined "sin(x)" ==> "sin(x)"
        Infix.parseOrUndefined " sin (x) " ==> "sin(x)"
        Infix.parseOrUndefined " sin ( - x ) " ==> "-sin(x)"
        Infix.parseOrUndefined "sin x" ==> "Undefined"
        Infix.parseOrUndefined "sin x-1" ==> "Undefined"
        Infix.parseOrUndefined "sin -x" ==> "sin - x"
        Infix.parseOrUndefined "sin" ==> "sin"

        Infix.parseOrUndefined "atan(x,y)" ==> "atan(x,y)"
        Infix.parseOrUndefined "atan ( x , y )"  ==> "atan(x,y)"
        Infix.parseOrUndefined " atan ( - x, - y ) " ==> "atan(-x,-y)"

        Infix.parseOrUndefined "log(x)" ==> "log(x)"
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
        Infix.parseOrUndefined "sqrt(x)" ===> "x^(1/2)"
        Infix.parseOrUndefined "sqrt(x)" ==> "sqrt(x)"
        Infix.parseOrUndefined "pow(x,3)" ==> "x^3"
        Infix.parseOrUndefined "pow(3*x,10*sin(x))" ==> "(3*x)^(10*sin(x))"
        Infix.parseOrUndefined "sqrt(pow(x,1/2))" ===> "(x^(1/2))^(1/2)"
        Infix.parseOrUndefined "sqrt(pow(x,1/2))" ==> "sqrt(sqrt(x))"

    [<Test>]
    let ``Underscores in names`` () =
        let expr = Infix.parseOrUndefined "(TESTING_UNDER)*(2)"
        expr ==> "2*TESTING_UNDER"
        LaTeX.format expr --> """2{TESTING_{UNDER}}"""

        let expr2 = Infix.parseOrUndefined "(TESTING_UNDER_second)*(2)"
        expr2 ==> "2*TESTING_UNDER_second"
        LaTeX.format expr2 --> """2{TESTING_{UNDER_{second}}}"""

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
