module Tests

#if INTERACTIVE
#load "Interactive.fsx"
#endif

open Expecto

open System.Collections.Generic
open MathNet.Numerics
open MathNet.Symbolics

open Operators

// Test: x should evaluate to expected
let inline (-->) x expected = Expect.equal x expected ""

// Test: x should evaluate to the expected string when formatted *nicely*
let inline (==>) x expected = Expect.equal (Infix.format x) expected ""

// Test: x should evaluate to the expected string when formatted *strictly* (not denormalized)
let inline (===>) x expected = Expect.equal (Infix.formatStrict x) expected ""

// extra test helpers for tuples, list, arrays and hash-sets - maybe there's a better way?
let inline (==|>) (x1, x2) expected = Expect.equal (Infix.format x1, Infix.format x2) expected ""
let inline (==||>) (x1, x2, x3) expected = Expect.equal (Infix.format x1, Infix.format x2, Infix.format x3) expected ""
let inline (==+>) x expected = Expect.equal (List.map Infix.format x) expected ""
let inline (==->) x expected = Expect.equal (Array.map Infix.format x) expected ""
let inline (==*>) (x:HashSet<Expression>) (expected:string list) = Expect.isTrue (HashSet(expected).SetEquals(x |> Seq.map Infix.format)) ""

// extra test helper for MathML (just normalizing XML, really)
let inline (==/>) (x:string) expected = Expect.equal x (Xml.normalizeString expected) ""

// variables
let x = symbol "x"
let y = symbol "y"
let z = symbol "z"
let a = symbol "a"
let b = symbol "b"
let c = symbol "c"
let d = symbol "d"
let e = symbol "e"
let f = symbol "f"

[<Tests>]
let tests =
    testList "All Tests" [

        test "Number Expressions" {

            // equivalent:
            number 3 ==> "3"
            3Q ==> "3"

            // expressions are not comparable (NoComparison) to prevent errors,
            // but if the expressions are numbers we can use compareNumber:
            Numbers.compare 0Q 1Q --> -1
            Numbers.compare 1Q 1Q --> 0
            Numbers.compare 1Q 2Q --> -1
            Numbers.compare 0Q (1Q/2Q) --> -1
            Numbers.compare 1Q (1Q/2Q) --> 1
            Numbers.compare (1Q/2Q) 0Q --> 1
            Numbers.compare (1Q/2Q) 1Q --> -1
            Numbers.compare 1Q infinity --> -1
            Numbers.compare 1Q complexInfinity --> -1
            Numbers.compare 1Q negativeInfinity --> 1
            Numbers.compare infinity 1Q --> 1
            Numbers.compare complexInfinity 1Q --> 1
            Numbers.compare negativeInfinity 1Q --> -1
            Numbers.compare negativeInfinity infinity --> -1
            Numbers.compare infinity negativeInfinity --> 1

            Numbers.max [ 2Q; 4Q; 7Q/2 ] --> 4Q
            Numbers.max [ 2Q; 4Q; 9Q/2 ] --> 9Q/2
            Numbers.max [ -2Q; -4Q; -7Q/2 ] --> -2Q

            Numbers.gcd [ 4Q; 6Q; 10Q ] --> 2Q
            Numbers.lcm [ 4Q; 6Q; 10Q ] --> 60Q

            number 2 + number 5 ==> "7"
            2 * number 2 ==> "4"
        }

        test "Constant Expressions" {

            Expression.Pi ==> "π"
            Expression.E ==> "e"
            Expression.I ==> "j"
            Expression.Real(1.23) ==> "1.23"
            Expression.Real(-0.23) ==> "-0.23"

            real 1.1 + real 2.2 ==> "3.3"
            real 1.1 * real 2.2 ==> "2.42"

            2 * real 2.0 ==> "4"

            x + 2*x ==> "3*x"
            x + 2.2*x ==> "3.2*x"
        }

        test "Real Infinity Expressions" {

            infinity ==> "∞"
            -infinity ==> "-∞"
            infinity + infinity ==> "∞"
            infinity - infinity ==> "Undefined"
            -infinity - infinity ==> "-∞"
            2*infinity ==> "∞"
            -2*infinity ==> "-∞"
            infinity + 2Q ==> "∞"
            infinity*infinity ==> "∞"
            infinity*(-2*infinity) ==> "-∞"
        }

        test "Complex Infinity Expressions" {

            complexInfinity ==> "⧝"
            -complexInfinity ==> "⧝"
            complexInfinity + complexInfinity ==> "Undefined"
            complexInfinity - complexInfinity ==> "Undefined"
            2*complexInfinity ==> "⧝"
            complexInfinity + 2Q ==> "⧝"
            complexInfinity*complexInfinity ==> "⧝"
            complexInfinity*(-2*complexInfinity) ==> "⧝"
            complexInfinity+x*complexInfinity ==> "Undefined"
            complexInfinity*(x+complexInfinity) ==> "⧝"

            complexInfinity + infinity ==> "Undefined"
            complexInfinity - infinity ==> "Undefined"
        }

        test "Zero, One, Infinity, and ComplexInfinity" {

            // Operators.add

            0Q + undefined ==> "Undefined"
            0Q + infinity ==> "∞"
            0Q + negativeInfinity ==> "-∞"
            0Q + complexInfinity ==> "⧝"

            infinity + undefined ==> "Undefined"
            infinity + 1Q ==> "∞"
            infinity - 1Q ==> "∞"
            infinity + infinity ==> "∞"
            infinity - infinity ==> "Undefined"
            infinity + complexInfinity ==> "Undefined"
            infinity - complexInfinity ==> "Undefined"

            negativeInfinity + undefined ==> "Undefined"
            negativeInfinity + 1Q ==> "-∞"
            negativeInfinity - 1Q ==> "-∞"
            negativeInfinity + infinity ==> "Undefined"
            negativeInfinity - infinity ==> "-∞"
            negativeInfinity + complexInfinity ==> "Undefined"
            negativeInfinity-complexInfinity ==> "Undefined"

            complexInfinity + undefined ==> "Undefined"
            complexInfinity + 1Q ==> "⧝"
            complexInfinity - 1Q ==> "⧝"
            complexInfinity + infinity ==> "Undefined"
            complexInfinity - infinity ==> "Undefined"
            complexInfinity + complexInfinity ==> "Undefined"
            complexInfinity - complexInfinity ==> "Undefined"

            // Operators.abs

            abs undefined ==> "Undefined"
            abs infinity ==> "∞"
            abs negativeInfinity ==> "∞"
            abs complexInfinity ==> "∞"
            abs Expression.I ==> "1"

            // Operators.invert

            invert undefined ==> "Undefined"
            invert 0Q ==> "⧝"
            invert infinity ==> "0"
            invert negativeInfinity ==> "0"
            invert complexInfinity ==> "0"

            // Operators.multiply

            0Q*undefined ==> "Undefined"
            0Q*infinity ==> "Undefined"
            0Q*negativeInfinity ==> "Undefined"
            0Q*complexInfinity ==> "Undefined"

            1Q*undefined ==> "Undefined"
            1Q*infinity ==> "∞"
            1Q*negativeInfinity ==> "-∞"
            1Q*complexInfinity ==> "⧝"

            (-1Q)*undefined ==> "Undefined"
            (-1Q)*infinity ==> "-∞"
            (-1Q)*negativeInfinity ==> "∞"
            (-1Q)*complexInfinity ==> "⧝"

            infinity*undefined ==> "Undefined"
            infinity*0Q ==> "Undefined"
            infinity*1Q ==> "∞"
            infinity*(-1Q) ==> "-∞"
            infinity*infinity ==> "∞"
            infinity*negativeInfinity ==> "-∞"
            infinity*complexInfinity ==> "⧝"

            negativeInfinity*undefined ==> "Undefined"
            negativeInfinity*0Q ==> "Undefined"
            negativeInfinity*1Q ==> "-∞"
            negativeInfinity*(-1Q) ==> "∞"
            negativeInfinity*infinity ==> "-∞"
            negativeInfinity*negativeInfinity ==> "∞"
            negativeInfinity*complexInfinity ==> "⧝"

            complexInfinity*undefined ==> "Undefined"
            complexInfinity*0Q ==> "Undefined"
            complexInfinity*1Q ==> "⧝"
            complexInfinity*(-1Q) ==> "⧝"
            complexInfinity*infinity ==> "⧝"
            complexInfinity*negativeInfinity ==> "⧝"
            complexInfinity*complexInfinity ==> "⧝"

            // Operators.divide

            0Q/undefined ==> "Undefined"
            0Q/0Q ==> "Undefined"
            0Q/1Q ==> "0"
            0Q/infinity ==> "0"
            0Q/negativeInfinity ==> "0"
            0Q/complexInfinity ==> "0"

            1Q/undefined ==> "Undefined"
            1Q/0Q ==> "⧝"
            1Q/1Q ==> "1"
            1Q/(-1Q) ==> "-1"
            1Q/infinity ==> "0"
            1Q/negativeInfinity ==> "0"
            1Q/complexInfinity ==> "0"

            (-1Q)/undefined ==> "Undefined"
            (-1Q)/0Q ==> "⧝"
            (-1Q)/1Q ==> "-1"
            (-1Q)/(-1Q) ==> "1"
            (-1Q)/infinity ==> "0"
            (-1Q)/negativeInfinity ==> "0"
            (-1Q)/complexInfinity ==> "0"

            infinity/undefined ==> "Undefined"
            infinity/0Q ==> "⧝"
            infinity/1Q ==> "∞"
            infinity/(-1Q) ==> "-∞"
            infinity/infinity ==> "Undefined"
            infinity/negativeInfinity ==> "Undefined"
            infinity/complexInfinity ==> "Undefined"

            negativeInfinity/undefined ==> "Undefined"
            negativeInfinity/0Q ==> "⧝"
            negativeInfinity/1Q ==> "-∞"
            negativeInfinity/(-1Q) ==> "∞"
            negativeInfinity/infinity ==> "Undefined"
            negativeInfinity/negativeInfinity ==> "Undefined"
            negativeInfinity/complexInfinity ==> "Undefined"

            complexInfinity/undefined ==> "Undefined"
            complexInfinity/0Q ==> "⧝"
            complexInfinity/1Q ==> "⧝"
            complexInfinity/(-1Q) ==> "⧝"
            complexInfinity/infinity ==> "Undefined"
            complexInfinity/negativeInfinity ==> "Undefined"
            complexInfinity/complexInfinity ==> "Undefined"

            // Operators.pow

            0Q**undefined ==> "Undefined"
            0Q**0Q ==> "Undefined"
            0Q**1Q ==> "0"
            0Q**2Q ==> "0"
            0Q**(-1Q) ==> "⧝"
            0Q**(-2Q) ==> "⧝"
            0Q**infinity ==> "0"
            0Q**negativeInfinity ==> "⧝"
            0Q**complexInfinity ==> "0"

            1Q**undefined ==> "Undefined"
            1Q**0Q ==> "1"
            1Q**1Q ==> "1"
            1Q**2Q ==> "1"
            1Q**(-1Q) ==> "1"
            1Q**(-2Q) ==> "1"
            1Q**infinity ==> "Undefined"
            1Q**negativeInfinity ==> "Undefined"
            1Q**complexInfinity ==> "Undefined"

            2Q**undefined ==> "Undefined"
            2Q**0Q ==> "1"
            2Q**1Q ==> "2"
            2Q**2Q ==> "4"
            2Q**(-1Q) ==> "1/2"
            2Q**(-2Q) ==> "1/4"
            2Q**infinity ==> "∞"
            2Q**negativeInfinity ==> "0"
            2Q**complexInfinity ==> "Undefined"

            (-1Q)**undefined ==> "Undefined"
            (-1Q)**0Q ==> "1"
            (-1Q)**1Q ==> "-1"
            (-1Q)**2Q ==> "1"
            (-1Q)**(-1Q) ==> "-1"
            (-1Q)**(-2Q) ==> "1"
            (-1Q)**infinity ==> "Undefined"
            (-1Q)**negativeInfinity ==> "Undefined"
            (-1Q)**complexInfinity ==> "Undefined"

            (-2Q)**undefined ==> "Undefined"
            (-2Q)**0Q ==> "1"
            (-2Q)**1Q ==> "-2"
            (-2Q)**2Q ==> "4"
            (-2Q)**(-1Q) ==> "-1/2"
            (-2Q)**(-2Q) ==> "1/4"
            (-2Q)**infinity ==> "⧝"
            (-2Q)**negativeInfinity ==> "0"
            (-2Q)**complexInfinity ==> "Undefined"

            infinity**undefined ==> "Undefined"
            infinity**0Q ==> "Undefined"
            infinity**1Q ==> "∞"
            infinity**(-1Q) ==> "0"
            infinity**(-2Q) ==> "0"
            infinity**infinity ==> "⧝"
            infinity**negativeInfinity ==> "0"
            infinity**complexInfinity ==> "Undefined"

            negativeInfinity**undefined ==> "Undefined"
            negativeInfinity**0Q ==> "Undefined"
            negativeInfinity**1Q ==> "-∞"
            negativeInfinity**2Q ==> "∞"
            negativeInfinity**(-1Q) ==> "0"
            negativeInfinity**(-2Q) ==> "0"
            negativeInfinity**infinity ==> "⧝"
            negativeInfinity**negativeInfinity ==> "0"
            negativeInfinity**complexInfinity ==> "Undefined"

            complexInfinity**undefined ==> "Undefined"
            complexInfinity**0Q ==> "Undefined"
            complexInfinity**1Q ==> "⧝"
            complexInfinity**(-1Q) ==> "0"
            complexInfinity**(-2Q) ==> "0"
            complexInfinity**infinity ==> "⧝"
            complexInfinity**negativeInfinity ==> "0"
            complexInfinity**complexInfinity ==> "Undefined"

            root 0Q x ==> "Undefined"
            root infinity x ==> "1"
            x**(1/infinity) ==>"1"
            root negativeInfinity x ==> "1"
            root complexInfinity x ==> "1"
            root infinity 0Q ==> "Undefined"
            root negativeInfinity 0Q ==> "Undefined"
            root complexInfinity 0Q ==> "Undefined" // In WolframAlpha, root(complexInfinity, 0) returns 0
            0Q**(1/complexInfinity) ==> "Undefined"

            // Operators.exp

            exp undefined ==> "Undefined"
            exp infinity ==> "∞"
            exp negativeInfinity ==> "0"
            exp complexInfinity ==> "Undefined"
            exp 0Q ==> "1"
            exp 1Q ==> "e"

            // Operators.ln

            ln undefined ==> "Undefined"
            ln infinity ==> "∞"
            ln negativeInfinity ==> "∞"
            ln complexInfinity ==> "∞"
            ln 0Q ==> "-∞"
            ln 1Q ==> "0"
            ln Expression.E ==> "1"

            // Operators.log10

            log10 undefined ==> "Undefined"
            log10 infinity ==> "∞"
            log10 negativeInfinity ==> "∞"
            log10 complexInfinity ==> "∞"
            log10 0Q ==> "-∞"
            log10 1Q ==> "0"
            log10 10Q ==> "1"

            // Todo - Imaginary related
            //Expression.I*infinity ==> "∞*j"
            //0Q**Expression.I ==> "Undefined"
            1Q**Expression.I ==> "1"
        }

        test "Expressions are always in auto-simplified form" {

            // readable output is F# interactive thanks to Text.format printer added in MathNet.Symbolics.fsx

            x + y ==> "x + y"
            y + x ==> "x + y"
            x + x ==> "2*x"
            x + 2*x ==> "3*x"
            x + x*2 ==> "3*x"
            2*x + 3*x ==> "5*x"
            a*x + 2*x ==> "2*x + a*x"
            a*x + x*b ==> "a*x + b*x"
            b*x + x*a ==> "a*x + b*x"
            1 + x + y ==> "1 + x + y"
            x + 1 + y ==> "1 + x + y"
            x + y + 1 ==> "1 + x + y"

            x*y ==> "x*y"
            y*x ==> "x*y"
            x**2*x ==> "x^3"
            x*y*x**2 ==> "x^3*y"
            y*x*y**2 ==> "x*y^3"
            2*x*y ==> "2*x*y"
            x*2*y ==> "2*x*y"
            x*y*2 ==> "2*x*y"

            2*(a*b) ==> "2*a*b"
            (a*b)*2 ==> "2*a*b"
            a*b + a*b ==> "2*a*b"
            a*b + b*a ==> "2*a*b"

            a + b + c + a*b + a*c + b*c ==> "a + b + a*b + c + a*c + b*c"
            c*b + c*a + b*a + c + b + a ==> "a + b + a*b + c + a*c + b*c"

            a**2 + b**2 ==> "a^2 + b^2"
            b**2 + a**2 ==> "a^2 + b^2"

            a**2 + a**3 ==> "a^2 + a^3"
            a**3 + a**2 ==> "a^2 + a^3"

            a**2 * b**2 ==> "a^2*b^2"
            b**2 * a**2 ==> "a^2*b^2"

            (a+c)**2 + (a+b)**2 ==> "(a + b)^2 + (a + c)^2"
            (a+b)**2 + (a+c)**2 ==> "(a + b)^2 + (a + c)^2"

            (a+c)**2 * (a+b)**2 ==> "(a + b)^2*(a + c)^2"
            (a+b)**2 * (a+c)**2 ==> "(a + b)^2*(a + c)^2"

            (a+c) * (a+b) ==> "(a + b)*(a + c)"
            (a+b) * (a+c) ==> "(a + b)*(a + c)"

            (1+x)**2 + (1+x)**3 + (1+y)**2 ==> "(1 + x)^2 + (1 + x)^3 + (1 + y)^2"
            (1+x)**3 + (1+y)**2 + (1+x)**2 ==> "(1 + x)^2 + (1 + x)^3 + (1 + y)^2"
            (1+y)**2 + (1+x)**2 + (1+x)**3 ==> "(1 + x)^2 + (1 + x)^3 + (1 + y)^2"

            (a+b)*x ==> "(a + b)*x"
            (a+b)*x*y ==> "(a + b)*x*y"
            (a+b)*y*x ==> "(a + b)*x*y"
            (a+b)*(x*y) ==> "(a + b)*x*y"
            (a+b)*(y*x) ==> "(a + b)*x*y"

            x*x ==> "x^2"
            x*x**2*x**3 ==> "x^6"
            (x**2)**3 ==> "x^6"

            2*(x*y)*z**2 ==> "2*x*y*z^2"
            1*x*y*z**2 ==> "x*y*z^2"
            2*x*y*z*z**2 ==> "2*x*y*z^3"

            (-2) + (-3)*x + 5*y ==> "-2 - 3*x + 5*y"
            (-2.0) + (-3.0)*x + 5.0*y ==> "-2 - 3*x + 5*y"
            (2*x)/(3*y) ==> "(2*x)/(3*y)"
            (1*x)/(3*y) ==> "x/(3*y)"
            (a*x)/(3*y) ==> "(a*x)/(3*y)"
            (2*x)/(1*y) ==> "(2*x)/y"
            (2*x)/(b*y) ==> "(2*x)/(b*y)"
            (2)/(3*b*y) ==> "2/(3*b*y)"
            (1)/(3*b*y) ==> "1/(3*b*y)"
            (2*a*x)/(3) ==> "2/3*a*x"
            (1*a*x)/(3) ==> "1/3*a*x"
            (-2*x)/(3*y) ==> "-(2*x)/(3*y)"
            (-1*x)/(3*y) ==> "-x/(3*y)"
            (-a*x)/(3*y) ==> "-(a*x)/(3*y)"
            (-2*x)/(1*y) ==> "-(2*x)/y"
            (-2*x)/(b*y) ==> "-(2*x)/(b*y)"
            (-2)/(3*b*y) ==> "-2/(3*b*y)"
            (-1)/(3*b*y) ==> "-1/(3*b*y)"
            (-2*a*x)/(3) ==> "-2/3*a*x"
            (-1*a*x)/(3) ==> "-1/3*a*x"

            // There is no subtraction, negation or division in simplified expressions (strict):
            1 / x ===> "x^(-1)" // strict
            1 / x ==> "1/x" // nice
            -x ===> "(-1)*x"
            -x ==> "-x"
            2 + 1/x - 1 ===> "1 + x^(-1)"
            2 + 1/x - 1 ==> "1 + 1/x"
            -(-x) ===> "x"
            -(-x) ==> "x"
            1 / (1 / x) ===> "x"
            1 / (1 / x) ==> "x"

            2*x*3 ==> "6*x"
            -x*y/3 ===> "(-1/3)*x*y"
            -x*y/3 ==> "-1/3*x*y"

            ((x*y)**(1Q/2)*z**2)**2 ==> "x*y*z^4"
            (a/b/(c*a))*(c*d/a)/d ===> "a^(-1)*b^(-1)" // strict
            (a/b/(c*a))*(c*d/a)/d ==> "1/(a*b)" // nice
            a**(3Q/2)*a**(1Q/2) ==> "a^2"

            x + ln x ==> "x + ln(x)"
            x + ln (x+1) ==> "x + ln(1 + x)"
            x + log10 (x+1) ==> "x + log(1 + x)"
            x + (log x (x+1)) ==> "x + log(x,1 + x)"
            2*abs x ==> "2*|x|"
            x + abs (-x) ==> "x + |x|"
            abs (-3Q) ==> "3"
            exp 0Q ==> "1"

            sin x ==> "sin(x)"
        }

        test "Convert Expression to VisualExpression" {

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
        }

        test "Print infix and LaTeX expressions" {

            Infix.format (1/(a*b)) --> "1/(a*b)"
            Infix.formatStrict (1/(a*b)) --> "a^(-1)*b^(-1)"
        }

        test "Parse infix expressions" {

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
        }

        testList "Culture Invariant Infix Expressions" [
            let cultureInvariantTest (cultureName:string) =
                let original = System.Threading.Thread.CurrentThread.CurrentCulture
                try
                    System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo(cultureName)
                    Infix.parseOrThrow "0.25 + 0.1" ==> "0.35"
                finally
                    System.Threading.Thread.CurrentThread.CurrentCulture <- original
            yield test "en-US" { cultureInvariantTest "en-US" }
            yield test "tr-TR" { cultureInvariantTest "tr-TR" }
            yield test "de-DE" { cultureInvariantTest "de-DE" }
            yield test "de-CH" { cultureInvariantTest "de-CH" }
            yield test "he-IL" { cultureInvariantTest "he-IL" }
        ]

        test "Print LaTeX expressions" {

            LaTeX.format (1/(a*b)) --> """\frac{1}{ab}"""
            LaTeX.format Expression.MinusOne --> """-1"""
            LaTeX.format Expression.ComplexInfinity --> """\infty"""
            LaTeX.format Expression.Pi --> """\pi"""
            LaTeX.format (Expression.Real -0.23) --> string -0.23
            LaTeX.format (a**b) --> """{a}^{b}"""
            LaTeX.format (a**(b+c)) --> """{a}^{b + c}"""
            LaTeX.format ((a+b)**c) --> """{\left(a + b\right)}^{c}"""
            LaTeX.format (a**(b**c)) --> """{a}^{{b}^{c}}"""
            LaTeX.format ((a**b)**c) --> """{\left({a}^{b}\right)}^{c}"""
            LaTeX.format (a*b*(symbol "def")) --> """ab{def}"""

            LaTeX.format (3Q*2Q**x) --> """3\cdot{2}^{x}"""
            LaTeX.format (3.0*(real 2.0)**x) --> """3\cdot{2}^{x}"""
            LaTeX.format (5Q*x) --> """5x"""
            LaTeX.format (Expression.Pi * 10Q) --> """10\pi"""
            LaTeX.format (Expression.E * 2Q**(4Q*x)) --> """e\cdot{2}^{4x}"""
            LaTeX.format (4Q * Expression.E ** x) --> """4{e}^{x}"""

            LaTeX.format (log10 x) --> """\log_{10}{x}"""
            LaTeX.format (log10 (x+y)) --> """\log_{10}\left(x + y\right)"""
            LaTeX.format (log 8Q y) --> """\log_{8}{y}"""
            LaTeX.format (log 8Q (x+y)) --> """\log_{8}\left(x + y\right)"""
            LaTeX.format (log (sin x) (tanh y)) --> """\log_{\sin{x}}{\tanh{y}}"""
            LaTeX.format (arctan x) --> """\arctan{x}"""
            LaTeX.format (arctan2 x (3Q*y)) --> """\operatorname{atan2}\left({x}, {3y}\right)"""

            LaTeX.format (sin (x+y)) --> """\sin\left(x + y\right)"""
            LaTeX.format (sin ((x+y) ** 2)) --> """\sin{{\left(x + y\right)}^{2}}"""
            LaTeX.format ((sin (x+y)) ** 2) --> """{\left(\sin\left(x + y\right)\right)}^{2}"""
            LaTeX.format ((sin x)*(cos x)+(tan x)) --> """\sin{x}\cos{x} + \tan{x}"""
            LaTeX.format ((sin (x+y))*(cos (x+y))+(tan (x+y))) --> """\sin\left(x + y\right)\cos\left(x + y\right) + \tan\left(x + y\right)"""

            LaTeX.format (x**(1Q/2)) --> "\sqrt{x}"
            LaTeX.format (x**(1Q/3)) --> "\sqrt[3]{x}"
        }

        test "Format MathML3 Strict Content" {

            MathML.formatContentStrict 1Q ==/> """<cn>1</cn>"""
            MathML.formatContentStrict -1Q ==/> """<cn>-1</cn>"""
            MathML.formatContentStrict (1Q/2) ==/> """<apply><csymbol cd="nums1">rational</csymbol><cn>1</cn><cn>2</cn></apply>"""
            MathML.formatContentStrict x ==/> """<ci>x</ci>"""
            MathML.formatContentStrict -x ==/> """<apply><csymbol cd="arith1">unary_minus</csymbol><ci>x</ci></apply>"""
            MathML.formatContentStrict (-2*x) ==/> """<apply><csymbol cd="arith1">unary_minus</csymbol><apply><csymbol cd="arith1">times</csymbol><cn>2</cn><ci>x</ci></apply></apply>"""
            MathML.formatContentStrict pi ==/> """<csymbol cd="nums1">pi</csymbol>"""
            MathML.formatContentStrict (1/x) ==/> """<apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><ci>x</ci></apply>"""
            MathML.formatContentStrict (1/(a*b)) ==/> """<apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><apply><csymbol cd="arith1">times</csymbol><ci>a</ci><ci>b</ci></apply></apply>"""
            MathML.formatContentStrict (x**2) ==/> """<apply><csymbol cd="arith1">power</csymbol><ci>x</ci><cn>2</cn></apply>"""
            MathML.formatContentStrict (x**(1Q/2)) ==/> """<apply><csymbol cd="arith1">root</csymbol><ci>x</ci><cn>2</cn></apply>"""
            MathML.formatContentStrict (x**(1Q/3)) ==/> """<apply><csymbol cd="arith1">root</csymbol><ci>x</ci><cn>3</cn></apply>"""
        }

        test "Format MathML3 Strict Content with Annotations" {

            MathML.formatSemanticsAnnotated (1/x) ==/> """
              <semantics>
                <apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><ci>x</ci></apply>
                <annotation encoding="application/x-tex">\frac{1}{x}</annotation>
                <annotation encoding="application/x-mathnet-infix">1/x</annotation>
              </semantics>"""
        }

        test "Parse MathML3 Strict Content" {

            MathML.parse """<ci>x</ci>""" ==> "x"
            MathML.parse """<cn>1</cn>""" ==> "1"
            MathML.parse """<csymbol cd="nums1">pi</csymbol>""" ==> "π"
            MathML.parse """<apply> <csymbol cd="nums1">rational</csymbol> <cn>1</cn> <cn>2</cn> </apply>""" ==> "1/2"
            MathML.parse """<apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><ci>x</ci></apply>""" ==> "1/x"
            MathML.parse """<apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><apply><csymbol cd="arith1">times</csymbol><ci>a</ci><ci>b</ci></apply></apply>""" ==> "1/(a*b)"
            MathML.parse """<apply><csymbol cd="arith1">power</csymbol><ci>x</ci><cn>2</cn></apply>""" ==> "x^2"
            MathML.parse """<apply><csymbol cd="arith1">root</csymbol><ci>x</ci><cn>2</cn></apply>""" ==> "sqrt(x)"
        }

        test "Parse MathML Non-Strict Content" {

            MathML.parse """<apply><divide/><cn>1</cn><ci>x</ci></apply>""" ==> "1/x"
            MathML.parse """<apply><divide/><cn>1</cn><apply><times/><ci>a</ci><ci>b</ci></apply></apply>""" ==> "1/(a*b)"
            MathML.parse """<apply><power/><ci>x</ci><cn>2</cn></apply>""" ==> "x^2"
            MathML.parse """<apply><root/><degree><cn>2</cn></degree><ci>x</ci></apply>""" ==> "sqrt(x)"
            MathML.parse """<apply><root/><degree><cn>3</cn></degree><ci>x</ci></apply>""" ==> "x^(1/3)"
        }

        test "Parse F# quotations" {

            Quotations.parse <@ 3 @> ==> "3"
            Quotations.parse <@ x @> ==> "x"
            Quotations.parse <@ fun x -> x @> ==> "x"
            Quotations.parse <@ 3/4 @> ==> "3/4"
            Quotations.parse <@ fun x -> 3/x @> ==> "3/x"
            Quotations.parse <@ -x*y/3 @> ==> "-1/3*x*y"
            Quotations.parse <@ fun x y -> -x*y/3 @> ==> "-1/3*x*y"
            Quotations.parse <@ fun (x, y) -> -x*y/3 @> ==> "-1/3*x*y"
        }

        test "Algebraic Expansion" {

            // Auto-simplification does not expand expressions:
            (a+b)-(a+b) ==> "a + b - (a + b)"
            (a+b)-(a+b) |> Algebraic.expand ==> "0"
            2*(a+b)-(a+b) ==> "a + b"
            (a+b)-2*(a+b) |> Algebraic.expand ===> "(-1)*a + (-1)*b"
            (a+b)-2*(a+b) |> Algebraic.expand ==> "-a - b"

            (a*b)/(b*a) ==> "1"
            (a*b)**2/(b*a) ==> "a*b"
            (a*b)/(b*a)**2 ==> "1/(a*b)"

            (a+b)/(b+a) ==> "1"
            (a+b)**2/(b+a) ==> "a + b"
            (a+b)/(b+a)**2 ==> "1/(a + b)"

            (x*(y+1)**(3Q/2)+1)*(x*(y+1)**(3Q/2)-1) ==> "(-1 + x*(1 + y)^(3/2))*(1 + x*(1 + y)^(3/2))"
            (x*(y+1)**(3Q/2)+1)*(x*(y+1)**(3Q/2)-1) |> Algebraic.expand |> Algebraic.expand ==> "-1 + x^2 + 3*x^2*y + 3*x^2*y^2 + x^2*y^3"
            sin(a*(x+y)) |> Algebraic.expand ==> "sin(a*(x + y))" // does not expand
            a/(b*(x+y)) |> Algebraic.expand ===> "a*b^(-1)*(x + y)^(-1)" // strict; does not expand
            a/(b*(x+y)) |> Algebraic.expand ==> "a/(b*(x + y))" // nice; does not expand
        }

        test "Structural Operators" {

            Structure.substitute 3Q 4Q (x**3) ==> "x^4"
            Structure.map (fun x -> -x) (x + y**2) ==> "-x - y^2"
            Structure.map (fun x -> -x) (x) ==> "x" // this is intended

            Structure.collectIdentifierSymbols (x*cos(y)) --> [ Symbol "x"; Symbol "y" ]
            Structure.collectIdentifiers (x*cos(y)) ==+> [ "x"; "y" ]
            Structure.collectIdentifiers (z/x*cos(y)**x) ==+> [ "x"; "y"; "z" ]

            Structure.collectFunctionTypes (x*cos(y)) --> [ Cos ]
            Structure.collectFunctions (x*cos(y)) ==+> [ "cos(y)" ]

            Structure.collectNumberValues (x*cos(2*y-4)/3) --> [ -4N; 1N/3N; 2N; ]
            Structure.collectNumbers (x*cos(2*y-4)/3) ==+> [ "-4"; "1/3"; "2" ]

            Structure.collect (function | Power _ as p -> Some p | _ -> None) ((x+y**z)**(a+b**c)+d) ==+> [ "(x + y^z)^(a + b^c)" ]
            Structure.collectPredicate (function | Power _ -> true | _ -> false) ((x+y**z)**(a+b**c)+d) ==+> [ "(x + y^z)^(a + b^c)" ]
            Structure.collectAll (function | Power _ as p -> Some p | _ -> None) ((x+y**z)**(a+b**c)+d) ==+> [ "b^c"; "y^z"; "(x + y^z)^(a + b^c)" ]
            Structure.collectAllPredicate (function | Power _ -> true | _ -> false) ((x+y**z)**(a+b**c)+d) ==+> [ "b^c"; "y^z"; "(x + y^z)^(a + b^c)" ]
            Structure.collectPowers ((x+y**z)**(a+b**c)+d) ==+> [ "b^c"; "y^z"; "(x + y^z)^(a + b^c)" ]
            Structure.collectSums ((x+y**z)**(a+b**c)+d) ==+> [ "a + b^c"; "x + y^z"; "d + (x + y^z)^(a + b^c)" ]
        }

        test "Algebaric Operators" {

            negate (x + y**2) ==> "-(x + y^2)"

            Algebraic.factors (b*cos(x)*ln(d)*x) ==+> ["b"; "x"; "ln(d)"; "cos(x)"]
            Algebraic.factors (b*cos(x)*log10(d)*x) ==+> ["b"; "x"; "log(d)"; "cos(x)"]
            Algebraic.factors (b*cos(x)*(log d (d*2))*x) ==+> ["b"; "x"; "log(d,2*d)"; "cos(x)"]
            Algebraic.factors (b+cos(x)) ==+> ["b + cos(x)"]
            Algebraic.summands (b+cos(x)+ln(d)+x) ==+> ["b"; "x"; "ln(d)"; "cos(x)"]
            Algebraic.summands (b+cos(x)+log10(d)+x) ==+> ["b"; "x"; "log(d)"; "cos(x)"]
            Algebraic.summands (b+cos(x)+(log d (d*2))+x) ==+> ["b"; "x"; "log(d,2*d)"; "cos(x)"]
            Algebraic.summands (b*cos(x)) ==+> ["b*cos(x)"]

            Algebraic.factorsInteger (2Q/3*b*cos(x)) --> (2I, [1Q/3; b; cos(x)])

            Algebraic.separateFactors x (b*cos(x)*ln(d)*x) ==|> ("b*ln(d)", "x*cos(x)")
            Algebraic.separateFactors x (b*cos(x)*log10(d)*x) ==|> ("b*log(d)", "x*cos(x)")
            Algebraic.separateFactors x (b*cos(x)*(log d (d*2))*x) ==|> ("b*log(d,2*d)", "x*cos(x)")
            Algebraic.separateFactors x (c*x*sin(x)/2) ==|> ("c/2", "x*sin(x)")

            Algebraic.expand ((x+1)*(x+3)) ==> "3 + 4*x + x^2"
            Algebraic.expand ((a+b)**2) ==> "a^2 + 2*a*b + b^2"
            Algebraic.expand ((a+b)**3) ==> "a^3 + 3*a^2*b + 3*a*b^2 + b^3"
            Algebraic.expand ((a+b)**4) ==> "a^4 + 4*a^3*b + 6*a^2*b^2 + 4*a*b^3 + b^4"
            Algebraic.expand ((a+b+c)**2) ==> "a^2 + 2*a*b + b^2 + 2*a*c + 2*b*c + c^2"

            Algebraic.expandMain (x*(2+(1+x)**2)) ==> "2*x + x*(1 + x)^2"
            Algebraic.expandMain ((x+(1+x)**2)**2) ==> "x^2 + 2*x*(1 + x)^2 + (1 + x)^4"

            Algebraic.expand ((a*x**2 + b*x + c)/(d*x + e)) ==> "c/(e + d*x) + (b*x)/(e + d*x) + (a*x^2)/(e + d*x)"
            let p = Algebraic.expand ((a*x**2 + b*x + c)*(d*x**2 + e*x + f))
            p ==> "c*f + c*e*x + b*f*x + c*d*x^2 + b*e*x^2 + a*f*x^2 + b*d*x^3 + a*e*x^3 + a*d*x^4"
            Polynomial.coefficients x p ==-> [|"c*f"; "c*e + b*f"; "c*d + b*e + a*f"; "b*d + a*e"; "a*d"|]
            Polynomial.leadingCoefficient x p ==> "a*d"
            Polynomial.collectTerms x p ==> "c*f + (c*e + b*f)*x + (c*d + b*e + a*f)*x^2 + (b*d + a*e)*x^3 + a*d*x^4"
            Polynomial.degree x p ==> "4"
            Polynomial.totalDegree p ==> "6"
            Polynomial.variables p ==*> ["a"; "b"; "c"; "d"; "e"; "f"; "x"]

            Exponential.expand (exp(2*x+y)) ==> "(exp(x))^2*exp(y)"
            Exponential.expand (exp(2*a*x + 3*y*z)) ==> "(exp(a*x))^2*(exp(y*z))^3"
            Exponential.expand (exp(2*(x+y))) ==> "(exp(x))^2*(exp(y))^2"
            Exponential.expand (1/(exp(2*x) - (exp(x))**2)) ==> "⧝"
            Exponential.expand (exp((x+y)*(x-y))) ==> "exp(x^2)/exp(y^2)"
            Exponential.expand (ln((c*x)**a) + ln(y**b*z)) ==> "a*ln(c) + a*ln(x) + b*ln(y) + ln(z)"
            Exponential.expand (log10((c*x)**a) + log10(y**b*z)) ==> "a*log(c) + a*log(x) + b*log(y) + log(z)"
            Exponential.expand ((log 5Q ((c*x)**a)) + (log 3Q (y**b*z))) ==> "a*log(5,c) + a*log(5,x) + b*log(3,y) + log(3,z)"

            Exponential.contract (exp(x)*exp(y)) ==> "exp(x + y)"
            Exponential.contract (exp(x)**a) ==> "exp(a*x)"
            Exponential.contract (exp(x)*(exp(x) + exp(y))) ==> "exp(2*x) + exp(x + y)"
            Exponential.contract ((exp(exp(x)))**exp(y)) ==> "exp(exp(x + y))"

            Exponential.simplify (1/(exp(x)*(exp(y)+exp(-x))) - (exp(x+y)-1)/((exp(x+y))**2-1)) ==> "0"

            Trigonometric.expand (sin(2*x)) ==> "2*sin(x)*cos(x)"
            Trigonometric.expand (sin(3*x)) ==> "-(sin(x))^3 + 3*sin(x)*(cos(x))^2"
            Trigonometric.expand (sin(a+x)) ==> "sin(x)*cos(a) + sin(a)*cos(x)"
            Trigonometric.expand (sin(2*x + 3*y)) ==> "(-(sin(x))^2 + (cos(x))^2)*(-(sin(y))^3 + 3*sin(y)*(cos(y))^2) + 2*sin(x)*cos(x)*(-3*(sin(y))^2*cos(y) + (cos(y))^3)"
            Trigonometric.expand (sin(2*(x+y))) ==> "2*sin(y)*(-(sin(x))^2 + (cos(x))^2)*cos(y) + 2*sin(x)*cos(x)*(-(sin(y))^2 + (cos(y))^2)"
            Trigonometric.expand (sin(2*(x+y))) |> Algebraic.expand ==> "-2*sin(x)*(sin(y))^2*cos(x) - 2*(sin(x))^2*sin(y)*cos(y) + 2*sin(y)*(cos(x))^2*cos(y) + 2*sin(x)*cos(x)*(cos(y))^2"
            Trigonometric.expand (cos(5*x)) ==> "5*(sin(x))^4*cos(x) - 10*(sin(x))^2*(cos(x))^3 + (cos(x))^5"
            // TODO: should actually be Undefined
            Trigonometric.expand ((sin(2*x)-2*sin(x)*cos(x))/((sin(x))**2 + (cos(x))**2 - 1)) ==> "0"

            Trigonometric.contract (sin(a)*sin(b)) ==> "-cos(a + b)/2 + cos(a - b)/2"
            Trigonometric.contract (sin(a)*cos(b)) ==> "sin(a + b)/2 + sin(a - b)/2"
            Trigonometric.contract (cos(a)*sin(b)) ==> "sin(a + b)/2 + sin(-a + b)/2"
            Trigonometric.contract (cos(a)*cos(b)) ==> "cos(a + b)/2 + cos(a - b)/2"
            Trigonometric.contract (sin(x)**2 + cos(x)**2) ==> "1" // Pythagorean identity
            Trigonometric.simplify (sec(x)**2 - tan(x)**2 - 1) ==> "0"
            Trigonometric.simplify (csc(x)**2 - cot(x)**2 - 1) ==> "0"
            Trigonometric.contract ((sin(x) + cos(y))*cos(y)) ==> "1/2 + sin(x + y)/2 + sin(x - y)/2 + cos(2*y)/2"
            Trigonometric.contract (sin(x)**2*cos(x)**2) ==> "1/8 - cos(4*x)/8"
            Trigonometric.contract (cos(x)**4) ==> "3/8 + cos(2*x)/2 + cos(4*x)/8"
            Trigonometric.contract (sin(x/2)**2) ==> "1/2 - cos(x)/2"
            Trigonometric.contract (cos(x/2)**2) ==> "1/2 + cos(x)/2"
            Trigonometric.simplify ((cos(x)+sin(x))**4 + (cos(x)-sin(x))**4 + cos(4*x) - 3) ==> "0"

            Trigonometric.substitute (tan(x)) ==> "sin(x)/cos(x)"
            Trigonometric.substitute (cot(x)) ==> "cos(x)/sin(x)"
            Trigonometric.substitute (csc(x)) ==> "1/sin(x)"
            Trigonometric.substitute (sec(x)) ==> "1/cos(x)"

            Trigonometric.expand (sinh(2*x)) ==> "2*sinh(x)*cosh(x)"
            Trigonometric.expand (sinh(3*x)) ==> "(sinh(x))^3 + 3*sinh(x)*(cosh(x))^2"
            Trigonometric.expand (sinh(4*x)) ==> "4*(sinh(x))^3*cosh(x) + 4*sinh(x)*(cosh(x))^3"
            Trigonometric.expand (sinh(5*x)) ==> "(sinh(x))^5 + 10*(sinh(x))^3*(cosh(x))^2 + 5*sinh(x)*(cosh(x))^4"
            Trigonometric.expand (cosh(2*x)) ==> "(sinh(x))^2 + (cosh(x))^2"
            Trigonometric.expand (cosh(3*x)) ==> "3*(sinh(x))^2*cosh(x) + (cosh(x))^3"
            Trigonometric.expand (cosh(4*x)) ==> "(sinh(x))^4 + 6*(sinh(x))^2*(cosh(x))^2 + (cosh(x))^4"
            Trigonometric.expand (cosh(5*x)) ==> "5*(sinh(x))^4*cosh(x) + 10*(sinh(x))^2*(cosh(x))^3 + (cosh(x))^5"

            Trigonometric.contract (sinh(a)*sinh(b)) ==> "cosh(a + b)/2 - cosh(a - b)/2"
            Trigonometric.contract (sinh(a)*cosh(b)) ==> "sinh(a + b)/2 + sinh(a - b)/2"
            Trigonometric.contract (cosh(a)*sinh(b)) ==> "sinh(a + b)/2 + sinh(-a + b)/2"
            Trigonometric.contract (cosh(a)*cosh(b)) ==> "cosh(a + b)/2 + cosh(a - b)/2"
            Trigonometric.contract (sinh(x)**2) ==> "-1/2 + cosh(2*x)/2"
            Trigonometric.contract (sinh(x)**3) ==> "-3/4*sinh(x) + sinh(3*x)/4"
            Trigonometric.contract (sinh(x)**4) ==> "3/8 - cosh(2*x)/2 + cosh(4*x)/8"
            Trigonometric.contract (sinh(x)**5) ==> "5/8*sinh(x) - 5/16*sinh(3*x) + sinh(5*x)/16"
            Trigonometric.contract (cosh(x)**2) ==> "1/2 + cosh(2*x)/2"
            Trigonometric.contract (cosh(x)**3) ==> "3/4*cosh(x) + cosh(3*x)/4"
            Trigonometric.contract (cosh(x)**4) ==> "3/8 + cosh(2*x)/2 + cosh(4*x)/8"
            Trigonometric.contract (cosh(x)**5) ==> "5/8*cosh(x) + 5/16*cosh(3*x) + cosh(5*x)/16"
            Trigonometric.contract (sinh(x/2)**2) ==> "-1/2 + cosh(x)/2"
            Trigonometric.contract (cosh(x/2)**2) ==> "1/2 + cosh(x)/2"
            Trigonometric.contract (cosh(x)**2 - sinh(x)**2) ==> "1"
            Trigonometric.simplify (sech(x)**2 + tanh(x)**2 - 1) ==> "0"
            Trigonometric.simplify (csch(x)**2 - coth(x)**2 + 1) ==> "0"
            Trigonometric.simplify ((cosh(x)+sinh(x))**4 + (cosh(x)-sinh(x))**4 - 2*cosh(4*x)) ==> "0"

            Trigonometric.substitute (tanh(x)) ==> "sinh(x)/cosh(x)"
            Trigonometric.substitute (coth(x)) ==> "cosh(x)/sinh(x)"
            Trigonometric.substitute (csch(x)) ==> "1/sinh(x)"
            Trigonometric.substitute (sech(x)) ==> "1/cosh(x)"

            // TODO: expected: 0
            Trigonometric.simplify (sin(x) + sin(y) - 2*sin(x/2+y/2)*cos(x/2-y/2))
                ==> "sin(y) - sin(x - y)/2 - sin(x/2 - y/2 - (x/2 - y/2))/2 - sin(-x/2 + y/2 - (x/2 - y/2))/2 - sin(x/2 + y/2 - (x/2 - y/2))"
        }

        test "Differentiation and Taylor Series" {

            Calculus.differentiate x (a*x) ==> "a"
            Calculus.differentiate x (sin(x)) ==> "cos(x)"
            Calculus.differentiate x (x*sin(x)) ==> "sin(x) + x*cos(x)"
            Calculus.differentiate x (a*x**2) ==> "2*a*x"
            Calculus.differentiate x (a*x**b) ==> "a*b*x^(-1 + b)"
            Calculus.differentiate x (a*x**2 + b*x + c) ==> "b + 2*a*x"
            Calculus.differentiate x (1Q/x) ==> "-1/x^2"
            Calculus.differentiate x ((ln x) / (ln 10Q)) ==> "1/(x*ln(10))"
            Calculus.differentiate x (ln x) ==> "1/x"
            Calculus.differentiate x (ln (x**2)) ==> "2/x"
            Calculus.differentiate x (log10 x) ==> "1/(x*ln(10))"
            Calculus.differentiate x (log10 (x**2)) ==> "2/(x*ln(10))"
            Calculus.differentiate x (log 10Q x) ==> "1/(x*ln(10))"
            Calculus.differentiate x (log x (x**2)) ==> "2/(x*ln(x)) - ln(x^2)/(x*(ln(x))^2)"

            Calculus.differentiate x (arcsin(x)) ==> "(1 - x^2)^(-1/2)"
            Calculus.differentiate x (arccos(x)) ==> "-(1 - x^2)^(-1/2)"
            Calculus.differentiate x (arctan(x)) ==> "1/(1 + x^2)"

            Calculus.taylor 3 x 0Q (1/(1-x)) ==> "1 + x + x^2"
            Calculus.taylor 3 x 1Q (1/x) ==> "3 - 3*x + x^2"
            Calculus.taylor 3 x 1Q (ln(x)) ==> "-3/2 + 2*x - x^2/2"
            Calculus.taylor 4 x 1Q (ln(x)) ==> "-11/6 + 3*x - 3/2*x^2 + x^3/3"
            Calculus.taylor 3 x 0Q (sin(x)+cos(x)) ==> "1 + x - x^2/2"
            Calculus.taylor 4 x 0Q (sin(x)+cos(x)) ==> "1 + x - x^2/2 - x^3/6"
            (sin(x)+cos(x)) |> Calculus.taylor 4 x 0Q ==> "1 + x - x^2/2 - x^3/6"
        }

        test "Tangent and Normal Lines" {

            (1/z) |> Calculus.tangentLine z 3Q ==> "2/3 - z/9"
            (x**3 - 12*x**2 - c) |> Calculus.tangentLine x 1Q ==> "10 - c - 21*x"

            Calculus.normalLine z 3Q (1/z) ==> "-80/3 + 9*z"
            (1/z) |> Calculus.normalLine z 3Q ==> "-80/3 + 9*z"
        }

        test "Polynomial Division" {

            Polynomial.divide x (5*x**2 + 4*x + 1) (2*x + 3) ==|> ("-7/4 + 5/2*x", "25/4")
            Polynomial.divide x (x**3 - 2*x**2 - 4) (x-3) ==|> ("3 + x + x^2", "5")
            Polynomial.quot x (x**3 - 2*x**2 - 4) (x-3) ==> "3 + x + x^2"
            Polynomial.remainder x (x**3 - 2*x**2 - 4) (x-3) ==> "5"

            Polynomial.divide x (3*x**3 + x**2 + x + 5) (5*x**2 - 3*x + 1) ==|> ("14/25 + 3/5*x", "111/25 + 52/25*x")
            Polynomial.divide x (3*x**3 + x**2 + x + 5) (2Q) ==|> ("5/2 + x/2 + x^2/2 + 3/2*x^3", "0")
            Polynomial.pseudoDivide x (3*x**3 + x**2 + x + 5) (5*x**2 - 3*x + 1) ==||> ("14 + 15*x", "111 + 52*x", "25")
            Polynomial.pseudoDivide x (3*x**3 + x**2 + x + 5) (2Q) ==||> ("5 + x + x^2 + 3*x^3", "0", "2")

            // tangent of polynomial at x = 1?
            Polynomial.divide x (x**3 - 12*x**2 - c) (x**2-2*x+1) ==|> ("-10 + x", "10 - c - 21*x")

            /// Find tangent equation for x(symbol) at symbol=a
            let tangent symbol x a =
                let m = Calculus.differentiate symbol x |> Structure.substitute symbol a
                m*(symbol - a) + Structure.substitute symbol a x |> Algebraic.expand

            tangent x (x**3 - 12*x**2 - c) 1Q ==> "10 - c - 21*x"
            tangent z (1/z) 3Q ==> "2/3 - z/9"
        }

        test "Polynomial Expansion" {

            let ex = Polynomial.polynomialExpansion x y (x**5 + 11*x**4 + 51*x**3 + 124*x**2 + 159*x + 86) (x**2 + 4*x + 5)
            ex ==> "1 + x + (2 + x)*y + (3 + x)*y^2"

            let exs = ex |> Structure.substitute y (x**2 + 4*x + 5)
            exs ==> "1 + x + (2 + x)*(5 + 4*x + x^2) + (3 + x)*(5 + 4*x + x^2)^2"

            // get back to original polynomial:
            Algebraic.expand exs ==> "86 + 159*x + 124*x^2 + 51*x^3 + 11*x^4 + x^5"
        }

        test "Polynomial From Coefficients" {
            Polynomial.fromCoefficients x [1Q; 1Q; 1Q] ==> "1 + x + x^2"
            Polynomial.fromCoefficients x [1Q; 2Q; 3Q] ==> "1 + 2*x + 3*x^2"
            Polynomial.fromCoefficients x [a; b; c] ==> "a + b*x + c*x^2"
            Polynomial.fromCoefficients x [sin(y); cos(y)] ==> "sin(y) + x*cos(y)"
            Polynomial.fromCoefficients x [] ==> "0"
        }

        test "Polynomial Euclidean/GCD" {

            Polynomial.gcd x (x**7 - 4*x**5 - x**2 + 4) (x**5 - 4*x**3 - x**2 + 4) ==> "4 - 4*x - x^2 + x^3"
            Polynomial.gcd x (x**4 - 2*x**3 - 6*x**2 + 12*x + 15) (x**3 + x**2 - 4*x - 4) ==> "1 + x"

            Polynomial.extendedGcd x (x**7 - 4*x**5 - x**2 + 4) (x**5 - 4*x**3 - x**2 + 4)
                ==||> ("4 - 4*x - x^2 + x^3", "-x", "1 + x^3")

            // verify A*u+B*v = gcd = 4 - 4*x - x^2 + x^3:
            (-x)*(x**7 - 4*x**5 - x**2 + 4) + (1+x**3)*(x**5 - 4*x**3 - x**2 + 4) |> Algebraic.expand
                ==> "4 - 4*x - x^2 + x^3"

            let u = x**4 - 2*x**3 - 6*x**2 + 12*x + 15
            let v = x**3 + x**2 - 4*x - 4
            Polynomial.gcd x u v ==> "1 + x"
            Polynomial.halfExtendedGcd x u v ==|> ("1 + x", "3/5 - x/5")
            let g,a,b = Polynomial.extendedGcd x u v
            g ==> "1 + x"
            a ==> "3/5 - x/5"
            b ==> "2 - 6/5*x + x^2/5"

            // hence u*a + v*b = g ? indeed:
            u*a + v*b |> Algebraic.expand ==> "1 + x"

            // Let's try to find s, t such that s*u + t*v = x^2 - 1
            let s, t = Polynomial.diophantineGcd x (x**4 - 2*x**3 - 6*x**2 + 12*x + 15) (x**3 + x**2 - 4*x - 4) (x**2 - 1)
            s ==> "-3/5 + 4/5*x - x^2/5"
            t ==> "-2 + 16/5*x - 7/5*x^2 + x^3/5"
            s*u + t*v |> Algebraic.expand ==> "-1 + x^2"

            // (x^2 + 3*x)/((x + 1)*(x^2 - 2*x + 1)) --> (-1/2)/(x+1) + (1/2 + (3/2)*x)/(x^2-2*x+1)
            let a0, ax = Polynomial.partialFraction x (x**2+3*x) [x+1; x**2-2*x+1]
            a0 ==> "0"
            ax ==+> ["-1/2"; "1/2 + 3/2*x"]
        }

        test "Evaluate some expression to floating point numbers" {

            let symbols = Map.ofList ["a", FloatingPoint.Real 2.0; "b", FloatingPoint.Real 3.0; "c", FloatingPoint.Complex (complex 1.0 -1.0)]
            Evaluate.evaluate symbols (a) --> FloatingPoint.Real 2.0
            Evaluate.evaluate symbols (1Q/2) --> FloatingPoint.Real 0.5
            Evaluate.evaluate symbols (sin(a) + ln(b)) --> FloatingPoint.Real (System.Math.Sin(2.0) + System.Math.Log(3.0))
            Evaluate.evaluate symbols (a*x**2 + b*x + c |> Structure.substitute x (number 1/2)) --> FloatingPoint.Complex (complex 3.0 -1.0)
            Evaluate.evaluate symbols (1Q/0Q) --> FloatingPoint.ComplexInf

            Expect.throws (fun () -> Evaluate.evaluate symbols (f) |> ignore) ""
            //(fun () -> Evaluate.evaluate symbols (f) |> ignore) |> should (throwWithMessage "Failed to find symbol: f") typeof<System.Exception>

            let (FloatingPoint.Complex c) = Evaluate.evaluate symbols (sqrt(-1Q))
            Expect.floatClose Accuracy.veryHigh c.Real 0.0 "Real"
            Expect.floatClose Accuracy.veryHigh c.Imaginary 1.0 "Imag"
        }

        test "Primitive Equation Solver" {

            let solve x expr =

                let expr' = Rational.simplify x expr |> Algebraic.expand

                if Polynomial.isPolynomial x expr' then
                    match Polynomial.coefficients x expr' with
                    | [||] -> Undefined
                    | [| a |] -> x
                    | [| a; b |] -> -a/b
                    | _ -> failwith "higher polynomials not supported"

                else failwith "only general polynomial expressions supported for now"

            // 2+3*x = 0 --> x =
            solve x (2+3*x) ==> "-2/3"

            // sin(a)+x*cos(b)+c = 0 --> x =
            solve x (sin(a)+x*cos(b)+c) ==> "-(c + sin(a))/cos(b)"

            // (x^2-1)/(x+1) = 0 --> x =
            solve x ((x**2-1)/(x+1)) ==> "1"

            /// Solve simple a=b line equations to y=f(x) form
            let solveLine x y a b =
                let z = solve y (a-b) |> Algebraic.expand |> Rational.simplify x
                let z' = z |> Algebraic.expand |> Polynomial.collectTerms x
                if z' <> Undefined then z' else z

            solveLine x y (x/2+y/3) 1Q ==> "3 - 3/2*x" //   -->  x/2 + y/3 = 1  ->  y = -3/2*x + 3
            solveLine x y (x/a) ((x+y)/b) ==> "(-1 + b/a)*x"
            solveLine x y ((y/x-2)/(1-3/x)) 6Q ==> "-18 + 8*x"
        }

        test "General Polynomial Expressions" {

            Polynomial.isMonomial x (a * x**2) --> true
            Polynomial.isMonomial x (ln(a) * x**2) --> true
            Polynomial.isMonomial x (log10(a) * x**2) --> true
            Polynomial.isMonomial x ((log a (a**2)) * x**2) --> true
            Polynomial.isMonomial x (x**2 + a) --> false
            Polynomial.isPolynomial x (x**2 + x**3) --> true
            Polynomial.isPolynomial x (x**2 + 2*x) --> true
            Polynomial.isPolynomial x ((x+1)*(x+3)) --> false

            Polynomial.isMonomialMV (Polynomial.symbols [x;y]) (a * x**2 * y**2) --> true
            Polynomial.isMonomialMV (Polynomial.symbols [x;y]) (ln(a) * x**2 * y**2) --> true
            Polynomial.isMonomialMV (Polynomial.symbols [x;y]) (log10(a) * x**2 * y**2) --> true
            Polynomial.isMonomialMV (Polynomial.symbols [x;y]) ((log a (a**2)) * x**2 * y**2) --> true
            Polynomial.isMonomialMV (Polynomial.symbols [x;y]) (x**2 + y**2) --> false
            Polynomial.isPolynomialMV (Polynomial.symbols [x;y]) (x**2 + y**2) --> true
            Polynomial.isPolynomialMV (Polynomial.symbols [x+1]) ((x+1)**2 + 2*(x+1)) --> true
            Polynomial.isPolynomialMV (Polynomial.symbols [x]) ((x+1)*(x+3)) --> false

            Polynomial.degreeMonomial x (a * x**2 * x * b**2) ==> "3"
            Polynomial.degree x (a*x**2 + b*x + c) ==> "2"

            Polynomial.degreeMonomialMV (Polynomial.symbols [x;y]) (a * x**2 * y * b**2) ==> "3" // (x:2 + y:1)
            Polynomial.degreeMV (Polynomial.symbols [x;y]) (a*x**2 + b*x + c) ==> "2"
            Polynomial.degreeMV (Polynomial.symbols [x;z]) (2*x**2*y**8*z**2 + a*x*z**6) ==> "7"

            Polynomial.variables (a * x**2 * y**2) ==*> ["a"; "x"; "y"]
            Polynomial.variables ((x+1)**2 + 2*(x+1)) ==*> ["1 + x"]
            Polynomial.variables ((x+1)*(x+3)) ==*> ["1 + x"; "3 + x"]
            Polynomial.variables ((x+1)*(x+3)*sin(x)) ==*> ["1 + x"; "3 + x"; "sin(x)"]
            Polynomial.totalDegree (2*x**2*y*z**2 + a*x*z**6) ==> "8"

            Polynomial.commonFactors (8*a*x + 6*a*x**2) ==> "2*a*x"
            Polynomial.commonFactors ((3Q/2)*x*y**2 + (5Q/8)*x**2*y + 7*x + (9Q/10)) ==> "1"
            Polynomial.commonFactors (512*x*y*z + 512*x**2*y*z + 3072*x*y**2*z + 3072*x**2*y**2*z + 1024*x*y**3*z) ==> "512*x*y*z"

            Polynomial.coefficientMonomial x (2*a * b * x**2) ==> "2*a*b"

            Polynomial.coefficientMonomialMV (Polynomial.symbols [x;y]) (2*a * b * x**2) ==> "2*a*b"
            Polynomial.coefficientMonomialMV (Polynomial.symbols [x;y]) (2*a * b * x**2 * y) ==> "2*a*b"
            Polynomial.coefficientMonomialMV (Polynomial.symbols [x;y]) (2*a * b * x**2 * y * z) ==> "2*a*b*z"
            Polynomial.coefficientMonomialMV (Polynomial.symbols [x;y;z;a;b]) (2*a * b * x**2 * y * z) ==> "2"

            Polynomial.coefficient x 2 (a*x**2 + b*x + c) ==> "a"
            Polynomial.coefficient x 2 (a*x*x + b*x + c) ==> "a"
            Polynomial.coefficient x 1 (3*x*y**2 + 5*x**2*y + 7*x + 9) ==> "7 + 3*y^2"
            Polynomial.coefficient x 3 (3*x*y**2 + 5*x**2*y + 7*x + 9) ==> "0"
            Polynomial.leadingCoefficient x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) ==> "5*y + 7*y^3"
            Polynomial.coefficients x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) ==-> [|"9"; "3*y^2"; "5*y + 7*y^3"|]

            Polynomial.collectTermsMonomial x (2*x*a) ==|> ("2*a", "x")
            Polynomial.collectTermsMonomial x (2*a*x*b*3) ==|> ("6*a*b", "x")
            Polynomial.collectTermsMonomial x (2*a*x**3*b*x*3) ==|> ("6*a*b", "x^4")

            Polynomial.collectTermsMonomialMV (Polynomial.symbols [x;y]) (2*x*a) ==|> ("2*a", "x")
            Polynomial.collectTermsMonomialMV (Polynomial.symbols [x;y]) (2*a*x*b*y*3) ==|> ("6*a*b", "x*y")
            Polynomial.collectTermsMonomialMV (Polynomial.symbols [x;y]) (2*a*x*b*y**3*x*3) ==|> ("6*a*b", "x^2*y^3")

            Polynomial.collectTerms x (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) ==> "x*(4*a + 5*b + 2*a*y + 3*b*y)"
            Polynomial.collectTerms a (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) ==> "5*b*x + 3*b*x*y + a*(4*x + 2*x*y)"
            Polynomial.collectTerms (ln(a)) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c) ==> "c + 5*b*x + 3*b*x*y + (4*x + 2*x*y)*ln(a)"
            Polynomial.collectTerms (log10(a)) (2*x*log10(a)*y + 4*x*log10(a) + 3*x*y*b + 5*x*b + c) ==> "c + 5*b*x + 3*b*x*y + (4*x + 2*x*y)*log(a)"
            Polynomial.collectTerms (log a (a**2)) (2*x*(log a (a**2))*y + 4*x*(log a (a**2)) + 3*x*y*b + 5*x*b + c) ==> "c + 5*b*x + 3*b*x*y + (4*x + 2*x*y)*log(a,a^2)"

            Polynomial.collectTermsMV (Polynomial.symbols [x;y]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) ==> "(4*a + 5*b)*x + (2*a + 3*b)*x*y"
            Polynomial.collectTermsMV (Polynomial.symbols [a;b]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) ==> "a*(4*x + 2*x*y) + b*(5*x + 3*x*y)"
            Polynomial.collectTermsMV (Polynomial.symbols [x;ln(a)]) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c) ==> "c + x*(5*b + 3*b*y) + x*(4 + 2*y)*ln(a)"
            Polynomial.collectTermsMV (Polynomial.symbols [x;log10(a)]) (2*x*log10(a)*y + 4*x*log10(a) + 3*x*y*b + 5*x*b + c) ==> "c + x*(5*b + 3*b*y) + x*(4 + 2*y)*log(a)"
            Polynomial.collectTermsMV (Polynomial.symbols [x;(log a (a**2))]) (2*x*(log a (a**2))*y + 4*x*(log a (a**2)) + 3*x*y*b + 5*x*b + c) ==> "c + x*(5*b + 3*b*y) + x*(4 + 2*y)*log(a,a^2)"

            Polynomial.isSquareFree x (x**3 + 1) --> true
            Polynomial.isSquareFree x (x**2 - 2) --> true
            Polynomial.isSquareFree x (8*x**3 + 12*x**2 + 6*x + 1) --> false

            Polynomial.factorSquareFree x (x**8 + 6*x**6 + 12*x**4 + 8*x**2) ==> "x^2*(2 + x^2)^3"

            let sf = Polynomial.factorSquareFree x (x**5 + 6*x**4 + 10*x**3 - 4*x**2 - 24*x - 16)
            sf ==> "(2 + x)^3*(-2 + x^2)"
            Algebraic.expand sf ==> "-16 - 24*x - 4*x^2 + 10*x^3 + 6*x^4 + x^5"
        }

        test "General Rational Expressions" {

            Rational.numerator (x/y) ==> "x"
            Rational.denominator (x/y) ==> "y"
            Rational.numerator (x**2/y**3) ==> "x^2"
            Rational.denominator (x**2/y**3) ==> "y^3"

            Rational.numerator (x**2) ==> "x^2"
            Rational.denominator (x**2) ==> "1"
            Rational.numerator (x**(-2)) ==> "1"
            Rational.denominator (x**(-2)) ==> "x^2"

            Rational.numerator (2Q/3*(x*(x+1))/(x+2)*y**a) ==> "2*x*(1 + x)*y^a"
            Rational.denominator (2Q/3*(x*(x+1))/(x+2)*y**a) ==> "3*(2 + x)"

            Rational.isRational x ((x**2+1)/(2*x+3)) --> true
            Rational.isRational x (1/x + 1/a) --> false

            Rational.variables ((2*x + 3*y)/(z + 4)) ==*> ["x"; "y"; "z"]
            Rational.variables (1/x + 1/y) ==*> ["1/x"; "1/y"]
            Rational.variables (a/x + b/y) ==*> ["a"; "1/x"; "b"; "1/y"]

            Rational.rationalize (a+1) ==> "1 + a"
            Rational.rationalize (a/b + c/d) ==> "(b*c + a*d)/(b*d)"
            Rational.rationalize (1+1/(1+1/x)) ==> "(1 + 2*x)/(1 + x)"
            Rational.rationalize (1/(1+1/x)**(1Q/2) + (1+1/x)**(3Q/2)) ==> "(x^2 + (1 + x)^2)/(x^2*sqrt((1 + x)/x))"
            Rational.rationalize ((1+1/x)**2) ==> "(1 + x)^2/x^2"

            Rational.rationalize (a/b + c/d + e/f) ==> "(b*d*e + (b*c + a*d)*f)/(b*d*f)"
            Rational.expand (a/b + c/d + e/f) ==> "(b*d*e + b*c*f + a*d*f)/(b*d*f)"

            Rational.rationalize (((1/((x+y)**2+1))**(1Q/2)+1)*((1/((x+y)**2+1))**(1Q/2)-1)/(x+1))
                ==> "((-1 + sqrt(1/(1 + (x + y)^2)))*(1 + sqrt(1/(1 + (x + y)^2))))/(1 + x)"
            Rational.expand (((1/((x+y)**2+1))**(1Q/2)+1)*((1/((x+y)**2+1))**(1Q/2)-1)/(x+1))
                ==> "(-x^2 - 2*x*y - y^2)/(1 + x + x^2 + x^3 + 2*x*y + 2*x^2*y + y^2 + x*y^2)"

            Rational.rationalize (1/(1/a + c/(a*b)) + (a*b*c + a*c**2)/(b+c)**2-a) |> Algebraic.expand ==> "0"
            Rational.expand (1/(1/a + c/(a*b)) + (a*b*c + a*c**2)/(b+c)**2-a) ==> "0"

            Rational.rationalize (x/z + y/z**2) ==> "(y*z + x*z^2)/z^3"
            Rational.simplify z (x/z + y/z**2) ==> "(y + x*z)/z^2"

            Rational.simplify x ((x**2-1)/(x+1)) ==> "-1 + x"
            Rational.simplify x ((x+1)/(x**2 - 1 - (x+1)*(x-1))) ==> "⧝"
            Rational.simplify x (1/(1+1/(x+1)) + 2/(x+2))  ==> "(3 + x)/(2 + x)"

            // http://stackoverflow.com/questions/32791138/extracting-common-terms-with-mathnet-symbolics
            let pn = (1Q/8)*x*y*z + (1Q/2)*x*(y**2)*z
            let pd = (1Q/8)*x*y*z + (1Q/8)*(x**2)*y*z + (3Q/4)*x*(y**2)*z + (3Q/4)*(x**2)*(y**2)*z + (1Q/4)*x*(y**3)*z
            Rational.expand (pn / pd) ==> "(1 + 4*y)/(1 + x + 6*y + 6*x*y + 2*y^2)"
        }

        test "Single Variable Polynomials" {

            SingleVariablePolynomial.isMonomialSV x (Quotations.parse <@ fun x -> 3*x @>) --> true
            SingleVariablePolynomial.isMonomialSV x (Quotations.parse <@ 3*x+2 @>) --> false
            SingleVariablePolynomial.isMonomialSV x (3*(x*x)) --> true
            SingleVariablePolynomial.isMonomialSV x (a*x) --> false
            SingleVariablePolynomial.isMonomialSV y (3*x) --> false
            SingleVariablePolynomial.degreeMonomialSV x 0Q ==> "-∞"
            SingleVariablePolynomial.degreeMonomialSV x 1Q ==> "0"
            SingleVariablePolynomial.degreeMonomialSV x (3*x) ==> "1"
            SingleVariablePolynomial.degreeMonomialSV x (3 * x*x) ==> "2"
            SingleVariablePolynomial.degreeMonomialSV x (3 * x*x * y) ==> "Undefined"
            SingleVariablePolynomial.degreeMonomialSV x (3 + x) ==> "Undefined"

            SingleVariablePolynomial.coefficientMonomialSV x 0Q ==> "0"
            SingleVariablePolynomial.coefficientMonomialSV x 1Q ==> "1"
            SingleVariablePolynomial.coefficientMonomialSV x (3 * x) ==> "3"
            SingleVariablePolynomial.coefficientMonomialSV x (3 * x*x) ==> "3"
            SingleVariablePolynomial.coefficientMonomialSV x (3 * x*x * y) ==> "Undefined"
            SingleVariablePolynomial.coefficientMonomialSV x (3 + x) ==> "Undefined"
            SingleVariablePolynomial.coefficientDegreeMonomialSV x 0Q ==|> ("0", "-∞")
            SingleVariablePolynomial.coefficientDegreeMonomialSV x 1Q ==|> ("1", "0")
            SingleVariablePolynomial.coefficientDegreeMonomialSV x (3*x) ==|> ("3", "1")
            SingleVariablePolynomial.coefficientDegreeMonomialSV x (3*x*x) ==|> ("3", "2")

            SingleVariablePolynomial.isPolynomialSV x (3*x) --> true
            SingleVariablePolynomial.isPolynomialSV x (3*x+2) --> true
            SingleVariablePolynomial.isPolynomialSV x (3*x*x+2) --> true
            SingleVariablePolynomial.degreeSV x (3*x*x + 2*x) ==> "2"
            SingleVariablePolynomial.degreeSV x (3*x*x + 2*x*x*x) ==> "3"
            SingleVariablePolynomial.degreeSV x (3*x + 2*x*(x**5) + 2*(x**3)) ==> "6"

            SingleVariablePolynomial.coefficientSV x 0 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==> "1"
            SingleVariablePolynomial.coefficientSV x 1 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==> "4"
            SingleVariablePolynomial.coefficientSV x 2 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==> "0"
            SingleVariablePolynomial.coefficientSV x 3 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==> "2"
            SingleVariablePolynomial.coefficientSV x 4 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==> "0"
            SingleVariablePolynomial.coefficientSV x 5 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==> "0"
            SingleVariablePolynomial.coefficientSV x 6 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==> "2"
            SingleVariablePolynomial.coefficientSV x 7 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==> "0"
            SingleVariablePolynomial.leadingCoefficientSV x (3*x*x + 2*x) ==> "3"
            SingleVariablePolynomial.leadingCoefficientSV x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==> "2"
            SingleVariablePolynomial.leadingCoefficientSV x 2Q ==> "2"
            SingleVariablePolynomial.leadingCoefficientSV x 0Q ==> "0"
            SingleVariablePolynomial.leadingCoefficientDegreeSV x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==|> ("2", "6")
            SingleVariablePolynomial.coefficientsSV x (3*x*x + 2*x)  ==-> [|"0"; "2"; "3"|]
            SingleVariablePolynomial.coefficientsSV x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1) ==-> [|"1"; "4"; "0"; "2"; "0"; "0"; "2"|]
        }

        test "Pseudo Function Test" {

            Infix.parseOrUndefined "sqrt(x)" ===> "x^(1/2)"
            Infix.parseOrUndefined "sqrt(x)" ==> "sqrt(x)"
            Infix.parseOrUndefined "pow(x,3)" ==> "x^3"
            Infix.parseOrUndefined "pow(3*x,10*sin(x))" ==> "(3*x)^(10*sin(x))"
            Infix.parseOrUndefined "sqrt(pow(x,1/2))" ===> "(x^(1/2))^(1/2)"
            Infix.parseOrUndefined "sqrt(pow(x,1/2))" ==> "sqrt(sqrt(x))"
        }

        test "Underscores in names" {
            let expr = Infix.parseOrUndefined "(TESTING_UNDER)*(2)"
            expr ==> "2*TESTING_UNDER"
            LaTeX.format expr --> """2{TESTING_{UNDER}}"""

            let expr2 = Infix.parseOrUndefined "(TESTING_UNDER_second)*(2)"
            expr2 ==> "2*TESTING_UNDER_second"
            LaTeX.format expr2 --> """2{TESTING_{UNDER_{second}}}"""
        }

        test "Test for other trigonometric function" {

            let exrp = Infix.parseOrUndefined "tan(x)*25*csc(x)"
            exrp ==> "25*tan(x)*csc(x)"

            let expr2 = Operators.sec 32Q
            expr2 ==> "sec(32)"

            let exrp3 = Expression.Apply(Function.Cot, expr2)
            exrp3 ==> "cot(sec(32))"

            let expr4 = Infix.parseOrUndefined "25*x*sec(x)"
            Calculus.differentiate x expr4  ==> "25*(sec(x) + x*tan(x)*sec(x))"

            let expr5 = Infix.parseOrUndefined "sinh(asinh(j)) + cosh(acosh(j)) + tanh(atanh(j)) + csch(acsch(j)) + sech(asech(j)) + coth(acoth(j))"
            expr5 ==> "sinh(asinh(j)) + cosh(acosh(j)) + tanh(atanh(j)) + csch(acsch(j)) + sech(asech(j)) + coth(acoth(j))" // "6*j"
        }

        test "Exponential notation parsing" {
            let expr = Infix.parseOrUndefined "(-6.40869140625E-05)*x"
            expr ==> "(-6.40869140625E-05)*x"

            let expr2 = Infix.parseOrUndefined "1.5e7"
            expr2 ==> "15000000"

            let expr3 = Infix.parseOrUndefined "-.5e7"
            expr3 ==> "-5000000"

            let expr4 = Infix.parseOrUndefined "58E-3"
            expr4 ==> "0.058"
        }

        test "Expression to delegate compilation" {

            let symX = Symbol "x"
            let symY = Symbol "y"

            let toComplex f = System.Numerics.Complex.Create(f, 0.0)

            let expr1 = x
            (Compile.compileExpression1OrThrow expr1 symX).Invoke(3.0) --> 3.0

            let expr2 = x*x
            (Compile.compileExpression1OrThrow expr2 symX).Invoke(3.0) --> 9.0

            let expr3 = x + y
            (Compile.compileExpression2OrThrow expr3 symX symY).Invoke(3.0, 3.0) --> 6.0

            let expr4 = ln x
            (Compile.compileExpression1OrThrow expr4 symX).Invoke(3.0) --> System.Math.Log(3.0)

            let expr5 = (Constant E) ** x
            (Compile.compileExpression1OrThrow expr5 symX).Invoke(3.0) --> System.Math.Exp(3.0)

            let expr6 = sqrt x
            (Compile.compileExpression1OrThrow expr6 symX).Invoke(12.5) --> System.Math.Sqrt(12.5)

            let expr7 = x ** y
            (Compile.compileExpression2OrThrow expr7 symX symY).Invoke(12.5, 5.7) --> System.Math.Pow(12.5, 5.7)

            let expr8 = abs x
            (Compile.compileExpression1OrThrow expr8 symX).Invoke(-14.0) --> 14.0

            let expr9 = x + 1
            (Compile.compileExpression1OrThrow expr9 symX).Invoke(1.0) --> 2.0

            let expr1' = x
            (Compile.compileComplexExpression1OrThrow expr1' symX).Invoke(toComplex 3.0) --> toComplex 3.0

            let expr2' = x*x
            (Compile.compileComplexExpression1OrThrow expr2' symX).Invoke(toComplex 3.0) --> toComplex 9.0

            let expr3' = x + y
            (Compile.compileComplexExpression2OrThrow expr3' symX symY).Invoke(toComplex 3.0, toComplex 3.0) --> toComplex 6.0

            let expr4' = ln x
            (Compile.compileComplexExpression1OrThrow expr4' symX).Invoke(toComplex 3.0) --> System.Numerics.Complex.Log(toComplex 3.0)

            let expr5' = (Constant E) ** x
            (Compile.compileComplexExpression1OrThrow expr5' symX).Invoke(toComplex 3.0) --> System.Numerics.Complex.Exp(toComplex 3.0)

            let expr6' = sqrt x
            (Compile.compileComplexExpression1OrThrow expr6' symX).Invoke(toComplex 12.5) --> System.Numerics.Complex.Sqrt(toComplex 12.5)

            let expr7' = x ** y
            (Compile.compileComplexExpression2OrThrow expr7' symX symY).Invoke(toComplex 12.5, toComplex 5.7) --> System.Numerics.Complex.Pow(toComplex 12.5, toComplex 5.7)

            let expr8' = abs x
            (Compile.compileComplexExpression1OrThrow expr8' symX).Invoke(System.Numerics.Complex.Create(-14.0, 0.0)) --> toComplex 14.0

            let expr9' = x + 1
            (Compile.compileComplexExpression1OrThrow expr9' symX).Invoke(System.Numerics.Complex.One) --> toComplex 2.0
        }

        test "Special values of elementary functions" {

            // Operators.exp
            exp(x + y - (x + y)) ==> "exp(x + y - (x + y))" // "1"

            exp(-1Q) ==> "exp(-1)" // "1/e"
            exp(1Q/2Q*pi*Constant I) ==> "exp(1/2*π*j)" // "j"
            exp(2Q/2Q*pi*Constant I) ==> "exp(π*j)" // "-1"
            exp(3Q/2Q*pi*Constant I) ==> "exp(3/2*π*j)" // "-j"
            exp(4Q/2Q*pi*Constant I) ==> "exp(2*π*j)" // "1"
            exp(-3Q/2Q*pi*Constant I) ==> "exp(-3/2*π*j)" // "j"

            exp(ln(x)) ==> "exp(ln(x))" // "x"
            exp(ln(1Q/Constant E)) ==> "exp(ln(1/e))" // "1/e"
            exp(ln(1Q/x)) ==> "exp(ln(1/x))" // "1/x"
            exp(ln(2Q/3Q)) ==> "exp(ln(2/3))" // "2/3"
            exp(ln(Constant I)) ==> "exp(ln(j))" // "j"

            // Operators.ln
            ln(1Q/x) ==> "ln(1/x)"
            ln(1Q/2Q) ==> "ln(1/2)" // "-ln(2)"
            ln(1Q/Constant E) ==> "ln(1/e)" // "-1"

            ln(Constant I) ==> "ln(j)" // "1/2*π*j"
            ln(2Q*Constant I) ==> "ln(2*j)" // "1/2*π*j + ln(2)"
            ln(-2Q*Constant I) ==> "ln(-2*j)" // "-1/2*π*j + ln(2)"
            ln(-1Q/3Q*Constant I) ==> "ln(-j/3)" // "-1/2*π*j - ln(3)"

            ln(exp(x)) ==> "ln(exp(x))"
            ln(exp(5Q)) ==> "ln(exp(5))" // "5"


            // Operators.log10
            log10(10Q) ==> "1"
            log10(1Q/10Q) ==> "log(1/10)" // "-1"

            // Operators.sin
            sin(-2Q) ==> "-sin(2)"
            sin(-pi) ==> "-sin(π)" // "0"
            sin(0Q) ==> "0"
            sin(pi/6Q) ==> "sin(π/6)" // "1/2"
            sin(5Q/7Q*pi) ==> "sin(5/7*π)"
            sin(Constant I) ==> "sin(j)" // "j*sinh(1)"

            sin(arcsin(x)) ==> "sin(asin(x))" // "x"
            sin(arccos(x)) ==> "sin(acos(x))" // "sqrt(1 - x^2)"
            sin(arctan(x)) ==> "sin(atan(x))" // "x/sqrt(1 + x^2)"
            sin(arccsc(x)) ==> "sin(acsc(x))" // "1/x"
            sin(arcsec(x)) ==> "sin(asec(x))" // "sqrt(1 - 1/x^2)"
            sin(arccot(x)) ==> "sin(acot(x))" // "1/(sqrt(1 + 1/x^2)*x)"

            sin(arcsin(4Q*pi + x)) ==> "sin(asin(4*π + x))" // "4*π + x"
            sin(arcsin(4Q*Constant I)) ==> "sin(asin(4*j))" // "4*j"

            Calculus.differentiate x (x*sin(x)) ==> "sin(x) + x*cos(x)"

            // Operators.cos
            cos(-2Q) ==> "cos(2)"
            cos(-pi) ==> "cos(π)" // "-1"
            cos(0Q) ==> "1"
            cos(pi/6Q) ==> "cos(π/6)" // "2/sqrt(3)"
            cos(5Q/7Q*pi) ==> "cos(5/7*π)"
            cos(Constant I) ==> "cos(j)" // "cosh(1)"

            cos(arcsin(x)) ==> "cos(asin(x))" // "sqrt(1 - x^2)"
            cos(arccos(x)) ==> "cos(acos(x))" // "x"
            cos(arctan(x)) ==> "cos(atan(x))" // "(1 + x^2)^(-1/2)"
            cos(arccsc(x)) ==> "cos(acsc(x))" // "sqrt(1 - 1/x^2)"
            cos(arcsec(x)) ==> "cos(asec(x))" // "1/x"
            cos(arccot(x)) ==> "cos(acot(x))" // "(1 + 1/x^2)^(-1/2)"

            Calculus.differentiate x (x*cos(x)) ==> "-x*sin(x) + cos(x)"

            // Operators.tan
            tan(-2Q) ==> "-tan(2)"
            tan(-pi) ==> "-tan(π)" // "0"
            tan(0Q) ==> "0"
            tan(pi/6Q) ==> "tan(π/6)" // "1/sqrt(3)"
            tan(5Q/7Q*pi) ==> "tan(5/7*π)"
            tan(Constant I) ==> "tan(j)" // "j*tanh(1)"

            tan(arcsin(x)) ==> "tan(asin(x))" // "x/sqrt(1 - x^2)"
            tan(arccos(x)) ==> "tan(acos(x))" // "sqrt(1 - x^2)/x"
            tan(arctan(x)) ==> "tan(atan(x))" // "x"
            tan(arccsc(x)) ==> "tan(acsc(x))" // "1/(sqrt(1 - 1/x^2)*x)"
            tan(arcsec(x)) ==> "tan(asec(x))" // "sqrt(1 - 1/x^2)*x"
            tan(arccot(x)) ==> "tan(acot(x))" // "1/x"

            Calculus.differentiate x (x*tan(x)) ==> "(2*x)/(1 + cos(2*x)) + tan(x)" // "tan(x) + x*sec(x)^2"

            // Operators.csc
            csc(-2Q) ==> "csc(-2)" // "-csc(2)"
            csc(-pi) ==> "csc(-π)" // "-csc(π)" // "⧝"
            csc(0Q) ==> "csc(0)" // "⧝"
            csc(pi/6Q) ==> "csc(π/6)" // "2"
            csc(5Q/7Q*pi) ==> "csc(5/7*π)"
            csc(Constant I) ==> "csc(j)" // "-j*csch(1)"

            csc(arcsin(x)) ==> "csc(asin(x))" // "1/x"
            csc(arccos(x)) ==> "csc(acos(x))" // "(1 - x^2)^(-1/2)"
            csc(arctan(x)) ==> "csc(atan(x))" // "sqrt(1 + x^2)/x"
            csc(arccsc(x)) ==> "csc(acsc(x))" // "x"
            csc(arcsec(x)) ==> "csc(asec(x))" // "(1 - 1/x^2)^(-1/2)"
            csc(arccot(x)) ==> "csc(acot(x))" // "sqrt(1 + 1/x^2)*x"

            Calculus.differentiate x (x*csc(x)) ==> "csc(x) - x*csc(x)*cot(x)"

            // Operators.sec
            sec(-2Q) ==> "sec(-2)" // "sec(2)"
            sec(-pi) ==> "sec(-π)" // "sec(π)" // "-1"
            sec(0Q) ==> "sec(0)" // "1"
            sec(pi/6Q) ==> "sec(π/6)" // "2/sqrt(3)"
            sec(5Q/7Q*pi) ==> "sec(5/7*π)"
            sec(Constant I) ==> "sec(j)" // "sech(1)"

            sec(arcsin(x)) ==> "sec(asin(x))" // "(1 - x^2)^(-1/2)"
            sec(arccos(x)) ==> "sec(acos(x))" // "1/x"
            sec(arctan(x)) ==> "sec(atan(x))" // "sqrt(1 + x^2)"
            sec(arccsc(x)) ==> "sec(acsc(x))" // "(1 - 1/x^2)^(-1/2)"
            sec(arcsec(x)) ==> "sec(asec(x))" // "x"
            sec(arccot(x)) ==> "sec(acot(x))" // "sqrt(1 + 1/x^2)"

            Calculus.differentiate x (x*sec(x)) ==> "sec(x) + x*tan(x)*sec(x)"

            // Operators.cot
            cot(-2Q) ==> "cot(-2)" // "-cot(2)"
            cot(-pi) ==> "cot(-π)" // "-cot(π)" // "⧝"
            cot(0Q) ==> "cot(0)" // "⧝"
            cot(pi/6Q) ==> "cot(π/6)" // "1/sqrt(3)"
            cot(5Q/7Q*pi) ==> "cot(5/7*π)"
            cot(Constant I) ==> "cot(j)" // "-j*coth(1)"

            cot(arcsin(x)) ==> "cot(asin(x))" // "sqrt(1 - x^2)/x"
            cot(arccos(x)) ==> "cot(acos(x))" // "x/sqrt(1 - x^2)"
            cot(arctan(x)) ==> "cot(atan(x))" // "1/x"
            cot(arccsc(x)) ==> "cot(acsc(x))" // "sqrt(1 - 1/x^2)*x"
            cot(arcsec(x)) ==> "cot(asec(x))" // "1/(sqrt(1 - 1/x^2)*x)"
            cot(arccot(x)) ==> "cot(acot(x))" // "x"

            cot(arcsec(-2Q)) ==> "cot(asec(-2))" // "-1/(2*sqrt(3/4))" // "-3^(-1/2)" or "-1/sqrt(3)"

            Calculus.differentiate x (x*cot(x)) ==> "-x/(sin(x))^2 + cot(x)" // "cot(x) - x*(csc(x))^2"

            // Operators.sinh
            sinh(0Q) ==> "sinh(0)" // "0"
            sinh(-2Q) ==> "sinh(-2)" // "-sinh(2)"
            sinh(Constant I) ==> "sinh(j)" // "j*sin(1)"
            sinh(-pi*Constant I) ==> "sinh(-π*j)" // "-sinh(π*j)" // "0"
            sinh(-1Q/6Q*pi*Constant I) ==> "sinh(-1/6*π*j)" // "-sinh(1/6*π*j)" // "1/2*j"
            sinh(5Q/7Q*pi*Constant I) ==> "sinh(5/7*π*j)"
            sinh(3Q/2Q*pi*Constant I) ==> "sinh(3/2*π*j)" // "-j"

            sinh(arcsinh(x)) ==> "sinh(asinh(x))" // "x"
            sinh(arccosh(x)) ==> "sinh(acosh(x))" // "sqrt((-1 + x)/(1 + x))*(1 + x)"
            sinh(arctanh(x)) ==> "sinh(atanh(x))" // "x/sqrt(1 - x^2)"
            sinh(arccsch(x)) ==> "sinh(acsch(x))" // "1/x"
            sinh(arcsech(x)) ==> "sinh(asech(x))" // "((1 + x)*sqrt((1 - x)/(1 + x)))/x"
            sinh(arccoth(x)) ==> "sinh(acoth(x))" // "1/(sqrt(1 - 1/x^2)*x)"

            Calculus.differentiate x (x*sinh(x)) ==> "sinh(x) + x*cosh(x)"

            // Operators.cosh
            cosh(0Q) ==> "cosh(0)" // "1"
            cosh(-2Q) ==> "cosh(-2)" // "cosh(2)"
            cosh(Constant I) ==> "cosh(j)" // "cos(1)"
            cosh(-pi*Constant I) ==> "cosh(-π*j)"  // "cosh(π*j)" // "-1"
            cosh(-1Q/6Q*pi*Constant I) ==> "cosh(-1/6*π*j)" // "cosh(1/6*π*j)" // "sqrt(3)/2"
            cosh(5Q/7Q*pi*Constant I) ==> "cosh(5/7*π*j)"
            cosh(3Q/2Q*pi*Constant I) ==> "cosh(3/2*π*j)" // "0"

            cosh(arcsinh(x)) ==> "cosh(asinh(x))" // "sqrt(1 + x^2)"
            cosh(arccosh(x)) ==> "cosh(acosh(x))" // "x"
            cosh(arctanh(x)) ==> "cosh(atanh(x))" // "(1 - x^2)^(-1/2)"
            cosh(arccsch(x)) ==> "cosh(acsch(x))" // "sqrt(1 + 1/x^2)"
            cosh(arcsech(x)) ==> "cosh(asech(x))" // "1/x"
            cosh(arccoth(x)) ==> "cosh(acoth(x))" // "(1 - 1/x^2)^(-1/2)"

            Calculus.differentiate x (x*cosh(x)) ==> "x*sinh(x) + cosh(x)"

            // Operators.tanh
            tanh(0Q) ==> "tanh(0)" // "0"
            tanh(-2Q) ==> "tanh(-2)" // "-tanh(2)"
            tanh(Constant I) ==> "tanh(j)" // "j*tan(1)"
            tanh(-pi*Constant I) ==> "tanh(-π*j)" // "-tanh(π*j)" // "0"
            tanh(-1Q/6Q*pi*Constant I) ==> "tanh(-1/6*π*j)" // "-tanh(1/6*π*j)" // "-sqrt(3)/2"
            tanh(5Q/7Q*pi*Constant I) ==> "tanh(5/7*π*j)" // "-tan(2/7*π)*j"
            tanh(3Q/2Q*pi*Constant I) ==> "tanh(3/2*π*j)" // "⧝"

            tanh(arcsinh(x)) ==> "tanh(asinh(x))" // "x/sqrt(1 + x^2)"
            tanh(arccosh(x)) ==> "tanh(acosh(x))" // "(sqrt((-1 + x)/(1 + x))*(1 + x))/x"
            tanh(arctanh(x)) ==> "tanh(atanh(x))" // "x"
            tanh(arccsch(x)) ==> "tanh(acsch(x))" // "1/(sqrt(1 + 1/x^2)*x)"
            tanh(arcsech(x)) ==> "tanh(asech(x))" // "(1 + x)*sqrt((1 - x)/(1 + x))"
            tanh(arccoth(x)) ==> "tanh(acoth(x))" // "1/x"

            Calculus.differentiate x (x*tanh(x)) ==> "(2*x)/(1 + cosh(2*x)) + tanh(x)" // "tanh(x) + x*(sech(x))^2"

            // Operators.csch
            csch(0Q) ==> "csch(0)" // "⧝"
            csch(-2Q) ==> "csch(-2)" // "-csch(2)"
            csch(Constant I) ==> "csch(j)" // "-j*csc(1)"
            csch(-pi*Constant I) ==> "csch(-π*j)" // "-csch(π*j)" // "⧝"
            csch(-1Q/6Q*pi*Constant I) ==> "csch(-1/6*π*j)" // "-csch(1/6*π*j)" // "2*j"
            csch(5Q/7Q*pi*Constant I) ==> "csch(5/7*π*j)" // "-csc(2/7*π)*j"
            csch(3Q/2Q*pi*Constant I) ==> "csch(3/2*π*j)" // "j"

            csch(arcsinh(x)) ==> "csch(asinh(x))" // "1/x"
            csch(arccosh(x)) ==> "csch(acosh(x))" // "1/(sqrt((-1 + x)/(1 + x))*(1 + x))"
            csch(arctanh(x)) ==> "csch(atanh(x))" // "sqrt(1 - x^2)/x"
            csch(arccsch(x)) ==> "csch(acsch(x))" // "x"
            csch(arcsech(x)) ==> "csch(asech(x))" // "x/((1 + x)*sqrt((1 - x)/(1 + x)))"
            csch(arccoth(x)) ==> "csch(acoth(x))" // "sqrt(1 - 1/x^2)*x"

            Calculus.differentiate x (x*csch(x)) ==> "csch(x) - x*csch(x)*coth(x)"

            // Operators.sech
            sech(0Q) ==> "sech(0)" // "1"
            sech(-2Q) ==> "sech(-2)" // "sech(2)"
            sech(Constant I) ==> "sech(j)" // "sec(1)"
            sech(-pi*Constant I) ==> "sech(-π*j)" // "sech(π*j)" // "-1"
            sech(-1Q/6Q*pi*Constant I) ==> "sech(-1/6*π*j)" // "sech(1/6*π*j)" // "2/sqrt(3)"
            sech(5Q/7Q*pi*Constant I) ==> "sech(5/7*π*j)" // "-1/sec(2/7*π)"
            sech(3Q/2Q*pi*Constant I) ==> "sech(3/2*π*j)" // "⧝"

            sech(arcsinh(x)) ==> "sech(asinh(x))" // "(1 + x^2)^(-1/2)"
            sech(arccosh(x)) ==> "sech(acosh(x))" // "1/x"
            sech(arctanh(x)) ==> "sech(atanh(x))" // "sqrt(1 - x^2)"
            sech(arccsch(x)) ==> "sech(acsch(x))" // "(1 + 1/x^2)^(-1/2)"
            sech(arcsech(x)) ==> "sech(asech(x))" // "x"
            sech(arccoth(x)) ==> "sech(acoth(x))" // "sqrt(1 - 1/x^2)"

            Calculus.differentiate x (x*sech(x)) ==> "sech(x) - x*tanh(x)*sech(x)"

            // Operators.coth
            coth(0Q) ==> "coth(0)" // "⧝"
            coth(-2Q) ==> "coth(-2)" // "-coth(2)"
            coth(Constant I) ==> "coth(j)" // "-j*cot(1)"
            coth(-pi*Constant I) ==> "coth(-π*j)" // "-coth(π*j)" // "0"
            coth(-1Q/6Q*pi*Constant I) ==> "coth(-1/6*π*j)" // "-coth(1/6*π*j)" // "-sqrt(3)*j"
            coth(5Q/7Q*pi*Constant I) ==> "coth(5/7*π*j)" // "cot(2/7*π)*j"
            coth(3Q/2Q*pi*Constant I) ==> "coth(3/2*π*j)" // "0"

            coth(arcsinh(x)) ==> "coth(asinh(x))" // "sqrt(1 + x^2)/x"
            coth(arccosh(x)) ==> "coth(acosh(x))" // "x/(sqrt((-1 + x)/(1 + x))*(1 + x))"
            coth(arctanh(x)) ==> "coth(atanh(x))" // "1/x"
            coth(arccsch(x)) ==> "coth(acsch(x))" // "sqrt(1 + 1/x^2)*x"
            coth(arcsech(x)) ==> "coth(asech(x))" // "1/((1 + x)*sqrt((1 - x)/(1 + x)))"
            coth(arccoth(x)) ==> "coth(acoth(x))" // "x"

            Calculus.differentiate x (x*coth(x)) ==> "-(2*x)/(-1 + cosh(2*x)) + coth(x)" // "coth(x) - x*(csch(x))^2"

            // Operators.arcsin
            arcsin(-1Q) ==> "asin(-1)" // "-π/2"
            arcsin(-1Q/3Q) ==> "asin(-1/3)" // "-asin(1/3)" // "-asin(1/3)"
            arcsin(0Q) ==> "asin(0)" // "0"
            arcsin(1Q/2Q) ==> "asin(1/2)" // "π/6"
            arcsin(1Q) ==> "asin(1)" // "π/2"

            Calculus.differentiate x (x*arcsin(x)) ==> "x/sqrt(1 - x^2) + asin(x)"

            // Operators.arccos
            arccos(-1Q) ==> "acos(-1)" // "π"
            arccos(-1Q/3Q) ==> "acos(-1/3)"
            arccos(0Q) ==> "acos(0)" // "π/2"
            arccos(1Q/2Q) ==> "acos(1/2)" // "acos(1/2)" // "π/3"
            arccos(1Q) ==> "acos(1)" // "0"

            Calculus.differentiate x (x*arccos(x)) ==> "-x/sqrt(1 - x^2) + acos(x)"

            // Operators.arctan
            arctan(-1Q) ==> "atan(-1)" // "-atan(1)" // "-π/4"
            arctan(-1Q/3Q) ==> "atan(-1/3)" // "-atan(1/3)"
            arctan(0Q) ==> "atan(0)" // "0"
            arctan(1Q/2Q) ==> "atan(1/2)"
            arctan(1Q) ==> "atan(1)" // "π/4"

            Calculus.differentiate x (x*arctan(x)) ==> "x/(1 + x^2) + atan(x)"

            // Operators.arccsc
            arccsc(-1Q) ==> "acsc(-1)" // "-π/2"
            arccsc(-1Q/3Q) ==> "acsc(-1/3)" // "-acsc(1/3)"
            arccsc(0Q) ==> "acsc(0)" // "⧝"
            arccsc(1Q/2Q) ==> "acsc(1/2)" // "csc(1/2)"
            arccsc(1Q) ==> "acsc(1)" // "π/2"

            Calculus.differentiate x (x*arccsc(x)) ==> "-1/(sqrt(1 - 1/x^2)*x) + acsc(x)"

            // Operators.arcsec
            arcsec(-1Q) ==> "asec(-1)" // "π"
            arcsec(-1Q/3Q) ==> "asec(-1/3)"
            arcsec(0Q) ==> "asec(0)" // "⧝"
            arcsec(1Q/2Q) ==> "asec(1/2)"
            arcsec(1Q) ==> "asec(1)" // "0"

            Calculus.differentiate x (x*arcsec(x)) ==> "1/(sqrt(1 - 1/x^2)*x) + asec(x)"

            // Operators.arccot
            arccot(-1Q) ==> "acot(-1)" // "-π/4"
            arccot(-1Q/3Q) ==> "acot(-1/3)" // "-acot(1/3)"
            arccot(0Q) ==> "acot(0)" // "π/2"
            arccot(1Q/2Q) ==> "acot(1/2)"
            arccot(1Q) ==> "acot(1)" // "π/4"

            Calculus.differentiate x (x*arccot(x)) ==> "-x/(1 + x^2) + acot(x)"

            // Operators.arcsinh
            arcsinh(0Q) ==> "asinh(0)" // "0"
            arcsinh(-Constant I) ==> "asinh(-j)" // "-asinh(j)" // "-1/2*π*j"
            arcsinh(-1Q/2Q*Constant I) ==> "asinh(-j/2)" // "-asinh(j/2)" // "1/2*j"
            arcsinh(Constant I) ==> "asinh(j)" // "1/2*π*j"
            arcsinh(2Q*Constant I) ==> "asinh(2*j)" // "asin(2)*j"

            Calculus.differentiate x (x*arcsinh(x)) ==> "x/sqrt(1 + x^2) + asinh(x)"

            // Operators.arccosh
            arccosh(0Q) ==> "acosh(0)" // "1/2*π*j"
            arccosh(-Constant I) ==> "acosh(-j)"
            arccosh(-1Q/2Q*Constant I) ==> "acosh(-j/2)"
            arccosh(Constant I) ==> "acosh(j)"
            arccosh(2Q*Constant I) ==> "acosh(2*j)"

            Calculus.differentiate x (x*arccosh(x)) ==> "x/(sqrt(-1 + x)*sqrt(1 + x)) + acosh(x)"

            // Operators.arctanh
            arctanh(0Q) ==> "atanh(0)" // "0"
            arctanh(-Constant I) ==> "atanh(-j)" // "-atanh(j)" // "-1/4*π*j"
            arctanh(-1Q/2Q*Constant I) ==> "atanh(-j/2)" // "-atanh(j/2)" // "-atan(1/2)*j"
            arctanh(Constant I) ==> "atanh(j)" // "1/4*π*j"
            arctanh(2Q*Constant I) ==> "atanh(2*j)" // "atan(2)*j"

            Calculus.differentiate x (x*arctanh(x)) ==> "x/(1 - x^2) + atanh(x)"

            // Operators.arccsch
            arccsch(0Q) ==> "acsch(0)" // "⧝"
            arccsch(-Constant I) ==> "acsch(-j)" // "-acsch(j)" // "1/2*π*j"
            arccsch(-1Q/2Q*Constant I) ==> "acsch(-j/2)" // "-acsch(j/2)" // "acsc(1/2)*j"
            arccsch(Constant I) ==> "acsch(j)" // "-1/2*π*j"
            arccsch(2Q*Constant I) ==> "acsch(2*j)" // "-1/6*π*j"

            Calculus.differentiate x (arccsch(x)) ==> "-1/(sqrt(1 + 1/x^2)*x^2)" // "-1/(sqrt(1 + 1/x^2)*x)"
            Calculus.differentiate x (x*arccsch(x)) ==> "-1/(sqrt(1 + 1/x^2)*x) + acsch(x)" // "-1/(sqrt(1 + 1/x^2)*x) + acsch(x)"

            // Operators.arcsech
            arcsech(0Q) ==> "asech(0)" // "∞"
            arcsech(-Constant I) ==> "asech(-j)"
            arcsech(-1Q/2Q*Constant I) ==> "asech(-j/2)"
            arcsech(Constant I) ==> "asech(j)"
            arcsech(2Q*Constant I) ==> "asech(2*j)"

            Calculus.differentiate x (arcsech(x)) ==> "sqrt((1 - x)/(1 + x))/(x*(-1 + x))"
            Calculus.differentiate x (x*arcsech(x)) ==> "sqrt((1 - x)/(1 + x))/(-1 + x) + asech(x)"

            // Operators.arccoth
            arccoth(0Q) ==> "acoth(0)" // "1/2*π*j"
            arccoth(-Constant I) ==> "acoth(-j)" // "-acoth(j)" // "1/4*π*j"
            arccoth(-1Q/2Q*Constant I) ==> "acoth(-j/2)" // "-acoth(j/2)" // "acot(1/2)*j"
            arccoth(Constant I) ==> "acoth(j)" // "-1/4*π*j"
            arccoth(2Q*Constant I) ==> "acoth(2*j)" // "-atan(2)*j"

            Calculus.differentiate x (x*arccoth(x)) ==> "x/(1 - x^2) + acoth(x)"
        }
    ]
