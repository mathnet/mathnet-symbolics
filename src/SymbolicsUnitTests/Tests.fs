module Tests

#if INTERACTIVE
#load "Interactive.fsx"
#endif

open NUnit.Framework
open FsUnit
open System.Collections.Generic
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

open Operators

// Test: x should evaluate to expected
let inline (-->) x expected = x |> should equal expected

// Test: x should evaluate to the expected string when formatted *nicely*
let inline (==>) x expected = Infix.print x |> should equal expected

// Test: x should evaluate to the expected string when formatted *strictly* (not denormalized)
let inline (===>) x expected = Infix.printStrict x |> should equal expected

// extra test helpers for tuples, list, arrays and hash-sets - maybe there's a better way?
let inline (==|>) (x1, x2) expected = (Infix.print x1, Infix.print x2) |> should equal expected
let inline (==||>) (x1, x2, x3) expected = (Infix.print x1, Infix.print x2, Infix.print x3) |> should equal expected
let inline (==+>) x expected = List.iter2 (fun x e -> Infix.print x |> should equal e) x expected
let inline (==->) x expected = Array.iter2 (fun x e -> Infix.print x |> should equal e) x expected
let inline (==*>) (x:HashSet<Expression>) (expected:string list) = HashSet(expected).SetEquals(x |> Seq.map Infix.print) |> should be True

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


[<Test>]
let ``Number Expressions`` () =

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
    Numbers.compare 1Q PositiveInfinity --> -1
    Numbers.compare 1Q NegativeInfinity --> 1
    Numbers.compare PositiveInfinity 1Q --> 1
    Numbers.compare NegativeInfinity 1Q --> -1

    Numbers.max [ 2Q; 4Q; 7Q/2 ] --> 4Q
    Numbers.max [ 2Q; 4Q; 9Q/2 ] --> 9Q/2
    Numbers.max [ -2Q; -4Q; -7Q/2 ] --> -2Q

    Numbers.gcd [ 4Q; 6Q; 10Q ] --> 2Q
    Numbers.lcm [ 4Q; 6Q; 10Q ] --> 60Q


[<Test>]
let ``Expressions are always in auto-simplified form`` () =

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
    -x*y/3 ==> "-(1/3)*x*y"

    ((x*y)**(1Q/2)*z**2)**2 ==> "x*y*z^4"
    (a/b/(c*a))*(c*d/a)/d ===> "a^(-1)*b^(-1)" // strict
    (a/b/(c*a))*(c*d/a)/d ==> "1/(a*b)" // nice
    a**(3Q/2)*a**(1Q/2) ==> "a^2"

    x + ln x ==> "x + ln(x)"
    x + ln (x+1) ==> "x + ln(1 + x)"
    2*abs x ==> "2*|x|"
    x + abs (-x) ==> "x + |x|"
    abs (-3Q) ==> "3"
    exp 0Q ==> "1"

    sin x ==> "sin(x)"
    cot x ==> "1/tan(x)"
    sec x ==> "1/cos(x)"


[<Test>]
let ``Parse F# quotations`` () =

    Quotations.parse <@ 3 @> ==> "3"
    Quotations.parse <@ x @> ==> "x"
    Quotations.parse <@ fun x -> x @> ==> "x"
    Quotations.parse <@ 3/4 @> ==> "3/4"
    Quotations.parse <@ fun x -> 3/x @> ==> "3/x"
    Quotations.parse <@ -x*y/3 @> ==> "-(1/3)*x*y"
    Quotations.parse <@ fun x y -> -x*y/3 @> ==> "-(1/3)*x*y"
    Quotations.parse <@ fun (x, y) -> -x*y/3 @> ==> "-(1/3)*x*y"


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

    Infix.parseOrUndefined "1/(a*b)" ==> "1/(a*b)"
    Infix.parseOrUndefined "exp(a)^exp(b)" ==> "exp(a)^exp(b)"
    Infix.parseOrUndefined "a^b^c" ==> "a^(b^c)"
    Infix.parseOrUndefined "|a-2|-1" ==> "-1 + |-2 + a|"

    Infix.parseOrUndefined "(y-1)*10 + 2" ==> "2 + 10*(-1 + y)"

    Infix.parseOrUndefined "15" ==> "15"
    Infix.parseOrUndefined "1.5" ==> "3/2"
    Infix.parseOrUndefined "0.25" ==> "1/4"
    Infix.parseOrUndefined "0.0250" ==> "1/40"
    Infix.parseOrUndefined "2.25" ==> "9/4"
    Infix.parseOrUndefined "2.250" ==> "9/4"
    Infix.parseOrUndefined "0.001" ==> "1/1000"
    Infix.parseOrUndefined "2.00" ==> "2"

    Infix.parseOrUndefined "1.5*a + o" ==> "(3/2)*a + o"

    Infix.parseOrUndefined ".001" ==> "Undefined"
    Infix.parseOrUndefined "1." ==> "Undefined"


[<Test>]
let ``Algebraic Expansion`` () =

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


[<Test>]
let ``Algebaric Operators`` () =

    Structure.substitute 3Q 4Q (x**3) ==> "x^4"
    Structure.map (fun x -> -x) (x + y**2) ==> "-x - y^2"
    negate (x + y**2) ==> "-(x + y^2)"

    Algebraic.factors (b*cos(x)*ln(d)*x) ==+> ["b"; "x"; "ln(d)"; "cos(x)"]
    Algebraic.factors (b+cos(x)) ==+> ["b + cos(x)"]
    Algebraic.summands (b+cos(x)+ln(d)+x) ==+> ["b"; "x"; "ln(d)"; "cos(x)"]
    Algebraic.summands (b*cos(x)) ==+> ["b*cos(x)"]

    Algebraic.separateFactors x (b*cos(x)*ln(d)*x) ==|> ("b*ln(d)", "x*cos(x)")
    Algebraic.separateFactors x (c*x*sin(x)/2) ==|> ("(1/2)*c", "x*sin(x)")

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

    Exponential.expand (exp(2*x+y)) ==> "exp(x)^2*exp(y)"
    Exponential.expand (exp(2*a*x + 3*y*z)) ==> "exp(a*x)^2*exp(y*z)^3"
    Exponential.expand (exp(2*(x+y))) ==> "exp(x)^2*exp(y)^2"
    Exponential.expand (1/(exp(2*x) - (exp(x))**2)) ==>  "ComplexInfinity"
    Exponential.expand (exp((x+y)*(x-y))) ==> "exp(x^2)/exp(y^2)"
    Exponential.expand (ln((c*x)**a) + ln(y**b*z)) ==> "a*ln(c) + a*ln(x) + b*ln(y) + ln(z)"

    Exponential.contract (exp(x)*exp(y)) ==> "exp(x + y)"
    Exponential.contract (exp(x)**a) ==> "exp(a*x)"
    Exponential.contract (exp(x)*(exp(x) + exp(y))) ==> "exp(2*x) + exp(x + y)"
    Exponential.contract ((exp(exp(x)))**exp(y)) ==> "exp(exp(x + y))"

    Exponential.simplify (1/(exp(x)*(exp(y)+exp(-x))) - (exp(x+y)-1)/((exp(x+y))**2-1)) ==> "0"

    Trigonometric.expand (sin(2*x)) ==> "2*sin(x)*cos(x)"
    Trigonometric.expand (sin(a+x)) ==> "sin(x)*cos(a) + sin(a)*cos(x)"
    Trigonometric.expand (sin(2*x + 3*y)) ==> "(-sin(x)^2 + cos(x)^2)*(-sin(y)^3 + 3*sin(y)*cos(y)^2) + 2*sin(x)*cos(x)*(-3*sin(y)^2*cos(y) + cos(y)^3)"
    Trigonometric.expand (sin(2*(x+y))) ==> "2*sin(y)*(-sin(x)^2 + cos(x)^2)*cos(y) + 2*sin(x)*cos(x)*(-sin(y)^2 + cos(y)^2)"
    Trigonometric.expand (sin(2*(x+y))) |> Algebraic.expand ==> "-2*sin(x)*sin(y)^2*cos(x) - 2*sin(x)^2*sin(y)*cos(y) + 2*sin(y)*cos(x)^2*cos(y) + 2*sin(x)*cos(x)*cos(y)^2"
    Trigonometric.expand (cos(5*x)) ==> "5*sin(x)^4*cos(x) - 10*sin(x)^2*cos(x)^3 + cos(x)^5"
    // TODO: should actually be Undefined
    Trigonometric.expand ((sin(2*x)-2*sin(x)*cos(x))/((sin(x))**2 + (cos(x))**2 - 1)) ==> "0"

    Trigonometric.contract (sin(a)*sin(b)) ==> "-(1/2)*cos(a + b) + (1/2)*cos(a - b)"
    Trigonometric.contract ((sin(x) + cos(y))*cos(y)) ==> "1/2 + (1/2)*sin(x + y) + (1/2)*sin(x - y) + (1/2)*cos(2*y)"
    Trigonometric.contract (sin(x)**2*cos(x)**2) ==> "1/8 - (1/8)*cos(4*x)"
    Trigonometric.contract (cos(x)**4) ==> "3/8 + (1/2)*cos(2*x) + (1/8)*cos(4*x)"

    Trigonometric.simplify ((cos(x)+sin(x))**4 + (cos(x)-sin(x))**4 + cos(4*x) - 3) ==> "0"

    // TODO: expected: 0
    Trigonometric.simplify (sin(x) + sin(y) - 2*sin(x/2+y/2)*cos(x/2-y/2))
        ==> "sin(y) - (1/2)*sin(x - y) - (1/2)*sin((1/2)*x - (1/2)*y - ((1/2)*x - (1/2)*y)) - (1/2)*sin(-(1/2)*x + (1/2)*y - ((1/2)*x - (1/2)*y)) - sin((1/2)*x + (1/2)*y - ((1/2)*x - (1/2)*y))"


[<Test>]
let ``Differentiation and Taylor Series`` () =

    Calculus.differentiate x (a*x) ==> "a"
    Calculus.differentiate x (sin(x)) ==> "cos(x)"
    Calculus.differentiate x (x*sin(x)) ==> "sin(x) + x*cos(x)"
    Calculus.differentiate x (a*x**2) ==> "2*a*x"
    Calculus.differentiate x (a*x**b) ==> "a*b*x^(-1 + b)"
    Calculus.differentiate x (a*x**2 + b*x + c) ==> "b + 2*a*x"

    Calculus.taylor 3 x 0Q (1/(1-x)) ==> "1 + x + x^2"
    Calculus.taylor 3 x 1Q (1/x) ==> "3 - 3*x + x^2"
    Calculus.taylor 3 x 1Q (ln(x)) ==> "-3/2 + 2*x - (1/2)*x^2"
    Calculus.taylor 4 x 1Q (ln(x)) ==> "-11/6 + 3*x - (3/2)*x^2 + (1/3)*x^3"
    Calculus.taylor 3 x 0Q (sin(x)+cos(x)) ==> "1 + x - (1/2)*x^2"
    Calculus.taylor 4 x 0Q (sin(x)+cos(x)) ==> "1 + x - (1/2)*x^2 - (1/6)*x^3"
    (sin(x)+cos(x)) |> Calculus.taylor 4 x 0Q ==> "1 + x - (1/2)*x^2 - (1/6)*x^3"


[<Test>]
let ``Tangent and Normal Lines`` () =

    (1/z) |> Calculus.tangentLine z 3Q ==> "2/3 - (1/9)*z"
    (x**3 - 12*x**2 - c) |> Calculus.tangentLine x 1Q ==> "10 - c - 21*x"

    Calculus.normalLine z 3Q (1/z) ==> "-80/3 + 9*z"
    (1/z) |> Calculus.normalLine z 3Q ==> "-80/3 + 9*z"


[<Test>]
let ``Polynomial Division`` () =

    Polynomial.divide x (5*x**2 + 4*x + 1) (2*x + 3) ==|> ("-7/4 + (5/2)*x", "25/4")
    Polynomial.divide x (x**3 - 2*x**2 - 4) (x-3) ==|> ("3 + x + x^2", "5")
    Polynomial.quot x (x**3 - 2*x**2 - 4) (x-3) ==> "3 + x + x^2"
    Polynomial.remainder x (x**3 - 2*x**2 - 4) (x-3) ==> "5"

    Polynomial.divide x (3*x**3 + x**2 + x + 5) (5*x**2 - 3*x + 1) ==|> ("14/25 + (3/5)*x", "111/25 + (52/25)*x")
    Polynomial.divide x (3*x**3 + x**2 + x + 5) (2Q) ==|> ("5/2 + (1/2)*x + (1/2)*x^2 + (3/2)*x^3", "0")
    Polynomial.pseudoDivide x (3*x**3 + x**2 + x + 5) (5*x**2 - 3*x + 1) ==||> ("14 + 15*x", "111 + 52*x", "25")
    Polynomial.pseudoDivide x (3*x**3 + x**2 + x + 5) (2Q) ==||> ("5 + x + x^2 + 3*x^3", "0", "2")

    // tangent of polynomial at x = 1?
    Polynomial.divide x (x**3 - 12*x**2 - c) (x**2-2*x+1) ==|> ("-10 + x", "10 - c - 21*x")

    /// Find tangent equation for x(symbol) at symbol=a
    let tangent symbol x a =
        let m = Calculus.differentiate symbol x |> Structure.substitute symbol a
        m*(symbol - a) + Structure.substitute symbol a x |> Algebraic.expand

    tangent x (x**3 - 12*x**2 - c) 1Q ==> "10 - c - 21*x"
    tangent z (1/z) 3Q ==> "2/3 - (1/9)*z"


[<Test>]
let ``Polynomial Expansion`` () =

    let ex = Polynomial.polynomialExpansion x y (x**5 + 11*x**4 + 51*x**3 + 124*x**2 + 159*x + 86) (x**2 + 4*x + 5)
    ex ==> "1 + x + (2 + x)*y + (3 + x)*y^2"

    let exs = ex |> Structure.substitute y (x**2 + 4*x + 5)
    exs ==> "1 + x + (2 + x)*(5 + 4*x + x^2) + (3 + x)*(5 + 4*x + x^2)^2"

    // get back to original polynomial:
    Algebraic.expand exs ==> "86 + 159*x + 124*x^2 + 51*x^3 + 11*x^4 + x^5"


[<Test>]
let ``Polynomial Euclidean/GCD`` () =

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
    Polynomial.halfExtendedGcd x u v ==|> ("1 + x", "3/5 - (1/5)*x")
    let g,a,b = Polynomial.extendedGcd x u v
    g ==> "1 + x"
    a ==> "3/5 - (1/5)*x"
    b ==> "2 - (6/5)*x + (1/5)*x^2"

    // hence u*a + v*b = g ? indeed:
    u*a + v*b |> Algebraic.expand ==> "1 + x"

    // Let's try to find s, t such that s*u + t*v = x^2 - 1
    let s, t = Polynomial.diophantineGcd x (x**4 - 2*x**3 - 6*x**2 + 12*x + 15) (x**3 + x**2 - 4*x - 4) (x**2 - 1)
    s ==> "-3/5 + (4/5)*x - (1/5)*x^2"
    t ==> "-2 + (16/5)*x - (7/5)*x^2 + (1/5)*x^3"
    s*u + t*v |> Algebraic.expand ==> "-1 + x^2"

    // (x^2 + 3*x)/((x + 1)*(x^2 - 2*x + 1)) --> (-1/2)/(x+1) + (1/2 + (3/2)*x)/(x^2-2*x+1)
    let a0, ax = Polynomial.partialFraction x (x**2+3*x) [x+1; x**2-2*x+1]
    a0 ==> "0"
    ax ==+> ["-1/2"; "1/2 + (3/2)*x"]


[<Test>]
let ``Evaluate some expression to floating point numbers`` () =

    let (=!=) x expected = x.ToString() |> should equal (expected.ToString())

    let symbols = Map.ofList ["a", Real 2.0; "b", Real 3.0; "c", Complex (complex 1.0 -1.0)]
    Evaluate.evaluate symbols (a) =!= Real 2.0
    Evaluate.evaluate symbols (1Q/2) =!= Real 0.5
    Evaluate.evaluate symbols (sin(a) + ln(b)) =!= Real 2.007909715
    Evaluate.evaluate symbols (a*x**2 + b*x + c |> Structure.substitute x (number 1/2)) =!= Complex (complex 3.0 -1.0)
    Evaluate.evaluate symbols (1Q/0Q) =!= ComplexInf


[<Test>]
let ``Primitive Equation Solver`` () =

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

    solveLine x y (x/2+y/3) 1Q ==> "3 - (3/2)*x" //   -->  x/2 + y/3 = 1  ->  y = -3/2*x + 3
    solveLine x y (x/a) ((x+y)/b) ==> "(-1 + b/a)*x"
    solveLine x y ((y/x-2)/(1-3/x)) 6Q ==> "-18 + 8*x"


[<Test>]
let ``General Polynomial Expressions`` () =

    Polynomial.isMonomial x (a * x**2) --> true
    Polynomial.isMonomial x (ln(a) * x**2) --> true
    Polynomial.isMonomial x (x**2 + a) --> false
    Polynomial.isPolynomial x (x**2 + x**3) --> true
    Polynomial.isPolynomial x (x**2 + 2*x) --> true
    Polynomial.isPolynomial x ((x+1)*(x+3)) --> false

    Polynomial.degreeMonomial x (a * x**2 * x * b**2) ==> "3"
    Polynomial.degree x (a*x**2 + b*x + c) ==> "2"

    Polynomial.coefficient x 2 (a*x**2 + b*x + c) ==> "a"
    Polynomial.coefficient x 2 (a*x*x + b*x + c) ==> "a"
    Polynomial.coefficient x 1 (3*x*y**2 + 5*x**2*y + 7*x + 9) ==> "7 + 3*y^2"
    Polynomial.coefficient x 3 (3*x*y**2 + 5*x**2*y + 7*x + 9) ==> "0"
    Polynomial.leadingCoefficient x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) ==> "5*y + 7*y^3"
    Polynomial.coefficients x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) ==-> [|"9"; "3*y^2"; "5*y + 7*y^3"|]

    Polynomial.collectTermsMonomial x (2*x*a) ==|> ("2*a", "x")
    Polynomial.collectTermsMonomial x (2*a*x*b*3) ==|> ("6*a*b", "x")
    Polynomial.collectTermsMonomial x (2*a*x**3*b*x*3) ==|> ("6*a*b", "x^4")

    Polynomial.collectTerms x (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) ==> "x*(4*a + 5*b + 2*a*y + 3*b*y)"
    Polynomial.collectTerms a (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) ==> "5*b*x + 3*b*x*y + a*(4*x + 2*x*y)"
    Polynomial.collectTerms (ln(a)) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c) ==> "c + 5*b*x + 3*b*x*y + (4*x + 2*x*y)*ln(a)"

    Polynomial.isMonomialMV (Polynomial.symbols [x;y]) (a * x**2 * y**2) --> true
    Polynomial.isMonomialMV (Polynomial.symbols [x;y]) (ln(a) * x**2 * y**2) --> true
    Polynomial.isMonomialMV (Polynomial.symbols [x;y]) (x**2 + y**2) --> false
    Polynomial.isPolynomialMV (Polynomial.symbols [x;y]) (x**2 + y**2) --> true
    Polynomial.isPolynomialMV (Polynomial.symbols [x+1]) ((x+1)**2 + 2*(x+1)) --> true
    Polynomial.isPolynomialMV (Polynomial.symbols [x]) ((x+1)*(x+3)) --> false

    Polynomial.degreeMonomialMV (Polynomial.symbols [x;y]) (a * x**2 * y * b**2) ==> "3" // (x:2 + y:1)
    Polynomial.degreeMV (Polynomial.symbols [x;y]) (a*x**2 + b*x + c) ==> "2"
    Polynomial.degreeMV (Polynomial.symbols [x;z]) (2*x**2*y**8*z**2 + a*x*z**6) ==> "7"

    Polynomial.variables (a * x**2 * y**2) ==*> ["a"; "x"; "y"]
    Polynomial.variables ((x+1)**2 + 2*(x+1)) ==*> ["1 + x"]
    Polynomial.variables ((x+1)*(x+3)) ==*> ["1 + x"; "3 + x"]
    Polynomial.variables ((x+1)*(x+3)*sin(x)) ==*> ["1 + x"; "3 + x"; "sin(x)"]
    Polynomial.totalDegree (2*x**2*y*z**2 + a*x*z**6) ==> "8"

    Polynomial.coefficient x 2 (a*x**2 + b*x + c) ==> "a"
    Polynomial.coefficient x 2 (a*x*x + b*x + c) ==> "a"
    Polynomial.coefficient x 1 (3*x*y**2 + 5*x**2*y + 7*x + 9) ==> "7 + 3*y^2"
    Polynomial.coefficient x 3 (3*x*y**2 + 5*x**2*y + 7*x + 9) ==> "0"
    Polynomial.leadingCoefficient x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) ==> "5*y + 7*y^3"
    Polynomial.coefficients x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) ==-> [|"9"; "3*y^2"; "5*y + 7*y^3"|]

    Polynomial.collectTermsMonomialMV (Polynomial.symbols [x;y]) (2*x*a) ==|> ("2*a", "x")
    Polynomial.collectTermsMonomialMV (Polynomial.symbols [x;y]) (2*a*x*b*y*3) ==|> ("6*a*b", "x*y")
    Polynomial.collectTermsMonomialMV (Polynomial.symbols [x;y]) (2*a*x*b*y**3*x*3) ==|> ("6*a*b", "x^2*y^3")

    Polynomial.collectTermsMV (Polynomial.symbols [x;y]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) ==> "(4*a + 5*b)*x + (2*a + 3*b)*x*y"
    Polynomial.collectTermsMV (Polynomial.symbols [a;b]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) ==> "a*(4*x + 2*x*y) + b*(5*x + 3*x*y)"
    Polynomial.collectTermsMV (Polynomial.symbols [x;ln(a)]) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c) ==> "c + x*(5*b + 3*b*y) + x*(4 + 2*y)*ln(a)"

    Polynomial.isSquareFree x (x**3 + 1) --> true
    Polynomial.isSquareFree x (x**2 - 2) --> true
    Polynomial.isSquareFree x (8*x**3 + 12*x**2 + 6*x + 1) --> false
    
    Polynomial.factorSquareFree x (x**8 + 6*x**6 + 12*x**4 + 8*x**2) ==> "x^2*(2 + x^2)^3"

    let sf = Polynomial.factorSquareFree x (x**5 + 6*x**4 + 10*x**3 - 4*x**2 - 24*x - 16)
    sf ==> "(2 + x)^3*(-2 + x^2)"
    Algebraic.expand sf ==> "-16 - 24*x - 4*x^2 + 10*x^3 + 6*x^4 + x^5"


[<Test>]
let ``General Rational Expressions`` () =

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
    Rational.rationalize (1/(1+1/x)**(1Q/2) + (1+1/x)**(3Q/2)) ==> "(x^2 + (1 + x)^2)/(x^2*((1 + x)/x)^(1/2))"
    Rational.rationalize ((1+1/x)**2) ==> "(1 + x)^2/x^2"

    Rational.rationalize (a/b + c/d + e/f) ==> "(b*d*e + (b*c + a*d)*f)/(b*d*f)"
    Rational.expand (a/b + c/d + e/f) ==> "(b*d*e + b*c*f + a*d*f)/(b*d*f)"

    Rational.rationalize (((1/((x+y)**2+1))**(1Q/2)+1)*((1/((x+y)**2+1))**(1Q/2)-1)/(x+1))
        ==> "((-1 + (1/(1 + (x + y)^2))^(1/2))*(1 + (1/(1 + (x + y)^2))^(1/2)))/(1 + x)"
    Rational.expand (((1/((x+y)**2+1))**(1Q/2)+1)*((1/((x+y)**2+1))**(1Q/2)-1)/(x+1))
        ==> "(-x^2 - 2*x*y - y^2)/(1 + x + x^2 + x^3 + 2*x*y + 2*x^2*y + y^2 + x*y^2)"

    Rational.rationalize (1/(1/a + c/(a*b)) + (a*b*c + a*c**2)/(b+c)**2-a) |> Algebraic.expand ==> "0"
    Rational.expand (1/(1/a + c/(a*b)) + (a*b*c + a*c**2)/(b+c)**2-a) ==> "0"

    Rational.rationalize (x/z + y/z**2) ==> "(y*z + x*z^2)/z^3"
    Rational.simplify z (x/z + y/z**2) ==> "(y + x*z)/z^2"

    Rational.simplify x ((x**2-1)/(x+1)) ==> "-1 + x"
    Rational.simplify x ((x+1)/(x**2 - 1 - (x+1)*(x-1))) ==> "ComplexInfinity"
    Rational.simplify x (1/(1+1/(x+1)) + 2/(x+2))  ==> "(3 + x)/(2 + x)"


[<Test>]
let ``Single Variable Polynomials`` () =

    SingleVariablePolynomial.isMonomialSV x (Quotations.parse <@ fun x -> 3*x @>) --> true
    SingleVariablePolynomial.isMonomialSV x (Quotations.parse <@ 3*x+2 @>) --> false
    SingleVariablePolynomial.isMonomialSV x (3*(x*x)) --> true
    SingleVariablePolynomial.isMonomialSV x (a*x) --> false
    SingleVariablePolynomial.isMonomialSV y (3*x) --> false
    SingleVariablePolynomial.degreeMonomialSV x 0Q ==> "NegativeInfinity"
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
    SingleVariablePolynomial.coefficientDegreeMonomialSV x 0Q ==|> ("0", "NegativeInfinity")
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
