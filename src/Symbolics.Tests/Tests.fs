namespace MathNet.Symbolics.Tests

open NUnit.Framework
open FsUnit
open FsUnitTyped

open MathNet.Numerics
open MathNet.Symbolics
open Operators

module Expressions =

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

    [<Test>]
    let ``Constant Expressions`` () =

        pi ==> "π"
        Expression.E ==> "e"
        Expression.I ==> "j"
        fromReal 1.23 ==> "1.23"
        fromReal -0.23 ==> "-0.23"

    [<Test>]
    let ``Approximations`` () =

        //real 1.1 + real 2.2 ==> "3.3"
        //real 1.1 * real 2.2 ==> "2.42"

        2 * real 2.0 ==> "4.0"
        x + 2*x ==> "3*x"
        x + 2.2*x ==> "3.2*x"

    [<Test>]
    let ``Real Infinity Expressions`` () =

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

    [<Test>]
    let ``Complex Infinity Expressions`` () =

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

        (-2) + (-3)*x + 5*y ==> "-2 - 3*x + 5*y"
        (-2.0) + (-3.0)*x + 5.0*y ==> "-2.0 - 3.0*x + 5.0*y"
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

        1 / x ==> "1/x"
        -x ==> "-x"
        2 + 1/x - 1 ==> "1 + 1/x"
        -(-x) ==> "x"
        1 / (1 / x) ==> "x"

        2*x*3 ==> "6*x"
        -x*y/3 ==> "-1/3*x*y"

        ((x*y)**(1Q/2)*z**2)**2 ==> "x*y*z^4"
        (a/b/(c*a))*(c*d/a)/d ==> "1/(a*b)"
        a**(3Q/2)*a**(1Q/2) ==> "a^2"

        x + ln x ==> "x + ln(x)"
        x + ln (x+1) ==> "x + ln(1 + x)"
        x + lg (x+1) ==> "x + lg(1 + x)"
        x + (log x (x+1)) ==> "x + log(x,1 + x)"
        2*abs x ==> "2*|x|"
        x + abs (-x) ==> "x + |x|"
        abs (-3Q) ==> "3"
        exp 0Q ==> "1"

        sin x ==> "sin(x)"

    [<Test>]
    let ``Parse F# quotations`` () =

        Quotations.parse <@ 3 @> ==> "3"
        Quotations.parse <@ x @> ==> "x"
        Quotations.parse <@ fun x -> x @> ==> "x"
        Quotations.parse <@ 3/4 @> ==> "3/4"
        Quotations.parse <@ fun x -> 3/x @> ==> "3/x"
        Quotations.parse <@ -x*y/3 @> ==> "-1/3*x*y"
        Quotations.parse <@ fun x y -> -x*y/3 @> ==> "-1/3*x*y"
        Quotations.parse <@ fun (x, y) -> -x*y/3 @> ==> "-1/3*x*y"

    [<Test>]
    let ``Algebraic Expansion`` () =

        // Auto-simplification does not expand expressions:
        (a+b)-(a+b) ==> "a + b - (a + b)"
        (a+b)-(a+b) |> Algebraic.expand ==> "0"
        2*(a+b)-(a+b) ==> "a + b"
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
        a/(b*(x+y)) |> Algebraic.expand ==> "a/(b*(x + y))" // does not expand

    [<Test>]
    let ``Structural Operators`` () =

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

    [<Test>]
    let ``Algebaric Operators`` () =

        negate (x + y**2) ==> "-(x + y^2)"

        Algebraic.factors (b*cos(x)*ln(d)*x) ==+> ["b"; "x"; "ln(d)"; "cos(x)"]
        Algebraic.factors (b*cos(x)*lg(d)*x) ==+> ["b"; "x"; "lg(d)"; "cos(x)"]
        Algebraic.factors (b*cos(x)*(log d (d*2))*x) ==+> ["b"; "x"; "cos(x)"; "log(d,2*d)"]
        Algebraic.factors (b+cos(x)) ==+> ["b + cos(x)"]
        Algebraic.summands (b+cos(x)+ln(d)+x) ==+> ["b"; "x"; "ln(d)"; "cos(x)"]
        Algebraic.summands (b+cos(x)+lg(d)+x) ==+> ["b"; "x"; "lg(d)"; "cos(x)"]
        Algebraic.summands (b+cos(x)+(log d (d*2))+x) ==+> ["b"; "x"; "cos(x)"; "log(d,2*d)"]
        Algebraic.summands (b*cos(x)) ==+> ["b*cos(x)"]

        Algebraic.factorsInteger (2Q/3*b*cos(x)) --> (2I, [1Q/3; b; cos(x)])

        Algebraic.separateFactors x (b*cos(x)*ln(d)*x) ==|> ("b*ln(d)", "x*cos(x)")
        Algebraic.separateFactors x (b*cos(x)*lg(d)*x) ==|> ("b*lg(d)", "x*cos(x)")
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
        Exponential.expand (lg((c*x)**a) + lg(y**b*z)) ==> "a*lg(c) + a*lg(x) + b*lg(y) + lg(z)"
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

    [<Test>]
    let ``Differentiation and Taylor Series`` () =

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
        Calculus.differentiate x (lg x) ==> "1/(x*ln(10))"
        Calculus.differentiate x (lg (x**2)) ==> "2/(x*ln(10))"
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

    [<Test>]
    let ``Tangent and Normal Lines`` () =

        (1/z) |> Calculus.tangentLine z 3Q ==> "2/3 - z/9"
        (x**3 - 12*x**2 - c) |> Calculus.tangentLine x 1Q ==> "10 - c - 21*x"

        Calculus.normalLine z 3Q (1/z) ==> "-80/3 + 9*z"
        (1/z) |> Calculus.normalLine z 3Q ==> "-80/3 + 9*z"

    [<Test>]
    let ``Evaluate some expression to floating point numbers`` () =

        let symbols = Map.ofList ["a", FloatingPoint.Real 2.0; "b", FloatingPoint.Real 3.0; "c", FloatingPoint.Complex (complex 1.0 -1.0)]
        Evaluate.evaluate symbols (a) --> FloatingPoint.Real 2.0
        Evaluate.evaluate symbols (1Q/2) --> FloatingPoint.Real 0.5
        Evaluate.evaluate symbols (sin(a) + ln(b)) --> FloatingPoint.Real (System.Math.Sin(2.0) + System.Math.Log(3.0))
        Evaluate.evaluate symbols (a*x**2 + b*x + c |> Structure.substitute x (number 1/2)) --> FloatingPoint.Complex (complex 3.0 -1.0)
        Evaluate.evaluate symbols (1Q/0Q) --> FloatingPoint.ComplexInf

        (fun () -> Evaluate.evaluate symbols (f) |> ignore) |> should (throwWithMessage "Failed to find symbol f") typeof<System.Exception>

        match Evaluate.evaluate symbols (sqrt(-1Q)) with
        | FloatingPoint.Complex c ->
            c.Real |> should (equalWithin 1e-12) 0.0f
            c.Imaginary |> should (equalWithin 1e-12) 1.0f
        | _ -> failwith "We expect a complex number here"


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

        solveLine x y (x/2+y/3) 1Q ==> "3 - 3/2*x" //   -->  x/2 + y/3 = 1  ->  y = -3/2*x + 3
        solveLine x y (x/a) ((x+y)/b) ==> "(-1 + b/a)*x"
        solveLine x y ((y/x-2)/(1-3/x)) 6Q ==> "-18 + 8*x"

    [<Test>]
    let ``Test for other trigonometric function`` () =

        let exrp = Infix.parseOrUndefined "tan(x)*25*csc(x)"
        exrp ==> "25*tan(x)*csc(x)"

        let expr2 = Operators.sec 32Q
        expr2 ==> "sec(32)"

        let exrp3 = Expression.Apply(Cot, expr2)
        exrp3 ==> "cot(sec(32))"

        let expr4 = Infix.parseOrUndefined "25*x*sec(x)"
        Calculus.differentiate x expr4  ==> "25*(sec(x) + x*tan(x)*sec(x))"

        let expr5 = Infix.parseOrUndefined "sinh(asinh(j)) + cosh(acosh(j)) + tanh(atanh(j)) + csch(acsch(j)) + sech(asech(j)) + coth(acoth(j))"
        expr5 ==> "2*j + sinh(1/2*π*j) + tanh(1/4*π*j) - csch(1/2*π*j) - coth(1/4*π*j)" // "6*j"
