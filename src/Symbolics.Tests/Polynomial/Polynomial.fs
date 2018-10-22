module Polynomial

open Expecto
open MathNet.Symbolics
open Operators

let tests =
    testList "Polynomial" [

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
    ]
