#load "MathNet.Symbolics.fsx"

open System
open System.Numerics
open Microsoft.FSharp
open MathNet.Numerics
open MathNet.Symbolics

open Elementary
open Functions
open Calculus

// variables:
let x = symbol "x"
let y = symbol "y"
let z = symbol "z"
let a = symbol "a"
let b = symbol "b"
let c = symbol "c"
let d = symbol "d"
let e = symbol "e"
let f = symbol "f"

// numeric literal expressions:
number 3
3Q

compareNumber 0Q 1Q
compareNumber 1Q 1Q
compareNumber 1Q 2Q
compareNumber 0Q (1Q/2Q)
compareNumber 1Q (1Q/2Q)
compareNumber (1Q/2Q) 0Q
compareNumber (1Q/2Q) 1Q
compareNumber 1Q Expression.PositiveInfinity
compareNumber 1Q Expression.NegativeInfinity
compareNumber Expression.PositiveInfinity 1Q
compareNumber Expression.NegativeInfinity 1Q

x + y
y + x
x + x
x + 2*x
x + x*2
2*x + 3*x
a*x + 2*x
a*x + x*b
b*x + x*a
1 + x + y
x + 1 + y
x + y + 1

2*(a*b)
(a*b)*2

a*b + a*b
a*b + b*a

a + b + c + a*b + a*c + b*c
c*b + c*a + b*a + c + b + a

a**2 + b**2
b**2 + a**2

a**2 + a**3
a**3 + a**2

a**2 * b**2
b**2 * a**2

(a+c)**2 + (a+b)**2
(a+b)**2 + (a+c)**2

(a+c)**2 * (a+b)**2
(a+b)**2 * (a+c)**2

(a+c) * (a+b)
(a+b) * (a+c)

(1+x)**2 + (1+x)**3 + (1+y)**2
(1+x)**3 + (1+y)**2 + (1+x)**2
(1+y)**2 + (1+x)**2 + (1+x)**3

(a+b)*x
(a+b)*x*y
(a+b)*y*x
(a+b)*(x*y)
(a+b)*(y*x)

x*y
y*x
x**2*x
x*y*x**2
y*x*y**2
2*x*y
x*2*y
x*y*2

2*(x*y)*z**2
1*x*y*z**2
2*x*y*z*z**2

1 / x
2 + 1/x - 1
2*x*3
-x*y/3

x*x
x*x**2*x**3
(x**2)**3

(a+b)-(a+b) |> algebraicExpand
2*(a+b)-(a+b)
(a+b)-2*(a+b) |> algebraicExpand

(a*b)/(b*a)
(a*b)**2/(b*a)
(a*b)/(b*a)**2

(a+b)/(b+a)
(a+b)**2/(b+a)
(a+b)/(b+a)**2

((x*y)**(1Q/2)*z**2)**2

(a/b/(c*a))*(c*d/a)/d

a**(3Q/2)*a**(1Q/2)

(x*(y+1)**(3Q/2)+1)*(x*(y+1)**(3Q/2)-1) |> algebraicExpand |> algebraicExpand
sin(a*(x+y)) |> algebraicExpand // does not expand
a/(b*(x+y)) |> algebraicExpand // does not expand

x + ln x
x + ln (x+1)
2*abs x


substitute 3Q 4Q (x**3)
map (fun x -> -x) (x + y**2)
negate (x + y**2)

Quotations.parse <@ 3 @>
Quotations.parse <@ x @>
Quotations.parse <@ fun x -> x @>
Quotations.parse <@ 3/4 @>
Quotations.parse <@ fun x -> 3/x @>
Quotations.parse <@ -x*y/3 @>
Quotations.parse <@ fun x y -> -x*y/3 @>
Quotations.parse <@ fun (x, y) -> -x*y/3 @>

differentiate x (a*x)
differentiate x (sin(x))
differentiate x (x*sin(x))
differentiate x (a*x**2)
differentiate x (a*x**b)
differentiate x (a*x**2 + b*x + c)

algebraicExpand ((x+1)*(x+3))
algebraicExpand ((a+b)**2)
algebraicExpand ((a+b)**3)
algebraicExpand ((a+b)**4)
algebraicExpand ((a+b+c)**2)

algebraicExpand ((a*x**2 + b*x + c)/(d*x + e))
let p = algebraicExpand ((a*x**2 + b*x + c)*(d*x**2 + e*x + f))
Polynomial.coefficients x p
Polynomial.leadingCoefficient x p // ad
Polynomial.collectTerms x p
Polynomial.degree x p // 4
MultivariatePolynomial.totalDegree p // 6
MultivariatePolynomial.variables p // a,b,c,d,e,f,x


module ``Polynomial Division`` =

    Polynomial.polynomialDivision x (5*x**2 + 4*x + 1) (2*x + 3) // q=-7/4+5/2*x, r=25/4
    Polynomial.polynomialDivision x (x**3 - 2*x**2 - 4) (x-3) // q=3+x+x^2, r=5
    Polynomial.quot x (x**3 - 2*x**2 - 4) (x-3) // q=3+x+x^2
    Polynomial.remainder x (x**3 - 2*x**2 - 4) (x-3) // r=5

    // tangent of polynomial at x = 1?
    Polynomial.polynomialDivision x (x**3 - 12*x**2 - a) (x**2-2*x+1) // q=-10x, r=10-a-21x (=u+v*x)
    let v = differentiate x (x**3 - 12*x**2 - a) |> substitute x 1Q // v=-21
    let u = (x**3 - 12*x**2 - a) - v*x |> substitute x 1Q  // u=10-a

    let sqr2 = (2Q)**(1/2Q)
    Polynomial.polynomialDivision x ((2-4*sqr2)*x**2 + (-1+4*sqr2)*x - 3+3*sqr2) ((1-2*sqr2)*x + 1-sqr2)


module ``Polynomoal Expansion`` =

    // (1+x) + (2+x)y + (3+x)y^2
    let ex = Polynomial.polynomialExpansion x y (x**5 + 11*x**4 + 51*x**3 + 124*x**2 + 159*x + 86) (x**2 + 4*x + 5)
    // (1+x) + (2+x)*(5+4x+x^2) + (3+x)*(5+4x+x^2)^2
    let exs = ex |> substitute y (x**2 + 4*x + 5)
    // get back to original polynomial
    algebraicExpand exs


module ``Polynomial GCD`` =

    // 4 - 4*x - x^2 + x^3
    Polynomial.gcd x (x**7 - 4*x**5 - x**2 + 4) (x**5 - 4*x**3 - x**2 + 4)

    // 4 - 4*x - x^2 + x^3, -x, 1 + x^3
    Polynomial.extendedGcd x (x**7 - 4*x**5 - x**2 + 4) (x**5 - 4*x**3 - x**2 + 4)
    // verify A*u+B*v = gcd = 4 - 4*x - x^2 + x^3
    (-x)*(x**7 - 4*x**5 - x**2 + 4) + (1+x**3)*(x**5 - 4*x**3 - x**2 + 4) |> algebraicExpand


module ``Evaluate some expression to floating point numbers`` =

    open FloatingPoint

    let symbols = Map.ofList ["a", freal 2.0; "b", freal 3.0; "c", fcomplex 1.0 -1.0]
    evaluate symbols (a)
    evaluate symbols (1Q/2)
    evaluate symbols (sin(a) + ln(b))
    evaluate symbols (a*x**2 + b*x + c |> substitute x (number 1/2))
    evaluate symbols (1Q/0Q)


module ``General Univariate Polynomial Expressions`` =

    open Polynomial

    isMonomial x (a * x**2) // true
    isMonomial x (ln(a) * x**2) // true
    isMonomial x (x**2 + a) // false
    isPolynomial x (x**2 + x**3) // true
    isPolynomial x (x**2 + 2*x) // true
    isPolynomial x ((x+1)*(x+3)) // false

    degreeMonomial x (a * x**2 * x * b**2) // 3
    degree x (a*x**2 + b*x + c) // 2

    coefficient x 2 (a*x**2 + b*x + c) // a
    coefficient x 2 (a*x*x + b*x + c) // a
    coefficient x 1 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 7 + 3y^2
    coefficient x 3 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 0
    leadingCoefficient x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 5y + 7y^3
    coefficients x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 9, 3y^2, 5y + 7y^3

    collectTermsMonomial x (2*x*a)
    collectTermsMonomial x (2*a*x*b*3)
    collectTermsMonomial x (2*a*x**3*b*x*3)

    collectTerms x (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b)
    collectTerms a (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b)
    collectTerms (ln(a)) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c)


module ``General Multivariate Polynomial Expressions`` =

    open Polynomial
    open MultivariatePolynomial

    isMonomialMV (symbols [x;y]) (a * x**2 * y**2) // true
    isMonomialMV (symbols [x;y]) (ln(a) * x**2 * y**2) // true
    isMonomialMV (symbols [x;y]) (x**2 + y**2) // false
    isPolynomialMV (symbols [x;y]) (x**2 + y**2) // true
    isPolynomialMV (symbols [x+1]) ((x+1)**2 + 2*(x+1)) // true
    isPolynomialMV (symbols [x]) ((x+1)*(x+3)) // false

    degreeMonomialMV (symbols [x;y]) (a * x**2 * y * b**2) // 3 (x:2 + y:1)
    degreeMV (symbols [x;y]) (a*x**2 + b*x + c) // 2
    degreeMV (symbols [x;z]) (2*x**2*y**8*z**2 + a*x*z**6) // 7

    variables (a * x**2 * y**2)
    variables ((x+1)**2 + 2*(x+1))
    variables ((x+1)*(x+3))
    variables ((x+1)*(x+3)*sin(x))
    totalDegree (2*x**2*y*z**2 + a*x*z**6) // 8

    coefficient x 2 (a*x**2 + b*x + c) // a
    coefficient x 2 (a*x*x + b*x + c) // a
    coefficient x 1 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 7 + 3y^2
    coefficient x 3 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 0
    leadingCoefficient x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 5y + 7y^3
    coefficients x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 9, 3y^2, 5y + 7y^3

    collectTermsMonomialMV (symbols [x;y]) (2*x*a)
    collectTermsMonomialMV (symbols [x;y]) (2*a*x*b*y*3)
    collectTermsMonomialMV (symbols [x;y]) (2*a*x*b*y**3*x*3)

    collectTermsMV (symbols [x;y]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b)
    collectTermsMV (symbols [a;b]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b)
    collectTermsMV (symbols [x;ln(a)]) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c)


module ``Single Variable Polynomials`` =

    open SingleVariablePolynomial

    isMonomialSV x <| Quotations.parse <@ fun x -> 3*x @>
    isMonomialSV x <| Quotations.parse <@ 3*x+2 @>
    isMonomialSV x (3*(x*x))
    isMonomialSV x (a*x) // false
    isMonomialSV y (3*x) // false
    degreeMonomialSV x 0Q
    degreeMonomialSV x 1Q
    degreeMonomialSV x (3*x)
    degreeMonomialSV x (3 * x*x)
    degreeMonomialSV x (3 * x*x * y) // undefined
    degreeMonomialSV x (3 + x) // undefined

    coefficientMonomialSV x 0Q
    coefficientMonomialSV x 1Q
    coefficientMonomialSV x (3 * x)
    coefficientMonomialSV x (3 * x*x)
    coefficientMonomialSV x (3 * x*x * y) // undefined
    coefficientMonomialSV x (3 + x) // undefined
    coefficientDegreeMonomialSV x 0Q
    coefficientDegreeMonomialSV x 1Q
    coefficientDegreeMonomialSV x (3*x)
    coefficientDegreeMonomialSV x (3*x*x)

    isPolynomialSV x (3*x)
    isPolynomialSV x (3*x+2)
    isPolynomialSV x (3*x*x+2)
    degreeSV x (3*x*x + 2*x)
    degreeSV x (3*x*x + 2*x*x*x)
    degreeSV x (3*x + 2*x*(x**5) + 2*(x**3))

    coefficientSV x 0 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficientSV x 1 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficientSV x 2 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficientSV x 3 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficientSV x 4 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficientSV x 5 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficientSV x 6 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficientSV x 7 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    leadingCoefficientSV x (3*x*x + 2*x)
    leadingCoefficientSV x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    leadingCoefficientSV x 2Q
    leadingCoefficientSV x 0Q
    leadingCoefficientDegreeSV x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficientsSV x (3*x*x + 2*x)
    coefficientsSV x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)


module ``General Univariate Rational Expressions`` =

    open Rational

    numerator (x/y)
    denominator (x/y)
    numerator (x**2/y**3)
    denominator (x**2/y**3)

    numerator (x**2)
    denominator (x**2)
    numerator (x**(-2))
    denominator (x**(-2))

    let u = 2Q/3*(x*(x+1))/(x+2)*y**a in numerator u, denominator u

    isRational x ((x**2+1)/(2*x+3)) // true
    isRational x (1/x + 1/a) // false


module ``General Multivariate Rational Expressions`` =

    open MultivariateRational

    variables ((2*x + 3*y)/(z + 4)) // x,y,z
    variables (1/x + 1/y) // 1/x, 1/y
    variables (a/x + b/y) // a, 1/x, b, 1/y


module ``Primitive Equation Solver`` =

    let solve x expr =

        if Polynomial.isPolynomial x expr then
            match Polynomial.coefficients x expr with
            | [||] -> undefined
            | [| a |] -> x
            | [| a; b |] -> -a/b
            | _ -> failwith "higher polynomials not supported"

        else failwith "only general polynomial expressions supported for now"

    // 2+3*x = 0  -->  x = -2/3
    solve x (2+3*x)

    // sin(a)+x*cos(b)+c = 0  -->  x = -(c+sin(a))/cos(b)
    solve x (sin(a)+x*cos(b)+c)
