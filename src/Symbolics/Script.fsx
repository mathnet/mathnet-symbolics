#load "MathNet.Symbolics.fsx"

open System
open System.Numerics
open Microsoft.FSharp
open MathNet.Numerics
open MathNet.Symbolics

open Elementary
open Functions
open Calculus

let x = symbol "x"
let y = symbol "y"
let z = symbol "z"
let a = symbol "a"
let b = symbol "b"
let c = symbol "c"
let d = symbol "d"
let e = symbol "e"
let f = symbol "f"

number 2 * x

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

((x*y)**((number 1)/2)*z**2)**2

algebraicExpand ((a*x**2 + b*x + c)/(d*x + e))
let p = algebraicExpand ((a*x**2 + b*x + c)*(d*x**2 + e*x + f))
Polynomial.coefficients x p
Polynomial.leadingCoefficient x p
Polynomial.collectTerms (set [x]) p
Polynomial.degree (set [x]) p
Polynomial.totalDegree p
Polynomial.variables p

Polynomial.polynomialDivision x (5*x**2 + 4*x + 1) (2*x + 3) // q=-7/4+5/2*x, r=25/4
Polynomial.polynomialDivision x (x**3 - 2*x**2 - 4) (x-3) // q=2+x+x^2, r=5

// tangent of polynomial at x = 1?
Polynomial.polynomialDivision x (x**3 - 12*x**2 - a) (x**2-2*x+1) // q=-10x, r=10-a-21x (=u+v*x)
let v = differentiate x (x**3 - 12*x**2 - a) |> substitute x (number 1) // v=-21
let u = (x**3 - 12*x**2 - a) - v*x |> substitute x (number 1)  // u=10-a

let sqr2 = (number 2)**(number 1/number 2)
Polynomial.polynomialDivision x ((2-4*sqr2)*x**2 + (-1+4*sqr2)*x - 3+3*sqr2) ((1-2*sqr2)*x + 1-sqr2)

// (1+x) + (2+x)y + (2+x)y^2
let ex = Polynomial.polynomialExpansion x y (x**5 + 11*x**4 + 51*x**3 + 124*x**2 + 159*x + 86) (x**2 + 4*x + 5)
// (1+x) + (2+x)*(5+4x+x^2) + (2+x)*(5+4x+x^2)^2
let exs = ex |> substitute y (x**2 + 4*x + 5)
// get back to original polynomial
algebraicExpand exs


x + ln x
x + ln (x+1)
2*abs x

substitute (number 3) (number 4) (x**3)
map (fun x -> -x) (x + y**2)
negate (x + y**2)

numerator (x/y)
denominator (x/y)
numerator (x**2/y**3)
denominator (x**2/y**3)

numerator (x**2)
denominator (x**2)
numerator (x**(-2))
denominator (x**(-2))

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


module ``Evaluate some expression to floating point numbers`` =

    open FloatingPoint

    let symbols = Map.ofList [a, freal 2.0; b, freal 3.0; c, fcomplex 1.0 -1.0]
    evaluate symbols (a)
    evaluate symbols (number 1/2)
    evaluate symbols (sin(a) + ln(b))
    evaluate symbols (a*x**2 + b*x + c |> substitute x (number 1/2))
    evaluate symbols (number 1/number 0)



module ``Single Variable Polynomials`` =

    open SingleVariablePolynomial

    isMonomial x <| Quotations.parse <@ fun x -> 3*x @>
    isMonomial x <| Quotations.parse <@ 3*x+2 @>
    isMonomial x (3*(x*x))
    isMonomial x (a*x) // false
    isMonomial y (3*x) // false
    degreeMonomial x (number 0)
    degreeMonomial x (number 1)
    degreeMonomial x (3*x)
    degreeMonomial x (3 * x*x)
    degreeMonomial x (3 * x*x * y) // undefined
    degreeMonomial x (3 + x) // undefined

    coefficientMonomial x (number 0)
    coefficientMonomial x (number 1)
    coefficientMonomial x (3 * x)
    coefficientMonomial x (3 * x*x)
    coefficientMonomial x (3 * x*x * y) // undefined
    coefficientMonomial x (3 + x) // undefined
    coefficientDegreeMonomial x (number 0)
    coefficientDegreeMonomial x (number 1)
    coefficientDegreeMonomial x (3*x)
    coefficientDegreeMonomial x (3*x*x)

    isPolynomial x (3*x)
    isPolynomial x (3*x+2)
    isPolynomial x (3*x*x+2)
    degree x (3*x*x + 2*x)
    degree x (3*x*x + 2*x*x*x)
    degree x (3*x + 2*x*(x**5) + 2*(x**3))

    coefficient x 0 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficient x 1 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficient x 2 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficient x 3 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficient x 4 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficient x 5 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficient x 6 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficient x 7 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    leadingCoefficient x (3*x*x + 2*x)
    leadingCoefficient x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    leadingCoefficient x (number 2)
    leadingCoefficient x (number 0)
    leadingCoefficientDegree x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
    coefficients x (3*x*x + 2*x)
    coefficients x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)


module ``General Polynomial Expressions`` =

    open Polynomial

    isMonomial (set [x;y]) (a * x**2 * y**2) // true
    isMonomial (set [x;y]) (ln(a) * x**2 * y**2) // true
    isMonomial (set [x;y]) (x**2 + y**2) // false
    isPolynomial (set [x;y]) (x**2 + y**2) // true
    isPolynomial (set [x+1]) ((x+1)**2 + 2*(x+1)) // true
    isPolynomial (set [x]) ((x+1)*(x+3)) // false

    variables (a * x**2 * y**2)
    variables ((x+1)**2 + 2*(x+1))
    variables ((x+1)*(x+3))
    variables ((x+1)*(x+3)*sin(x))

    degreeMonomial (set [x;y]) (a * x**2 * y * b**2) // 3 (x:2 + y:1)
    degree (set [x;y]) (a*x**2 + b*x + c) // 2
    degree (set [x;z]) (2*x**2*y**8*z**2 + a*x*z**6) // 7
    totalDegree (2*x**2*y*z**2 + a*x*z**6) // 8

    coefficient x 2 (a*x**2 + b*x + c) // a
    coefficient x 2 (a*x*x + b*x + c) // a
    coefficient x 1 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 7 + 3y^2
    coefficient x 3 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 0
    leadingCoefficient x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 5y + 7y^3
    coefficients x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 9, 3y^2, 5y + 7y^3

    collectTermsMonomial (set [x;y]) (2*x*a)
    collectTermsMonomial (set [x;y]) (2*a*x*b*y*3)
    collectTermsMonomial (set [x;y]) (2*a*x*b*y**3*x*3)

    collectTerms (set [x;y]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b)
    collectTerms (set [a;b]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b)
    collectTerms (set [x;ln(a)]) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c)



/// primitive equation solver (symbolic roots)
let solve x expr =

    if Polynomial.isPolynomial (set [x]) expr then
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
