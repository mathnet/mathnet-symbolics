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

number 2 * x

1 / x
2 + 1/x - 1
2*x*3
-x*y/3

(x**2)**3

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

Expand.algebraicExpand ((x+1)*(x+3))
Expand.algebraicExpand ((a+b)**2)
Expand.algebraicExpand ((a+b)**3)
Expand.algebraicExpand ((a+b)**4)
Expand.algebraicExpand ((a+b+c)**2)


module ``single variable polynomials`` =

    open Polynomials

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


module ``general polynomials`` =

    open GeneralPolynomials

    isMonomial (Set.ofList [x;y]) (a * x**2 * y**2) // true
    isMonomial (Set.ofList [x;y]) (ln(a) * x**2 * y**2) // true
    isMonomial (Set.ofList [x;y]) (x**2 + y**2) // false
    isPolynomial (Set.ofList [x;y]) (x**2 + y**2) // true
    isPolynomial (Set.ofList [x+1]) ((x+1)**2 + 2*(x+1)) // true
    isPolynomial (Set.ofList [x]) ((x+1)*(x+3)) // false

    variables (a * x**2 * y**2)
    variables ((x+1)**2 + 2*(x+1))
    variables ((x+1)*(x+3))
    variables ((x+1)*(x+3)*sin(x))

    degreeMonomial (Set.ofList [x;y]) (a * x**2 * y * b**2) // 3 (x:2 + y:1)
    degree (Set.ofList [x;y]) (a*x**2 + b*x + c) // 2
    degree (Set.ofList [x;z]) (2*x**2*y**8*z**2 + a*x*z**6) // 7
    totalDegree (2*x**2*y*z**2 + a*x*z**6) // 8

    coefficient x 2 (a*x**2 + b*x + c) // a
    coefficient x 2 (a*x*x + b*x + c) // a
    coefficient x 1 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 7 + 3y^2
    coefficient x 3 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 0
    leadingCoefficient x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 7y^3 + 5y
    coefficients x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 9, 3y^2, 7y^3 + 5y

    collectTermsMonomial (Set.ofList [x;y]) (2*x*a)
    collectTermsMonomial (Set.ofList [x;y]) (2*a*x*b*y*3)
    collectTermsMonomial (Set.ofList [x;y]) (2*a*x*b*y**3*x*3)

    collectTerms (Set.ofList [x;y]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b)
    collectTerms (Set.ofList [a;b]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b)
    collectTerms (Set.ofList [x;ln(a)]) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c)
