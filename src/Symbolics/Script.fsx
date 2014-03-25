#load "MathNet.Symbolics.fsx"

open System
open System.Numerics
open Microsoft.FSharp
open MathNet.Numerics
open MathNet.Symbolics

let x = symbol "x"
let y = symbol "y"

number 2 * x

1 / x
2 + 1/x - 1
2*x*3
-x*y/3

(x**2)**3

Elementary.substitute (number 3) (number 4) (x**3)
Elementary.map (fun x -> -x) (x + y**2)
negate (x + y**2)

Elementary.numerator (x/y)
Elementary.denominator (x/y)
Elementary.numerator (x**2/y**3)
Elementary.denominator (x**2/y**3)

Elementary.numerator (x**2)
Elementary.denominator (x**2)
Elementary.numerator (x**(-2))
Elementary.denominator (x**(-2))

Quotations.parse <@ 3 @>
Quotations.parse <@ x @>
Quotations.parse <@ fun x -> x @>
Quotations.parse <@ 3/4 @>
Quotations.parse <@ fun x -> 3/x @>
Quotations.parse <@ fun x y -> -x*y/3 @>
Quotations.parse <@ fun (x, y) -> -x*y/3 @>

Polynomials.isMonomial x <| Quotations.parse <@ fun x -> 3*x @>
Polynomials.isMonomial x <| Quotations.parse <@ 3*x+2 @>
Polynomials.isMonomial x (3*(x*x))
Polynomials.isMonomial y (3*x)
Polynomials.degreeMonomial x (number 0)
Polynomials.degreeMonomial x (number 1)
Polynomials.degreeMonomial x (3*x)
Polynomials.degreeMonomial x (3 * x*x)
Polynomials.degreeMonomial x (3 * x*x * y) // undefined
Polynomials.degreeMonomial x (3 + x) // undefined
Polynomials.coefficientMonomial x (number 0)
Polynomials.coefficientMonomial x (number 1)
Polynomials.coefficientMonomial x (3 * x)
Polynomials.coefficientMonomial x (3 * x*x)
Polynomials.coefficientMonomial x (3 * x*x * y) // undefined
Polynomials.coefficientMonomial x (3 + x) // undefined
Polynomials.coefficientDegreeMonomial x (number 0)
Polynomials.coefficientDegreeMonomial x (number 1)
Polynomials.coefficientDegreeMonomial x (3*x)
Polynomials.coefficientDegreeMonomial x (3*x*x)

Polynomials.isPolynomial x <| Quotations.parse <@ fun x -> 3*x @>
Polynomials.isPolynomial x <| Quotations.parse <@ 3*x+2 @>
Polynomials.isPolynomial x (3*x*x+2)
Polynomials.degree x (3*x*x + 2*x)
Polynomials.degree x (3*x*x + 2*x*x*x)
Polynomials.degree x (3*x + 2*x*(x**5) + 2*(x**3))
Polynomials.coefficient x 0 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.coefficient x 1 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.coefficient x 2 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.coefficient x 3 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.coefficient x 4 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.coefficient x 5 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.coefficient x 6 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.coefficient x 7 (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.leadingCoefficient x (3*x*x + 2*x)
Polynomials.leadingCoefficient x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.leadingCoefficient x (number 2)
Polynomials.leadingCoefficient x (number 0)
Polynomials.leadingCoefficientDegree x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
Polynomials.coefficients x (3*x*x + 2*x)
Polynomials.coefficients x (3*x + 2*x*(x**5) + 2*(x**3) + x + 1)
