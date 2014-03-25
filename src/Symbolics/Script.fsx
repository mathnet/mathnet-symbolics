#load "MathNet.Symbolics.fsx"

open System
open System.Numerics
open Microsoft.FSharp
open MathNet.Numerics
open MathNet.Symbolics

let x = symbol "x"
let y = symbol "y"

-x*y/(number 3)

number 1 / x
number 2 + number 1 / x - number 1
number 2 * x * number 3

(x ** number 2) ** number 3

Elementary.substitute (number 3) (number 4) (x ** number 3)
Elementary.map (fun x -> -x) (x + y**number 2)
Elementary.negate (x + y**number 2)

Elementary.numerator (x/y)
Elementary.denominator (x/y)
Elementary.numerator (x**number 2/y**number 3)
Elementary.denominator (x**number 2/y**number 3)

Elementary.numerator (x**(number 2))
Elementary.denominator (x**(number 2))
Elementary.numerator (x**(number -2))
Elementary.denominator (x**(number -2))

Quotations.parse <@ 3 @>
Quotations.parse <@ x @>
Quotations.parse <@ fun x -> x @>
Quotations.parse <@ 3/4 @>
Quotations.parse <@ fun x -> 3/x @>
Quotations.parse <@ fun x y -> -x*y/3 @>
Quotations.parse <@ fun (x, y) -> -x*y/3 @>

Polynomials.isMonomial x <| Quotations.parse <@ fun x -> 3*x @>
Polynomials.isMonomial x <| Quotations.parse <@ fun x -> 3*x+2 @>
Polynomials.isMonomial x <| Quotations.parse <@ fun x -> 3*(x*x) @>
Polynomials.isMonomial y <| Quotations.parse <@ fun x -> 3*x @>
Polynomials.degreeMonomial x (number 0)
Polynomials.degreeMonomial x (number 1)
Polynomials.degreeMonomial x (number 3 * x)
Polynomials.degreeMonomial x (number 3 * x*x)
Polynomials.degreeMonomial x (number 3 * x*x * y) // undefined
Polynomials.degreeMonomial x (number 3 + x) // undefined
Polynomials.coefficientMonomial x (number 0)
Polynomials.coefficientMonomial x (number 1)
Polynomials.coefficientMonomial x (number 3 * x)
Polynomials.coefficientMonomial x (number 3 * x*x)
Polynomials.coefficientMonomial x (number 3 * x*x * y) // undefined
Polynomials.coefficientMonomial x (number 3 + x) // undefined
Polynomials.coefficientDegreeMonomial x (number 0)
Polynomials.coefficientDegreeMonomial x (number 1)
Polynomials.coefficientDegreeMonomial x (number 3 * x)
Polynomials.coefficientDegreeMonomial x (number 3 * x*x)

Polynomials.isPolynomial x <| Quotations.parse <@ fun x -> 3*x @>
Polynomials.isPolynomial x <| Quotations.parse <@ fun x -> 3*x+2 @>
Polynomials.isPolynomial x <| Quotations.parse <@ fun x -> 3*x*x+2 @>
Polynomials.degree x (number 3*x*x + number 2*x)
Polynomials.degree x (number 3*x*x + number 2*x*x*x)
Polynomials.degree x (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3))
Polynomials.coefficient x 0 (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.coefficient x 1 (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.coefficient x 2 (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.coefficient x 3 (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.coefficient x 4 (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.coefficient x 5 (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.coefficient x 6 (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.coefficient x 7 (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.leadingCoefficient x (number 3*x*x + number 2*x)
Polynomials.leadingCoefficient x (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.leadingCoefficient x (number 2)
Polynomials.leadingCoefficient x (number 0)
Polynomials.leadingCoefficientDegree x (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
Polynomials.coefficients x (number 3*x*x + number 2*x)
Polynomials.coefficients x (number 3*x + number 2*x*(x**number 5) + number 2*(x**number 3) + x + number 1)
