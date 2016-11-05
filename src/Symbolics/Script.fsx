#load @"..\..\packages\MathNet.Numerics.FSharp\MathNet.Numerics.fsx"

//#load "MathNet.Symbolics.fsx"
#I @"..\..\out\lib\Net40"
#r "MathNet.Numerics.dll"
#r "MathNet.Symbolics.dll"

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

open Operators

fsi.AddPrinter Infix.format

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


// This script is just for local experiments
// For examples see Tests.fs in the UnitTest project which is quite readable

(3 + 2)*4/6

(3Q + 2)*4/6 // 10/3
(3*a+4)**2 // (4 + 3*a)^2
-x*y/3 // -(1/3)*x*y


1 / x // 1/x
-x // -x
2 + 1/x - 1 // 1 + 1/x
-(-x) // x
1 / (1 / x) // x
(a/b/(c*a))*(c*d/a)/d // 1/(a*b)


(8*a*x + 6*a*x**2)/(4*x*a)
(8*a*x + 6*a*x**2)/(4*x*a) |> Rational.reduce // (1/2)*(4 + 3*x)
(8*a*x + 6*a*x**2)/(4*x*a) |> Rational.expand // 2 + (3/2)*x
(8*a*x + 6*a*x**2)/(4*x*a) |> Rational.simplify x // 2 + (3/2)*x
(8*a*x + 6*a*x**2)/(4*x*a) |> Rational.rationalize // ((1/4)*(8*a*x + 6*a*x^2))/(a*x)
(8*a*x + 6*a*x**2)/(4*x*a) |> Rational.expand |> Rational.rationalize // (1/2)*(4 + 3*x)

Polynomial.commonFactors (8*a*x + 6*a*x**2) // ==> "2*a*x"


Algebraic.separateFactors x (b*cos(x)*ln(d)*x) // (b*ln(d), x*cos(x))
Algebraic.separateFactors x (c*x*sin(x)/2) // ((1/2)*c, x*sin(x))

Exponential.expand (exp(2*x+y)) // exp(x)^2*exp(y)
Exponential.expand (exp(2*a*x + 3*y*z)) // exp(a*x)^2*exp(y*z)^3
Exponential.expand (exp(2*(x+y))) // exp(x)^2*exp(y)^2
Exponential.expand (1/(exp(2*x) - (exp(x))**2)) // ComplexInfinity
Exponential.expand (exp((x+y)*(x-y))) // exp(x^2)/exp(y^2)
Exponential.expand (ln((c*x)**a) + ln(y**b*z)) // a*ln(c) + a*ln(x) + b*ln(y) + ln(z)

Exponential.contract (exp(x)*exp(y)) // exp(x + y)
Exponential.contract (exp(x)**a) // exp(a*x)
Exponential.contract (exp(x)*(exp(x) + exp(y))) // exp(2*x) + exp(x + y)
Exponential.contract ((exp(exp(x)))**exp(y)) // exp(exp(x + y))

Exponential.simplify (1/(exp(x)*(exp(y)+exp(-x))) - (exp(x+y)-1)/((exp(x+y))**2-1)) // 0

Trigonometric.expand (sin(2*x)) // 2*sin(x)*cos(x)
Trigonometric.expand (sin(a+x)) // sin(x)*cos(a) + sin(a)*cos(x)
Trigonometric.expand (sin(2*x + 3*y)) // (-sin(x)^2 + cos(x)^2)*(-sin(y)^3 + 3*sin(y)*cos(y)^2) + 2*sin(x)*cos(x)*(-3*sin(y)^2*cos(y) + cos(y)^3)
Trigonometric.expand (sin(2*(x+y))) // 2*sin(y)*(-sin(x)^2 + cos(x)^2)*cos(y) + 2*sin(x)*cos(x)*(-sin(y)^2 + cos(y)^2)
|> Algebraic.expand // -2*sin(x)*sin(y)^2*cos(x) - 2*sin(x)^2*sin(y)*cos(y) + 2*sin(y)*cos(x)^2*cos(y) + 2*sin(x)*cos(x)*cos(y)^2
Trigonometric.expand (cos(5*x)) // 5*sin(x)^4*cos(x) - 10*sin(x)^2*cos(x)^3 + cos(x)^5

// TODO: expected Undefined
Trigonometric.expand ((sin(2*x)-2*sin(x)*cos(x))/((sin(x))**2 + (cos(x))**2 - 1)) // 0

Trigonometric.contract (sin(a)*sin(b)) //-(1/2)*cos(a + b) + (1/2)*cos(a - b)
Trigonometric.contract ((sin(x) + cos(y))*cos(y)) // 1/2 + (1/2)*sin(x + y) + (1/2)*sin(x - y) + (1/2)*cos(2*y)
Trigonometric.contract (sin(x)**2*cos(x)**2) // 1/8 - (1/8)*cos(4*x)
Trigonometric.contract (cos(x)**4) // 3/8 + (1/2)*cos(2*x) + (1/8)*cos(4*x)

Trigonometric.simplify ((cos(x)+sin(x))**4 + (cos(x)-sin(x))**4 + cos(4*x) - 3) // 0

// TODO: expected: 0
// actual: sin(y) - (1/2)*sin(x - y) - (1/2)*sin((1/2)*x - (1/2)*y - ((1/2)*x - (1/2)*y)) - (1/2)*sin(-(1/2)*x + (1/2)*y - ((1/2)*x - (1/2)*y)) - sin((1/2)*x + (1/2)*y - ((1/2)*x - (1/2)*y))
Trigonometric.simplify (sin(x) + sin(y) - 2*sin(x/2+y/2)*cos(x/2-y/2))



/// Taylor expansion of x(symbol) at symbol=a of the first k terms
let taylor (k:int) symbol x a =
    let rec impl n nf acc dxn =
        if n = k then acc else
        impl (n+1) (nf*(n+1)) (acc + (dxn |> Structure.substitute symbol a)/nf*(symbol-a)**n) (Calculus.differentiate symbol dxn)
    impl 0 1 zero x |> Algebraic.expand

taylor 4 x (sin(x)+cos(x)) 0Q // 1 + x - (1/2)*x^2 - (1/6)*x^3
(sin(x)+cos(x)) |> Calculus.taylor 4 x 0Q // 1 + x - (1/2)*x^2 - (1/6)*x^3


Polynomial.divide x (5*x**2 + 4*x + 1) (2*x + 3) // (-7/4 + (5/2)*x, 25/4)
Polynomial.divide x (x**3 - 2*x**2 - 4) (x-3) // (3 + x + x^2, 5)
Polynomial.quot x (x**3 - 2*x**2 - 4) (x-3) // 3 + x + x^2
Polynomial.remainder x (x**3 - 2*x**2 - 4) (x-3) // 5

Polynomial.divide x (3*x**3 + x**2 + x + 5) (5*x**2 - 3*x + 1) // (14/25 + (3/5)*x, 111/25 + (52/25)*x)
Polynomial.divide x (3*x**3 + x**2 + x + 5) (2Q) // (5/2 + (1/2)*x + (1/2)*x^2 + (3/2)*x^3, 0)
Polynomial.pseudoDivide x (3*x**3 + x**2 + x + 5) (5*x**2 - 3*x + 1) // (14 + 15*x, 111 + 52*x, 25)
Polynomial.pseudoDivide x (3*x**3 + x**2 + x + 5) (2Q) // (5 + x + x^2 + 3*x^3, 0, 2)

// tangent of polynomial at x = 1?
Polynomial.divide x (x**3 - 12*x**2 - c) (x**2-2*x+1) // (-10 + x, 10 - c - 21*x)

/// Find tangent equation for x(symbol) at symbol=a
let tangent symbol x a =
    let m = Calculus.differentiate symbol x |> Structure.substitute symbol a
    m*(symbol - a) + Structure.substitute symbol a x |> Algebraic.expand

tangent x (x**3 - 12*x**2 - c) 1Q // 10 - c - 21*x
tangent z (1/z) 3Q // 2/3 - (1/9)*z
1/z |> Calculus.tangentLine z 3Q // 2/3 - (1/9)*z

// 1 + x + (2 + x)*y + (3 + x)*y^2
let ex = Polynomial.polynomialExpansion x y (x**5 + 11*x**4 + 51*x**3 + 124*x**2 + 159*x + 86) (x**2 + 4*x + 5)
// 1 + x + (2 + x)*(5 + 4*x + x^2) + (3 + x)*(5 + 4*x + x^2)^2
let exs = ex |> Structure.substitute y (x**2 + 4*x + 5)
// get back to original polynomial:
// 86 + 159*x + 124*x^2 + 51*x^3 + 11*x^4 + x^5
Algebraic.expand exs

// 4 - 4*x - x^2 + x^3
Polynomial.gcd x (x**7 - 4*x**5 - x**2 + 4) (x**5 - 4*x**3 - x**2 + 4)

// (4 - 4*x - x^2 + x^3, -x, 1 + x^3)
Polynomial.extendedGcd x (x**7 - 4*x**5 - x**2 + 4) (x**5 - 4*x**3 - x**2 + 4)
// verify A*u+B*v = gcd = 4 - 4*x - x^2 + x^3:
// 4 - 4*x - x^2 + x^3
(-x)*(x**7 - 4*x**5 - x**2 + 4) + (1+x**3)*(x**5 - 4*x**3 - x**2 + 4) |> Algebraic.expand


let u = x**4 - 2*x**3 - 6*x**2 + 12*x + 15
let v = x**3 + x**2 - 4*x - 4
Polynomial.gcd x u v // 1 + x
Polynomial.halfExtendedGcd x u v // (1 + x, 3/5 - (1/5)*x)

let g,a',b' = Polynomial.extendedGcd x u v
// val g : Expression = 1 + x
// val b : Expression = 2 - (6/5)*x + (1/5)*x^2
// val a : Expression = 3/5 - (1/5)*x
// hence u*a + v*b = g ? indeed:
u*a' + v*b' |> Algebraic.expand // 1 + x

// Let's try to find s, t such that s*u + t*v = x^2 - 1
let s, t = Polynomial.diophantineGcd x (x**4 - 2*x**3 - 6*x**2 + 12*x + 15) (x**3 + x**2 - 4*x - 4) (x**2 - 1)
// val t : Expression = -2 + (16/5)*x - (7/5)*x^2 + (1/5)*x^3
// val s : Expression = -3/5 + (4/5)*x - (1/5)*x^2
s*u + t*v |> Algebraic.expand // -1 + x^2

// (x^2 + 3*x)/((x + 1)*(x^2 - 2*x + 1)) --> (-1/2)/(x+1) + (1/2 + (3/2)*x)/(x^2-2*x+1)
Polynomial.partialFraction x (x**2+3*x) [x+1; x**2-2*x+1] // (0, [-1/2; 1/2 + (3/2)*x])




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
solve x (2+3*x) // -2/3

// sin(a)+x*cos(b)+c = 0 --> x =
solve x (sin(a)+x*cos(b)+c) // -(c + sin(a))/cos(b)

// (x^2-1)/(x+1) = 0 --> x =
solve x ((x**2-1)/(x+1)) // 1

/// Solve simple a=b line equations to y=f(x) form
let solveLine x y a b =
    let z = solve y (a-b) |> Algebraic.expand |> Rational.simplify x
    let z' = z |> Algebraic.expand |> Polynomial.collectTerms x
    if z' <> Undefined then z' else z

solveLine x y (x/2+y/3) 1Q // 3 - (3/2)*x   -->  x/2 + y/3 = 1  ->  y = -3/2*x + 3
solveLine x y (x/a) ((x+y)/b) // (-1 + b/a)*x
solveLine x y ((y/x-2)/(1-3/x)) 6Q // -18 + 8*x

Rational.rationalize (a+1) // 1 + a
Rational.rationalize (a/b + c/d) // (b*c + a*d)/(b*d)
Rational.rationalize (1+1/(1+1/x)) // (1 + 2*x)/(1 + x)
Rational.rationalize (1/(1+1/x)**(1Q/2) + (1+1/x)**(3Q/2)) // (x^2 + (1 + x)^2)/(x^2*((1 + x)/x)^(1/2))
Rational.rationalize ((1+1/x)**2) // (1 + x)^2/x^2

Rational.rationalize (a/b + c/d + e/f) // (b*d*e + (b*c + a*d)*f)/(b*d*f)
Rational.expand (a/b + c/d + e/f) // (b*d*e + b*c*f + a*d*f)/(b*d*f)

// ((-1 + (1/(1 + (x + y)^2))^(1/2))*(1 + (1/(1 + (x + y)^2))^(1/2)))/(1 + x)
Rational.rationalize (((1/((x+y)**2+1))**(1Q/2)+1)*((1/((x+y)**2+1))**(1Q/2)-1)/(x+1))
// (-x^2 - 2*x*y - y^2)/(1 + x + x^2 + x^3 + 2*x*y + 2*x^2*y + y^2 + x*y^2)
Rational.expand (((1/((x+y)**2+1))**(1Q/2)+1)*((1/((x+y)**2+1))**(1Q/2)-1)/(x+1))

Rational.rationalize (1/(1/a + c/(a*b)) + (a*b*c + a*c**2)/(b+c)**2-a) |> Algebraic.expand // 0
Rational.expand (1/(1/a + c/(a*b)) + (a*b*c + a*c**2)/(b+c)**2-a) // 0

Rational.rationalize (x/z + y/z**2) // (y*z + x*z^2)/z^3
Rational.simplify z (x/z + y/z**2) // (y + x*z)/z^2

Rational.simplify x ((x**2-1)/(x+1)) // -1 + x
Rational.simplify x ((x+1)/(x**2 - 1 - (x+1)*(x-1))) // ComplexInfinity
Rational.simplify x (1/(1+1/(x+1)) + 2/(x+2))  // (3 + x)/(2 + x)



// Printing
Infix.format (1/(a*b))        // "1/(a*b)"
Infix.formatStrict (1/(a*b))  // "a^(-1)*b^(-1)"
LaTeX.format (1/(a*b))        // "\frac{1}{ab}"


// Parsing
Infix.parse "1/(a*b)"
Infix.parse "sin(x)"
Infix.parse "sin (x)"
Infix.parse "sin x" // ParseFailure, as it should
Infix.parse "sin"

Infix.parseOrUndefined "sin x - 1" // -1 + sin(x)
Infix.parseOrUndefined "|a-2|-1"
Infix.parseOrUndefined "x-" // undefined

Quotations.parse <@ fun x y -> -x*y/3 @>  // -(1/3)*x*y
