#load "MathNet.Symbolics.fsx"

open System
open System.Numerics
open Microsoft.FSharp
open MathNet.Numerics
open MathNet.Symbolics

open Operators

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


module ``Create number literal expressions with the Q suffix or the number function`` =

    // equivalent:
    number 3 // 3
    6Q/4 // 3/2

    // expressions are not comparable (NoComparison) to prevent errors,
    // but if the expressions are numbers we can use compareNumber:
    Numbers.compare 0Q 1Q // -1
    Numbers.compare 1Q 1Q // 0
    Numbers.compare 1Q 2Q // -1
    Numbers.compare 0Q (1Q/2Q) // -1
    Numbers.compare 1Q (1Q/2Q) // 1
    Numbers.compare (1Q/2Q) 0Q // 1
    Numbers.compare (1Q/2Q) 1Q // -1
    Numbers.compare 1Q PositiveInfinity // -1
    Numbers.compare 1Q NegativeInfinity // 1
    Numbers.compare PositiveInfinity 1Q // 1
    Numbers.compare NegativeInfinity 1Q // -1

    // The literal suffix is only needed if a number appears isolated,
    // otherwise F#'s excellent type inference does its work:
    3Q/2 // 3/2
    (3Q + 2)*4/6 // 10/3
    (3*a+4)**2 // (4 + 3*a)^2


module ``Expressions are always in simplified form`` =

    // readable output is F# interactive thanks to Text.format printer added in MathNet.Symbolics.fsx

    x + y // x + y
    y + x // x + y
    x + x // 2*x
    x + 2*x // 3*x
    x + x*2 // 3*x
    2*x + 3*x // 5*x
    a*x + 2*x // 2*x + a*x
    a*x + x*b // a*x + b*x
    b*x + x*a // a*x + b*x
    1 + x + y // 1 + x + y
    x + 1 + y // 1 + x + y
    x + y + 1 // 1 + x + y

    x*y // x*y
    y*x // x*y
    x**2*x // x^3
    x*y*x**2 // x^3*y
    y*x*y**2 // x*y^3
    2*x*y // 2*x*y
    x*2*y // 2*x*y
    x*y*2 // 2*x*y

    2*(a*b) // 2*a*b
    (a*b)*2 // 2*a*b
    a*b + a*b // 2*a*b
    a*b + b*a // 2*a*b

    a + b + c + a*b + a*c + b*c // a + b + a*b + c + a*c + b*c
    c*b + c*a + b*a + c + b + a // a + b + a*b + c + a*c + b*c

    a**2 + b**2 // a^2 + b^2
    b**2 + a**2 // a^2 + b^2

    a**2 + a**3 // a^2 + a^3
    a**3 + a**2 // a^2 + a^3

    a**2 * b**2 // a^2*b^2
    b**2 * a**2 // a^2*b^2

    (a+c)**2 + (a+b)**2 // (a + b)^2 + (a + c)^2
    (a+b)**2 + (a+c)**2 // (a + b)^2 + (a + c)^2

    (a+c)**2 * (a+b)**2 // (a + b)^2*(a + c)^2
    (a+b)**2 * (a+c)**2 // (a + b)^2*(a + c)^2

    (a+c) * (a+b) // (a + b)*(a + c)
    (a+b) * (a+c) // (a + b)*(a + c)

    (1+x)**2 + (1+x)**3 + (1+y)**2 // (1 + x)^2 + (1 + x)^3 + (1 + y)^2
    (1+x)**3 + (1+y)**2 + (1+x)**2 // (1 + x)^2 + (1 + x)^3 + (1 + y)^2
    (1+y)**2 + (1+x)**2 + (1+x)**3 // (1 + x)^2 + (1 + x)^3 + (1 + y)^2

    (a+b)*x // (a + b)*x
    (a+b)*x*y // (a + b)*x*y
    (a+b)*y*x // (a + b)*x*y
    (a+b)*(x*y) // (a + b)*x*y
    (a+b)*(y*x) // (a + b)*x*y

    x*x // x^2
    x*x**2*x**3 // x^6
    (x**2)**3 // x^6

    2*(x*y)*z**2 // 2*x*y*z^2
    1*x*y*z**2 // x*y*z^2
    2*x*y*z*z**2 // 2*x*y*z^3

    // There is no subtraction, negation or division in simplified expressions:
    // TODO: Print as text in a nicer way (Text module): i.e. x^(-1) -> 1/x, (-1)*x -> -x
    1 / x // x^(-1)
    -x // (-1)*x
    2 + 1/x - 1 // 1 + x^(-1)
    -(-x) // x
    1 / (1 / x) // x

    2*x*3 // 6*x
    -x*y/3 // (-1/3)*x*y

    ((x*y)**(1Q/2)*z**2)**2 // x*y*z^4
    (a/b/(c*a))*(c*d/a)/d // a^(-1)*b^(-1)
    a**(3Q/2)*a**(1Q/2) // a^2

    x + ln x // x + ln(x)
    x + ln (x+1) // x + ln(1 + x)
    2*abs x // 2*|x|
    x + abs (-x) // x + |x|
    abs (-3Q) // 3
    exp 0Q // 1


module ``Parse F# quotations`` =

    Quotations.parse <@ 3 @> // 3
    Quotations.parse <@ x @> // x
    Quotations.parse <@ fun x -> x @> // x
    Quotations.parse <@ 3/4 @> // 3/4
    Quotations.parse <@ fun x -> 3/x @> // 3*x^(-1)
    Quotations.parse <@ -x*y/3 @> // (-1/3)*x*y
    Quotations.parse <@ fun x y -> -x*y/3 @> // (-1/3)*x*y
    Quotations.parse <@ fun (x, y) -> -x*y/3 @> // (-1/3)*x*y


module ``Algebraic Expansion`` =

    // Auto-simplification does not expand expressions:
    (a+b)-(a+b) // a + b + (-1)*(a + b)
    (a+b)-(a+b) |> Algebraic.expand // 0
    2*(a+b)-(a+b) // a + b
    (a+b)-2*(a+b) |> Algebraic.expand // (-1)*a + (-1)*b

    (a*b)/(b*a) // 1
    (a*b)**2/(b*a) // a*b
    (a*b)/(b*a)**2 // a^(-1)*b^(-1)

    (a+b)/(b+a) // 1
    (a+b)**2/(b+a) // a + b
    (a+b)/(b+a)**2 // (a + b)^(-1)

    (x*(y+1)**(3Q/2)+1)*(x*(y+1)**(3Q/2)-1) // ((-1) + x*(1 + y)^(3/2))*(1 + x*(1 + y)^(3/2))
    (x*(y+1)**(3Q/2)+1)*(x*(y+1)**(3Q/2)-1) |> Algebraic.expand |> Algebraic.expand // (-1) + x^2 + 3*x^2*y + 3*x^2*y^2 + x^2*y^3
    sin(a*(x+y)) |> Algebraic.expand // sin(a*(x + y)) - does not expand
    a/(b*(x+y)) |> Algebraic.expand // a*b^(-1)*(x + y)^(-1) - does not expand


module ``There are various algebaric operators available`` =

    Structure.substitute 3Q 4Q (x**3) // x^4
    Structure.map (fun x -> -x) (x + y**2) // (-1)*x + (-1)*y^2
    negate (x + y**2) // (-1)*(x + y^2)

    Algebraic.separateFactors x (b*cos(x)*ln(d)*x) // (b*ln(d), x*cos(x))
    Algebraic.separateFactors x (c*x*sin(x)/2) // ((1/2)*c, x*sin(x))

    Algebraic.expand ((x+1)*(x+3)) // 3 + 4*x + x^2
    Algebraic.expand ((a+b)**2) // a^2 + 2*a*b + b^2
    Algebraic.expand ((a+b)**3) // a^3 + 3*a^2*b + 3*a*b^2 + b^3
    Algebraic.expand ((a+b)**4) // a^4 + 4*a^3*b + 6*a^2*b^2 + 4*a*b^3 + b^4
    Algebraic.expand ((a+b+c)**2) // a^2 + 2*a*b + b^2 + 2*a*c + 2*b*c + c^2

    Algebraic.expandMain (x*(2+(1+x)**2)) // 2*x + x*(1 + x)^2
    Algebraic.expandMain ((x+(1+x)**2)**2) // x^2 + 2*x*(1 + x)^2 + (1 + x)^4

    Algebraic.expand ((a*x**2 + b*x + c)/(d*x + e)) // c*(e + d*x)^(-1) + b*x*(e + d*x)^(-1) + a*x^2*(e + d*x)^(-1)
    let p = Algebraic.expand ((a*x**2 + b*x + c)*(d*x**2 + e*x + f)) // c*f + c*e*x + b*f*x + c*d*x^2 + b*e*x^2 + a*f*x^2 + b*d*x^3 + a*e*x^3 + a*d*x^4
    Polynomial.coefficients x p // c*f; c*e + b*f; c*d + b*e + a*f; b*d + a*e; a*d
    Polynomial.leadingCoefficient x p // a*d
    Polynomial.collectTerms x p // c*f + (c*e + b*f)*x + (c*d + b*e + a*f)*x^2 + (b*d + a*e)*x^3 + a*d*x^4
    Polynomial.degree x p // 4
    Polynomial.totalDegree p // 6
    Polynomial.variables p // a; b; c; d; e; f; x

    Exponential.expand (exp(2*x+y)) // exp(x)^2*exp(y)
    Exponential.expand (exp(2*a*x + 3*y*z)) // exp(a*x)^2*exp(y*z)^3
    Exponential.expand (exp(2*(x+y))) // exp(x)^2*exp(y)^2
    Exponential.expand (1/(exp(2*x) - (exp(x))**2)) // ComplexInfinity
    Exponential.expand (exp((x+y)*(x-y))) // exp(x^2)*exp(y^2)^(-1)
    Exponential.expand (ln((c*x)**a) + ln(y**b*z)) // a*ln(c) + a*ln(x) + b*ln(y) + ln(z)

    Exponential.contract (exp(x)*exp(y)) // exp(x + y)
    Exponential.contract (exp(x)**a) // exp(a*x)
    Exponential.contract (exp(x)*(exp(x) + exp(y))) // exp(2*x) + exp(x + y)
    Exponential.contract ((exp(exp(x)))**exp(y)) // exp(exp(x + y))

    Exponential.simplify (1/(exp(x)*(exp(y)+exp(-x))) - (exp(x+y)-1)/((exp(x+y))**2-1)) // 0

    Trigonometric.expand (sin(2*x)) // 2*sin(x)*cos(x)
    Trigonometric.expand (sin(a+x)) // sin(x)*cos(a) + sin(a)*cos(x)
    Trigonometric.expand (sin(2*x + 3*y)) // ((-1)*sin(x)^2 + cos(x)^2)*((-1)*sin(y)^3 + 3*sin(y)*cos(y)^2) + 2*sin(x)*cos(x)*((-3)*sin(y)^2*cos(y) + cos(y)^3)
    Trigonometric.expand (sin(2*(x+y))) // 2*sin(y)*((-1)*sin(x)^2 + cos(x)^2)*cos(y) + 2*sin(x)*cos(x)*((-1)*sin(y)^2 + cos(y)^2)
    |> Algebraic.expand // (-2)*sin(x)*sin(y)^2*cos(x) + (-2)*sin(x)^2*sin(y)*cos(y) + 2*sin(y)*cos(x)^2*cos(y) + 2*sin(x)*cos(x)*cos(y)^2
    Trigonometric.expand (cos(5*x)) // 5*sin(x)^4*cos(x) + (-10)*sin(x)^2*cos(x)^3 + cos(x)^5
    Trigonometric.expand ((sin(2*x)-2*sin(x)*cos(x))/((sin(x))**2 + (cos(x))**2 - 1)) // 0 - should actually be Undefined

    Trigonometric.contract (sin(a)*sin(b)) // (-1/2)*cos(a + b) + (1/2)*cos(a + (-1)*b)
    Trigonometric.contract ((sin(x) + cos(y))*cos(y)) // 1/2 + (1/2)*sin(x + y) + (1/2)*sin(x + (-1)*y) + (1/2)*cos(2*y)
    Trigonometric.contract (sin(x)**2*cos(x)**2) // 1/8 + (-1/8)*cos(4*x)
    Trigonometric.contract (cos(x)**4) // 3/8 + (1/2)*cos(2*x) + (1/8)*cos(4*x)

    Trigonometric.simplify ((cos(x)+sin(x))**4 + (cos(x)-sin(x))**4 + cos(4*x) - 3) // 0

    // TODO:
    // expected: 0
    // actual: sin(y) + (-1/2)*sin(x + (-1)*y) + (-1/2)*sin((1/2)*x + (-1/2)*y + (-1)*((1/2)*x + (-1/2)*y)) + (-1/2)*sin((-1/2)*x + (1/2)*y + (-1)*((1/2)*x + (-1/2)*y)) + (-1)*sin((1/2)*x + (1/2)*y + (-1)*((1/2)*x + (-1/2)*y))
    Trigonometric.simplify (sin(x) + sin(y) - 2*sin(x/2+y/2)*cos(x/2-y/2))


module ``Differentiation and Taylor Series`` =

    Calculus.differentiate x (a*x) // a
    Calculus.differentiate x (sin(x)) // cos(x)
    Calculus.differentiate x (x*sin(x)) // sin(x) + x*cos(x)
    Calculus.differentiate x (a*x**2) // 2*a*x
    Calculus.differentiate x (a*x**b) // a*b*x^((-1) + b)
    Calculus.differentiate x (a*x**2 + b*x + c) // b + 2*a*x

    /// Taylor expansion of x(symbol) at symbol=a of the first k terms
    let taylor1 (k:int) symbol x a =
        let rec taylor n nf acc dxn =
            if n = k then acc else
            taylor (n+1) (nf*(n+1)) (acc + (dxn |> Structure.substitute symbol a)/nf*(symbol-a)**n) (Calculus.differentiate symbol dxn)
        taylor 0 1 zero x |> Algebraic.expand

    taylor1 3 x (1/(1-x)) 0Q // 1 + x + x^2
    taylor1 3 x (1/x) 1Q // 3 + (-3)*x + x^2
    taylor1 3 x (ln(x)) 1Q // -3/2 + 2*x + (-1/2)*x^2
    taylor1 4 x (ln(x)) 1Q // -11/6 + 3*x + (-3/2)*x^2 + (1/3)*x^3
    taylor1 3 x (sin(x)+cos(x)) 0Q // 1 + x + (-1/2)*x^2
    taylor1 4 x (sin(x)+cos(x)) 0Q // 1 + x + (-1/2)*x^2 + (-1/6)*x^3


module ``Polynomial Division`` =

    Polynomial.divide x (5*x**2 + 4*x + 1) (2*x + 3) // (-7/4 + (5/2)*x, 25/4)
    Polynomial.divide x (x**3 - 2*x**2 - 4) (x-3) // (3 + x + x^2, 5)
    Polynomial.quot x (x**3 - 2*x**2 - 4) (x-3) // 3 + x + x^2
    Polynomial.remainder x (x**3 - 2*x**2 - 4) (x-3) // 5

    Polynomial.divide x (3*x**3 + x**2 + x + 5) (5*x**2 - 3*x + 1) // (14/25 + (3/5)*x, 111/25 + (52/25)*x)
    Polynomial.divide x (3*x**3 + x**2 + x + 5) (2Q) // (5/2 + (1/2)*x + (1/2)*x^2 + (3/2)*x^3, 0)
    Polynomial.pseudoDivide x (3*x**3 + x**2 + x + 5) (5*x**2 - 3*x + 1) // (14 + 15*x, 111 + 52*x, 25)
    Polynomial.pseudoDivide x (3*x**3 + x**2 + x + 5) (2Q) // (5 + x + x^2 + 3*x^3, 0, 2)

    // tangent of polynomial at x = 1?
    Polynomial.divide x (x**3 - 12*x**2 - c) (x**2-2*x+1) // ((-10) + x, 10 + (-1)*c + (-21)*x)

    /// Find tangent equation for x(symbol) at symbol=a
    let tangent symbol x a =
        let m = Calculus.differentiate symbol x |> Structure.substitute symbol a
        m*(symbol - a) + Structure.substitute symbol a x |> Algebraic.expand

    tangent x (x**3 - 12*x**2 - c) 1Q // 10 + (-1)*c + (-21)*x
    tangent z (1/z) 3Q // 2/3 + (-1/9)*z


module ``Polynomial Expansion`` =

    // 1 + x + (2 + x)*y + (3 + x)*y^2
    let ex = Polynomial.polynomialExpansion x y (x**5 + 11*x**4 + 51*x**3 + 124*x**2 + 159*x + 86) (x**2 + 4*x + 5)
    // 1 + x + (2 + x)*(5 + 4*x + x^2) + (3 + x)*(5 + 4*x + x^2)^2
    let exs = ex |> Structure.substitute y (x**2 + 4*x + 5)
    // get back to original polynomial:
    // 86 + 159*x + 124*x^2 + 51*x^3 + 11*x^4 + x^5
    Algebraic.expand exs


module ``Polynomial GCD`` =

    // 4 + (-4)*x + (-1)*x^2 + x^3
    Polynomial.gcd x (x**7 - 4*x**5 - x**2 + 4) (x**5 - 4*x**3 - x**2 + 4)

    // (4 + (-4)*x + (-1)*x^2 + x^3, (-1)*x, 1 + x^3)
    Polynomial.extendedGcd x (x**7 - 4*x**5 - x**2 + 4) (x**5 - 4*x**3 - x**2 + 4)
    // verify A*u+B*v = gcd = 4 - 4*x - x^2 + x^3:
    // 4 + (-4)*x + (-1)*x^2 + x^3
    (-x)*(x**7 - 4*x**5 - x**2 + 4) + (1+x**3)*(x**5 - 4*x**3 - x**2 + 4) |> Algebraic.expand

    Polynomial.gcd x (x**4 - 2*x**3 - 6*x**2 + 12*x + 15) (x**3 + x**2 - 4*x - 4) // 1 + x
    Polynomial.halfExtendedGcd x (x**4 - 2*x**3 - 6*x**2 + 12*x + 15) (x**3 + x**2 - 4*x - 4) // (1 + x, 3/5 + (-1/5)*x)
    Polynomial.extendedGcd x (x**4 - 2*x**3 - 6*x**2 + 12*x + 15) (x**3 + x**2 - 4*x - 4) // (1 + x, 3/5 + (-1/5)*x, 2 + (-6/5)*x + (1/5)*x^2)


module ``Evaluate some expression to floating point numbers`` =

    open FloatingPoint

    let symbols = Map.ofList ["a", freal 2.0; "b", freal 3.0; "c", fcomplex 1.0 -1.0]
    evaluate symbols (a) // Real 2.0
    evaluate symbols (1Q/2) // Real 0.5
    evaluate symbols (sin(a) + ln(b)) // Real 2.007909715
    evaluate symbols (a*x**2 + b*x + c |> Structure.substitute x (number 1/2)) // Complex (3, -1)
    evaluate symbols (1Q/0Q) // ComplexInf


module ``Primitive Equation Solver`` =

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
    solve x (sin(a)+x*cos(b)+c) // (-1)*(c + sin(a))*cos(b)^(-1)

    // (x^2-1)/(x+1) = 0 --> x =
    solve x ((x**2-1)/(x+1)) // 1

    /// Solve simple a=b line equations to y=f(x) form
    let solveLine x y a b =
        let z = solve y (a-b) |> Algebraic.expand |> Rational.simplify x
        let z' = z |> Algebraic.expand |> Polynomial.collectTerms x
        if z' <> Undefined then z' else z

    solveLine x y (x/2+y/3) 1Q // 3 + (-3/2)*x   -->  x/2 + y/3 = 1  ->  y = -3/2*x + 3
    solveLine x y (x/a) ((x+y)/b) // ((-1) + a^(-1)*b)*x
    solveLine x y ((y/x-2)/(1-3/x)) 6Q // (-18) + 8*x


module ``General Polynomial Expressions`` =

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
    coefficient x 1 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 7 + 3*y^2
    coefficient x 3 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 0
    leadingCoefficient x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 5*y + 7*y^3
    coefficients x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 9; 3*y^2; 5*y + 7*y^3

    collectTermsMonomial x (2*x*a) // (2*a, x)
    collectTermsMonomial x (2*a*x*b*3) // (6*a*b, x)
    collectTermsMonomial x (2*a*x**3*b*x*3) // (6*a*b, x^4)

    collectTerms x (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) // x*(4*a + 5*b + 2*a*y + 3*b*y)
    collectTerms a (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) // 5*b*x + 3*b*x*y + a*(4*x + 2*x*y)
    collectTerms (ln(a)) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c) // c + 5*b*x + 3*b*x*y + (4*x + 2*x*y)*ln(a)

    isMonomialMV (symbols [x;y]) (a * x**2 * y**2) // true
    isMonomialMV (symbols [x;y]) (ln(a) * x**2 * y**2) // true
    isMonomialMV (symbols [x;y]) (x**2 + y**2) // false
    isPolynomialMV (symbols [x;y]) (x**2 + y**2) // true
    isPolynomialMV (symbols [x+1]) ((x+1)**2 + 2*(x+1)) // true
    isPolynomialMV (symbols [x]) ((x+1)*(x+3)) // false

    degreeMonomialMV (symbols [x;y]) (a * x**2 * y * b**2) // 3 - (x:2 + y:1)
    degreeMV (symbols [x;y]) (a*x**2 + b*x + c) // 2
    degreeMV (symbols [x;z]) (2*x**2*y**8*z**2 + a*x*z**6) // 7

    variables (a * x**2 * y**2) // a; x; y
    variables ((x+1)**2 + 2*(x+1)) // 1 + x
    variables ((x+1)*(x+3)) // 1 + x; 3 + x
    variables ((x+1)*(x+3)*sin(x)) // 1 + x; 3 + x; sin(x)
    totalDegree (2*x**2*y*z**2 + a*x*z**6) // 8

    coefficient x 2 (a*x**2 + b*x + c) // a
    coefficient x 2 (a*x*x + b*x + c) // a
    coefficient x 1 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 7 + 3*y^2
    coefficient x 3 (3*x*y**2 + 5*x**2*y + 7*x + 9) // 0
    leadingCoefficient x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 5*y + 7*y^3
    coefficients x (3*x*y**2 + 5*x**2*y + 7*x**2*y**3 + 9) // 9; 3*y^2; 5*y + 7*y^3

    collectTermsMonomialMV (symbols [x;y]) (2*x*a) // (2*a, x)
    collectTermsMonomialMV (symbols [x;y]) (2*a*x*b*y*3) // (6*a*b, x*y)
    collectTermsMonomialMV (symbols [x;y]) (2*a*x*b*y**3*x*3) // (6*a*b, x^2*y^3)

    collectTermsMV (symbols [x;y]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) // (4*a + 5*b)*x + (2*a + 3*b)*x*y
    collectTermsMV (symbols [a;b]) (2*x*a*y + 4*a*x + 3*x*y*b + 5*x*b) // a*(4*x + 2*x*y) + b*(5*x + 3*x*y)
    collectTermsMV (symbols [x;ln(a)]) (2*x*ln(a)*y + 4*x*ln(a) + 3*x*y*b + 5*x*b + c) // c + x*(5*b + 3*b*y) + x*(4 + 2*y)*ln(a)


module ``General Rational Expressions`` =

    open Rational

    numerator (x/y) // x
    denominator (x/y) // y
    numerator (x**2/y**3) // x^2
    denominator (x**2/y**3) // y^3

    numerator (x**2) // x^2
    denominator (x**2) // 1
    numerator (x**(-2)) // 1
    denominator (x**(-2)) // x^2

    numerator (2Q/3*(x*(x+1))/(x+2)*y**a) // 2*x*(1 + x)*y^a
    denominator (2Q/3*(x*(x+1))/(x+2)*y**a) // 3*(2 + x)

    isRational x ((x**2+1)/(2*x+3)) // true
    isRational x (1/x + 1/a) // false

    variables ((2*x + 3*y)/(z + 4)) // x; y; z
    variables (1/x + 1/y) // x^(-1); y^(-1)
    variables (a/x + b/y) // a; x^(-1); b; y^(-1)

    rationalize (a+1) // 1 + a
    rationalize (a/b + c/d) // b^(-1)*d^(-1)*(b*c + a*d)
    rationalize (1+1/(1+1/x)) // (1 + x)^(-1)*(1 + 2*x)
    rationalize (1/(1+1/x)**(1Q/2) + (1+1/x)**(3Q/2)) // x^(-2)*(x^(-1)*(1 + x))^(-1/2)*(x^2 + (1 + x)^2)
    rationalize ((1+1/x)**2) // x^(-2)*(1 + x)^2

    rationalize (a/b + c/d + e/f) // b^(-1)*d^(-1)*f^(-1)*(b*d*e + (b*c + a*d)*f)
    expand (a/b + c/d + e/f) // b^(-1)*d^(-1)*f^(-1)*(b*d*e + b*c*f + a*d*f)

    // (1 + x)^(-1)*((-1) + ((1 + (x + y)^2)^(-1))^(1/2))*(1 + ((1 + (x + y)^2)^(-1))^(1/2))
    rationalize (((1/((x+y)**2+1))**(1Q/2)+1)*((1/((x+y)**2+1))**(1Q/2)-1)/(x+1))
    // ((-1)*x^2 + (-2)*x*y + (-1)*y^2)*(1 + x + x^2 + x^3 + 2*x*y + 2*x^2*y + y^2 + x*y^2)^(-1)
    expand (((1/((x+y)**2+1))**(1Q/2)+1)*((1/((x+y)**2+1))**(1Q/2)-1)/(x+1))

    rationalize (1/(1/a + c/(a*b)) + (a*b*c + a*c**2)/(b+c)**2-a) |> Algebraic.expand // 0
    expand (1/(1/a + c/(a*b)) + (a*b*c + a*c**2)/(b+c)**2-a) // 0

    rationalize (x/z + y/z**2) // z^(-3)*(y*z + x*z^2)
    simplify z (x/z + y/z**2) // z^(-2)*(y + x*z)

    simplify x ((x**2-1)/(x+1)) // (-1) + x
    simplify x ((x+1)/(x**2 - 1 - (x+1)*(x-1))) // ComplexInfinity
    simplify x (1/(1+1/(x+1)) + 2/(x+2))  // (2 + x)^(-1)*(3 + x)


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


module ``Exponentional and Trigonometric Functions`` =

    sin x // sin(x)
    cot x // tan(x)^(-1)
    sec x // cos(x)^(-1)
