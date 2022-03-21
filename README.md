Math.NET Symbolics (With Matrix/Vector/Customized Function supported)
2022-03-21 The progress is really great. Critical lambda compilation and evaluation bugs are resolved.

```
    let a0 = SymbolicExpression(Infix.parseOrThrow("v * 2")).Evaluate(symbols2)
    printfn "%A" a0.RealVectorValue
    let a1 = SymbolicExpression(Infix.parseOrThrow("v + 1")).Evaluate(symbols2)
    printfn "%A" a1.RealVectorValue
    let a2 = SymbolicExpression(Infix.parseOrThrow("mat_by_row(v, v)")).Evaluate(symbols2)
    printfn "%A" a2.RealMatrixValue
    let a3 = SymbolicExpression(Infix.parseOrThrow("mat_by_col(v, v)")).Evaluate(symbols2)
    printfn "a3: %A" a3.RealMatrixValue
    let a4 = SymbolicExpression(Infix.parseOrThrow("mat_multiply(m, mat_by_col(v, vec(1.0,2.0,3.0), v), v)")).Evaluate(symbols2)
    printfn "a4: %A" a4

    cFun ("mat_by_row", []) |> ignore

    let symV = Symbol "v"
    let symW = Symbol "w"
    
    let symV1 = Symbol "v1"
    let symW1 = Symbol "w1"
    let symV2 = Symbol "v2"
    let symW2 = Symbol "w2"
    let symX = Symbol "x"
    let syml = dict [ "x", FloatingPoint.Real 9.0; ]
    let _ = define "t0" ([symV; symW], (v + w))
    let _ = define "t1" ([symV; symW], Infix.parseOrThrow("t0(v, w)"))
    let _ = define "t2" ([symV; symW], Infix.parseOrThrow("2 * t0(v, w) / 3"))

    let lambdaExp =
        try
            MathNet.Symbolics.Linq.formatLambda (cFun("t0", [x; x])) [symV; symW] //intensive error
        with
        | _ -> None

    printfn "t0: %A" <| SymbolicExpression(cFun("t0", [x; x])).Evaluate(syml)
    printfn "t0-2: %A" <| SymbolicExpression.Parse("1 + t0(x, x)").Evaluate(syml)
    printfn "2 * t1(x, t1(x, x)) / t1(2 * x, x) * 4: %A" <| SymbolicExpression.Parse("2 * t1(x, t1(x, x)) / t1(2 * x, x) * 4").Evaluate(syml)

    let infix2 = Infix.parseOrThrow("2 * t0(v1, w1) / 3")
    let lambdaExp2 =
        try
            MathNet.Symbolics.Linq.formatLambda infix2 [symV2; symW2]  //intensive error
        with
        | _ -> None

    let infix3_0 = Infix.parseOrThrow("t0(x, x)")
    let infix3_1 = Infix.parseOrThrow("t1(x, x)")
    let infix3_2 = Infix.parseOrThrow("t2(x, x * 2)")
    let infix3 = Infix.parseOrThrow("2 * t2(x, x) / 3 + t2(x, x * 2)")


    let (Some lambdaExp3_0) = MathNet.Symbolics.Linq.formatLambda infix3_0 [symX]
    let (Some lambdaExp3_2) = MathNet.Symbolics.Linq.formatLambda infix3_2 [symX]
    let (Some lambdaExp3) = MathNet.Symbolics.Linq.formatLambda infix3 [symX]

    let toEvaluate = SymbolicExpression.Parse("2 * t2(x, x) / 3 + t2(x, x * 2)")
    let (Some toLambda) = MathNet.Symbolics.Linq.formatLambda toEvaluate.Expression [symX]

    printfn "2 * t2(x, x) / 3 + t2(x, x * 2): %A" <| toEvaluate.Evaluate(syml)
    printfn "t1(x, 2 * t0(x,x)): %A" <| SymbolicExpression(cFun("t1", [x; 2 * cFun("t0", [x; x])])).Evaluate(syml)
    printfn "t1(x, 2 * t1(x,x)): %A" <| SymbolicExpression(cFun("t1", [x; 2 * cFun("t1", [x; x])])).Evaluate(syml)
    printfn "t0(x, t0(x, x) * 2): %A" <| SymbolicExpression(cFun("t0", [x; cFun("t0", [x; x]) * 2])).Evaluate(syml)
    printfn "t0(x, t1(x, x) * 2): %A" <| SymbolicExpression(cFun("t0", [x; cFun("t1", [x; x]) * 2])).Evaluate(syml)

    let a5 = SymbolicExpression(Infix.parseOrThrow("2 * mat_multiply(m, mat_by_col(v, vec(1.0,2.0,3.0), v), v)")).Evaluate(symbols2)
    printfn "a5: %A" a5

    let a6 = SymbolicExpression.Parse("2 * htensor(lo(lo(lo(vec(1,2,3), vec(4,5,6)), lo(vec(7,8,9), vec(10,11,12)))))").Evaluate(symbols2)
    printfn "a6:%A" a6

    let a7expr = SymbolicExpression.Parse("t0(1, 2 * htensor(lo(lo(lo(vec(1,2,3), vec(4,5,6)), lo(vec(7,8,9), vec(10,11,12))))))")
    let a7 = a7expr.Evaluate(symbols2)
    printfn "a7:%A" a7
```
Results:
```
seq [2.0; 4.0; 6.0]
seq [2.0; 3.0; 4.0]
DenseMatrix 2x3-Double
1  2  3
1  2  3

a3: DenseMatrix 3x2-Double
1  1
2  2
3  3

a4: RealVector (seq [18.0; 30.0; 84.0])
t0: Real 18.0
t0-2: Real 19.0
2 * t1(x, t1(x, x)) / t1(2 * x, x) * 4: Real 8.0
2 * t2(x, x) / 3 + t2(x, x * 2): Real 26.0
t1(x, 2 * t0(x,x)): Real 45.0
t1(x, 2 * t1(x,x)): Real 45.0
t0(x, t0(x, x) * 2): Real 45.0
t0(x, t1(x, x) * 2): Real 45.0
a5: RealVector (seq [36.0; 60.0; 168.0])
twl.Length: 1
twl.Length: 2
twl.Length: 2
v.Count: 3
v.Count: 3
twl.Length: 2
v.Count: 3
v.Count: 3
a6:WTensor
  (DSTensor
     tensor([[[[ 2.,  4.,  6.],
          [ 8., 10., 12.]],

         [[14., 16., 18.],
          [20., 22., 24.]]]]))
twl.Length: 1
twl.Length: 2
twl.Length: 2
v.Count: 3
v.Count: 3
twl.Length: 2
v.Count: 3
v.Count: 3
a7:WTensor
  (DSTensor
     tensor([[[[ 3.,  5.,  7.],
          [ 9., 11., 13.]],

         [[15., 17., 19.],
          [21., 23., 25.]]]]))    
```
    
    




2022-03-12 Now there is a private repo which supports DiffSharp Tensor within Math.NET Symbolics. (very rough/early stage)
If you are interested about it, please leave a message in issues.

==================

For supporting code like the following:

```
#r @"src\Symbolics\bin\Debug\netstandard2.0\MathNet.Symbolics.dll"
#r @"nuget:MathNet.Numerics"
#r @"nuget:FsUnit"
#r @"nuget:FParsec"
#r @"nuget:MathNet.Numerics.FSharp"
#load @"src\Symbolics.Tests\Global.fs"

open MathNet.Numerics
open MathNet.Symbolics
open Global
open Operators
open VariableSets.Alphabet
type Expr = SymbolicExpression

let symV = Symbol "v"
let symW = Symbol "w"
let symX = Symbol "x"
let symY = Symbol "y"
let symZ = Symbol "z"


open Definition
define "test" ([symV; symW], (v + w)*2)
SymbolicExpression(Infix.parseOrThrow("2^test(x, 2 * x)")).Evaluate(dict[ "x", FloatingPoint.Real 2.0; ])
```

Result:
```
val it : FloatingPoint = Real 4096.0
```

Code:
```
SymbolicExpression(cFun("test", [x + (fromInt32 10); (fromDouble 100.0)])*2).Evaluate(dict[ "x", FloatingPoint.Real 9.0; ])

```

Result:
```
val it : FloatingPoint = Real 476.0
```


```
open System
open MathNet.Numerics.LinearAlgebra
open MathNet.Symbolics
let v = FloatingPoint.RealVector <| vector[1.0;2.0;3.0]

let symbols2 = dict[ "a", v ]

[<EntryPoint>]
let main argv =
    let a0 = SymbolicExpression(Infix.parseOrThrow("a * 2")).Evaluate(symbols2)
    printfn "%A" a0.RealVectorValue
    let a1 = SymbolicExpression(Infix.parseOrThrow("a + 1")).Evaluate(symbols2)
    printfn "%A" a1.RealVectorValue
    let a2 = SymbolicExpression(Infix.parseOrThrow("mat_by_row(a, a)")).Evaluate(symbols2)
    printfn "%A" a2.RealMatrixValue
    let a4 = SymbolicExpression(Infix.parseOrThrow("mat_multiply(m, m, a)")).Evaluate(symbols2)
    printfn "%A" a4
    0 // return an integer exit code
```

Math.NET Symbolics is a basic open source **computer algebra library for .NET, Silverlight and Mono** written entirely in F#.

This project does *not* aim to become a full computer algebra system. If you need such a system, have a look at Axiom or Maxima instead, or for proprietary commercial solutions Maple, Mathematica or Wolfram Alpha.

You'll find a large set of expression and algebraic operator examples in the [Unit Tests](src/Symbolics.Tests/Tests.fs) (yes, they're actually very readable). A few examples:

* `(3Q + 2)*4/6` → `10/3`.
* `(a/b/(c*a))*(c*d/a)/d` → `1/(a*b)`
* `(a+b)/(b+a)**2` → `1/(a + b)`
* `Algebraic.expand ((a+b)**3)` → `a^3 + 3*a^2*b + 3*a*b^2 + b^3`
* `Exponential.expand (exp(2*x+y))` → `exp(x)^2*exp(y)`
* `Exponential.contract (exp(x)*(exp(x) + exp(y)))` → `exp(2*x) + exp(x + y)`
* `Exponential.simplify (1/(exp(x)*(exp(y)+exp(-x))) - (exp(x+y)-1)/((exp(x+y))**2-1))` → `0`
* `Trigonometric.expand (sin(2*x))` → `2*sin(x)*cos(x)`
* `Trigonometric.contract (sin(x)**2*cos(x)**2)` → `1/8 - (1/8)*cos(4*x)`
* `Trigonometric.simplify ((cos(x)+sin(x))**4 + (cos(x)-sin(x))**4 + cos(4*x) - 3)` → `0`
* `Polynomial.polynomialDivision x (x**3 - 2*x**2 - 4) (x-3)` → `(3 + x + x^2, 5)`
* `Polynomial.polynomialExpansion x y (x**5 + 11*x**4 + 51*x**3 + 124*x**2 + 159*x + 86) (x**2 + 4*x + 5)` → `1 + x + (2 + x)*y + (3 + x)*y^2`
* `Polynomial.gcd x (x**7 - 4*x**5 - x**2 + 4) (x**5 - 4*x**3 - x**2 + 4)` → `4 - 4*x - x^2 + x^3`
* `Rational.rationalize (1+1/(1+1/x))` → `(1 + 2*x)/(1 + x)`
* `Rational.simplify x ((x**2-1)/(x+1))` → `-1 + x`

```fsharp
let taylor (k:int) symbol x a =
    let rec impl n nf acc dxn =
        if n = k then acc else
        impl (n+1) (nf*(n+1)) (acc + (dxn |> Structure.substitute symbol a)/nf*(symbol-a)**n) (Calculus.differentiate symbol dxn)
    impl 0 1 zero x |> Algebraic.expand

taylor 3 x (1/(1-x)) 0Q       → 1 + x + x^2
taylor 3 x (1/x) 1Q           → 3 - 3*x + x^2
taylor 3 x (ln(x)) 1Q         → -3/2 + 2*x - (1/2)*x^2
taylor 4 x (ln(x)) 1Q         → -11/6 + 3*x - (3/2)*x^2 + (1/3)*x^3
taylor 4 x (sin(x)+cos(x)) 0Q → 1 + x - (1/2)*x^2 - (1/6)*x^3
```

### Literature

* Computer Algebra and Symbolic Computation - Elementary Algorithms, Joel. S. Cohen
* Computer Algebra and Symbolic Computation - Mathematical Methods, Joel. S. Cohen
* Modern Computer Algebra, Second Edition, Joachim von zur Gathen, Jürgen Gerhard
* Symbolic Integration I - Transcendental Functions, Second Edition, Manuel Bronstein
* Concrete Mathematics, Second Edition, Graham, Knuth, Patashnik
* ... and of course the fundamental theory by Euclid, Newton, Gauss, Fermat and Hilbert.

### Project

Windows (.NET): [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/6u71stf85qudifvv/branch/master)](https://ci.appveyor.com/project/cdrnet/mathnet-symbolics)

Maintained by [Christoph Rüegg](https://christoph.ruegg.name/) and part of the [Math.NET initiative](https://www.mathdotnet.com) (see also [Math.NET Numerics](https://numerics.mathdotnet.com/)). It is covered under the terms of the [MIT/X11](https://mathnetnumerics.codeplex.com/license) open source license. See also the [license](LICENSE.md) file in the root folder. **We accept contributions!**

* [**Project Website**](https://symbolics.mathdotnet.com/)
* [Source Code](https://github.com/mathnet/mathnet-symbolics)
* [Release Notes](https://symbolics.mathdotnet.com/ReleaseNotes.html)
* [Documentation](https://symbolics.mathdotnet.com/)
* [Issues & Bugs](https://github.com/mathnet/mathnet-symbolics/issues)
* [Discussions](https://discuss.mathdotnet.com/c/symbolics) | [Stack Overflow](https://stackoverflow.com/questions/tagged/mathdotnet) | [Twitter](https://twitter.com/MathDotNet)
