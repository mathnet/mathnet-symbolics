(*** hide ***)
#I "../../out/lib/net40"
#load @"..\..\packages\MathNet.Numerics.FSharp.3.2.1\MathNet.Numerics.fsx"
#load @"..\..\src\Symbolics\MathNet.Symbolics.fsx"

(**
Math.NET Symbolics
==================

Math.NET Symbolics is a basic open source computer algebra library for .Net and Mono written in F#.

This project does not aim to become a full computer algebra system. If you need such a system,
have a look at Axiom or Maxima instead, or for commercial solutions Maple, Mathematica or Wolfram Alpha.

Math.NET Symbolics is part of the [Math.NET initiative](http://www.mathdotnet.com/).
Available for free under the [MIT/X11 License](License.html).

NuGet Packages
--------------

The recommended way to get Math.NET Symbolics is to use NuGet. The following packages are provided and maintained in the public [NuGet Gallery](https://nuget.org/profiles/mathnet/):

Core Package:

- [**MathNet.Symbolics**](https://www.nuget.org/packages/MathNet.Symbolics/) - core package

Platform Support and Dependencies
---------------------------------

- .Net 4.0 and Mono: Windows, Linux and Mac.

Package Dependencies:

- [FParsec](http://www.nuget.org/packages/FParsec) (isolated usage only for infix parsing)


Math.NET Symbolics with F# and F# Interactive
---------------------------------------------

With NuGet you can start quickly by installing the `MathNet.Symbolics` package,
which automatically loads its dependencies `MathNet.Numerics` and `MathNet.Numerics.FSharp`.
In F# interactive you can reference them by loading two scripts, along the lines of

    [lang=text]
    #load @"..\..\packages\MathNet.Numerics.FSharp.3.2.1\MathNet.Numerics.fsx"
    #load @"..\..\packages\MathNet.Symbolics.0.2.0-alpha\MathNet.Symbolics.fsx"

To get started, open the namespaces and the Operators module and declare the variables
and constants you intend to use as symbols:
*)

open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

open Operators

let x = symbol "x"
let y = symbol "y"
let z = symbol "z"
let a = symbol "a"
let b = symbol "b"
let c = symbol "c"
let d = symbol "d"
let e = symbol "e"
let f = symbol "f"

(**
Then we're all set to start writing expressions:
*)

a + a                  // returns 2*a
a * a                  // returns a^2
2 + 1/x - 1            // returns 1 + 1/x
(a/b/(c*a))*(c*d/a)/d  // returns 1/(a*b)

(**
Math.NET Symbolics expressions are always in a simplified form according to a set of rules.
Expressions are tree structures, but F# interactive shows them in a readable infix form thanks
to a display printer added in the script loaded above. You can also use these printers manually
to format any expression as infix string, LaTeX expression or in strict mode to see the actual
internal representation:
*)

Infix.print (1/(a*b))        // returns string "1/(a*b)"
Infix.printStrict (1/(a*b))  // returns string "a^(-1)*b^(-1)"
LaTeX.print (1/(a*b))        // returns string "\frac{1}{ab}"

(**
Strings in infix notation can be parsed back into expressions:
*)

Infix.parse "1/(a*b)"     // Returns ParsedExpression 1/(a*b)
Infix.parse "1/(a*b"      // Returns ParseFailure, 7: Expecting infix operator or ')'
Infix.tryParse "1/(a*b)"  // Returns Some (1/(a*b))
Infix.parseOrUndefined "1/(a*b)"  // Returns 1/(a*b)
Infix.parseOrThrow "1/(a*b)"      // Returns 1/(a*b)

(**
### Number Literals

Numbers can be forced to become expressions using the `Q` suffix, e.g. `3Q`
is an expression representing an integer with value 3. This is usually not needed
if at least one of the operands is an expression, e.g. a symbol. But if all operands
are standard .Net numbers, they will be treated as such. For example `(3 + 2)*4/6` is
a standard F# integer expression and will result in `3` due to .Net integer arithmetics.
However, if we force it to become an expression by writing `(3Q + 2)*4/6`, it will
result in the fraction expression `10/3` as expected.

Since Math.NET Symbolics is about algebra, all number literals are arbitrarily big
rational numbers, i.e. integers or fractions. If you need floating point numbers, use
a symbol for them instead and provide their value at evaluation time.

### Evaluating Expressions

Often you need to evaluate the resulting number value of an expression given the values
for all its symbols. To do so, prepare the value set as map or dictionary and pass it
to the evaluate function. Values need to be of type FloatingPoint which is a discriminated
union that can represent not only float and complex but also vectors and matrices of the same.
*)

let symbols = Map.ofList [ "a", Real 2.0; "b", Real 3.0 ]
Evaluate.evaluate symbols (1/(a*b))  // Returns Real 0.1666666667 (as float)

(**
### Manipulating Expressions

There are various modules to help you combine and manipulate expressions:

* **Operators:** standard operators, recommended to open always.
* **Structure:** structural analysis, operand access, substitution, map
* **Algebraic:** algebraic expansion, separate factors
* **Polynomial:** properties, degrees, coefficients, terms, divide, gcd, expansion, partial fraction
* **Rational:** numerator/denominator, properties, rationalize, expand, simplify
* **Exponential:** expand, contract, simplify
* **Trigonometric:** expand, separate, contract, substitute, simplify
* **Calculus:** differentiate

For example, let's try to contract the trigonometric expression $(\cos{x})^4$
into $\frac{3}{8} + \frac{1}{2}\cos{2x} + \frac{1}{8}\cos{4x}$:
*)

Trigonometric.contract (cos(x)**4)  // Returns 3/8 + (1/2)*cos(2*x) + (1/8)*cos(4*x)

(**
### Algebraic Algorithms

These modules can also be combined to build more interesting manipulations.
For example, let's implement a [taylor expansion](https://en.wikipedia.org/wiki/Taylor_series)
routine to approximate the shape of a differentiable function $x(\zeta)$ at
some point $\zeta = a$ by its $k-1$ first derivatives at that point (order $k$). We can leverage the existing
structural substitute routine to substitute $\zeta$ with $a$ to get the resulting expression at $a$,
and the differentiate routine to evaluate the partial derivative $\frac{\partial{x}}{\partial\zeta}$.
*)

let taylor (k:int) symbol a x =
    let rec impl n nf acc dxn =
        if n = k then acc else
        let dxn_a = dxn |> Structure.substitute symbol a
        let dxn'  = dxn |> Calculus.differentiate symbol
        impl (n+1) (nf*(n+1)) (acc + dxn_a/nf*(symbol-a)**n) dxn'
    impl 0 1 zero x |> Algebraic.expand

(**
Let's use this routine to approximate $\sin{x}+\cos{x}$ at $x = 0$ using the first 4 derivatives:
*)

taylor 4 x 0Q (sin(x)+cos(x))  // Returns 1 + x - (1/2)*x^2 - (1/6)*x^3

(**
Math.NET Symbolics with C#
--------------------------

Even though Math.NET Symbolics is written entirely in F#, it can be used in C#
almost exactly the same way. The equivalent C# code to the F# code above could look as follows:

    [lang=csharp]
    using System.Numerics
    using MathNet.Numerics
    using MathNet.Symbolics
    using Expr = MathNet.Symbolics.Expression

    var x = Expr.Symbol("x");
    var y = Expr.Symbol("y");
    var z = Expr.Symbol("z");
    var a = Expr.Symbol("a");
    var b = Expr.Symbol("b");
    var c = Expr.Symbol("c");
    var d = Expr.Symbol("d");
    var e = Expr.Symbol("e");
    var f = Expr.Symbol("f");

    Infix.Print(a+a);                    // returns 2*a
    Infix.Print(a*a);                    // returns a^2
    Infix.Print(2 + 1/x - 1);            // returns 1 + 1/x
    Infix.Print((a/b/(c*a))*(c*d/a)/d);  // returns 1/(a*b)

    Infix.Print(1/(a*b));        // returns string "1/(a*b)"
    Infix.PrintStrict(1/(a*b));  // returns string "a^(-1)*b^(-1)"
    LaTeX.Print(1/(a*b));        // returns string "\frac{1}{ab}"

    Infix.Print(Infix.ParseOrUndefined("1/(a*b)")); // Returns 1/(a*b)
    Infix.Print(Infix.ParseOrUndefined("1/(a*b"));  // Returns Undefined
    Infix.Print(Infix.ParseOrThrow("1/(a*b)"));     // Returns 1/(a*b)

    var symbols = new Dictionary<string,FloatingPoint>
       {{ "a", 2.0 },
        { "b", 3.0 }};

    // Returns 0.166666666666667
    Evaluate.Evaluate(symbols, 1/(a*b)).RealValue;

    // Returns 3/8 + (1/2)*cos(2*x) + (1/8)*cos(4*x)
    Infix.Print(Trigonometric.Contract(Expr.Pow(Expr.Cos(x), 4)));

    // Taylor Expansion
    Expr Taylor(int k, Expr symbol, Expr a, Expr x)
    {
        int factorial = 1;
        Expr accumulator = Expr.Zero;
        Expr derivative = x;
        for(int i=0; i<k; i++)
        {
            var subs = Structure.Substitute(symbol, a, derivative);
            derivative = Calculus.Differentiate(symbol, derivative);
            accumulator = accumulator + subs/factorial*Expr.Pow(symbol-a,i);
            factorial *= (i+1);
        }
        return Algebraic.Expand(accumulator);
    }

    // Returns 1 + x - (1/2)*x^2 - (1/6)*x^3
    Infix.Print(Taylor(4, x, 0, Expr.Sin(x)+Expr.Cos(x)));

*)
