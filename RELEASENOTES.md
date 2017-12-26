### 0.18.1 - 2017-12-23
* Code Signing: SHA2 (256) instead of SHA1

### 0.18.0 - 2017-11-08
* Calculus: fix derivatives of Asin, Acos, Atan *~diluculo*
* Trigonometric: fix contraction of cos(a)*sin(b) *~diluculo*
* Trigonometric: expansion and contraction of sinh/cosh *~diluculo*
* Fix and improve arithmetic of infinity, closer to Mathematica *~diluculo*
* Evaluate: powers with negative radix and non-integer exponent are lifted to complex numbers
* Binaries are now signed with a certificate

### 0.17.0 - 2017-09-16
* Compile expressions to delegates *~Francesco Bertolaccini*
* SymbolicExpression: OOP-friendly expression wrapper

### 0.16.0 - 2017-08-24
* Multi-Arg functions: atan2, log *~Francesco Bertolaccini, FoggyFinder*
* Trigonometric.substitute: handle cot, sec, csc correctly *~Francesco Bertolaccini*
* Expression.root: apply automatic simplification
* VisualExpressions: introduce alternative visual (instead of semantic) expression structure
* Infix, LaTeX, MathML: formatting using visual expressions
* LaTeX: proper rendering of roots, improved parenthesis handling
* Infix: avoid parenthesis around floating-point approximations
* Drop obsolete print functions (use format instead)

### 0.15.0 - 2017-04-30
* Infix: support parsing approximations in exponential notation *~FoggyFinder*
* Functions: trigonometric functions sec, csc, cot *~FoggyFinder*

### 0.14.0 - 2017-02-12
* Infix/LaTeX: enforce culture invariant floating point format
* Infix/LaTeX: support for underscores in symbol names *~FoggyFinder*
* Structure: collectApproximations

### 0.13.0 - 2016-12-29
* Structure: collectSums, collectProducts, collectPowers
* Structure: collectAll, like collect but continues on accepted nodes
* Structure: collectPredicate, collectAllPredicate

### 0.12.0 - 2016-12-27
* LaTeX: format symbols with a long name with braces

### 0.11.0 - 2016-11-06
* Approximation: new concept to represent real (floating point) numbers
* Approximate: new module to partially evaluate/approximate expressions
* Expression: explicit NegativeInfinity DU case

### 0.10.1 - 2016-11-05
* Evaluate: fix CLI method signature of the evaluate function (cross-language support).

### 0.10.0 - 2016-11-05
* LaTeX: always emit braces around power operands
* LaTeX: formatter must not skip multiplication sign in some cases *~FoggyFinder*
* Infix: parsing sqrt, pow *~FoggyFinder*
* Infix: fix bug on parsing hyperbolic functions *~FoggyFinder*
* Evaluate: better error message when a symbol is missing *~Cer Lewis*
* Polynomial: generate from coefficients list *~Zaid-Ajaj*
* Rational: expand to algebraically expand result if its denominator is a number

### 0.9.0 - 2016-03-14
* MathML: basic strict MathML formatting and parsing support
* Infix/LaTeX: fix missing paranthesis when formatting denominator-free powers
* LaTeX: fix symbols when formatting arcsin, arccos or arctan
* Expression: refactoring; operators are now implemented in module instead of class

### 0.8.0 - 2016-01-09
* Simplification: more consistent behavior on infinity and complex infinity
* Expression: new Constant expression leaf nodes (e, pi, I, real/floating-point)
* Expression: merge Positive/NegativeInfinity with Infinity
* Expression: Root, Sqrt, Sinh, Cosh, Tanh, ArcSin, ArcCos, ArcTan
* Functions: Sinh, Cosh, Tanh, ArcSin, ArcCos, ArcTan
* Operators: real, pi, infinity, complexInfinity, negativeInfinity
* Operators: log, root, sqrt, sinh, cosh, tanh, arcsin, arccos, arctan
* Numbers: compare/min/max can also handle the new constants
* Structure: collect, collectIdentifiers, collectNumbers, collectFunctions etc.
* Infix: decimal numbers are now parsed as real constant instead of interpreted as rational
* Infix: unicode symbols for infinity, complex infinity and pi
* Calculus: learnt to differentiate the new functions

### 0.7.1 - 2015-10-03
* Revert FParsec dependency from 1.0.2 back to 1.0.1

### 0.7.0 - 2015-10-03
* Updated package dependencies (no functional changes)
* NuGet package now lists the proper FSharp.Core package

### 0.6.0 - 2015-09-29
* Polynomial: square-free factorization
* Polynomial: commonFactors, coefficientMonomial
* Rational: reduce to cancel common simple factors (part of expand)
* Numbers: integer gcd and lcm routines
* Algebraic: summands, factors, factorsInteger
* Expression: FromIntegerFraction

### 0.5.0 - 2015-07-18
* Infix Parser: interpret decimal notation as exact rational numbers ("0.2" -> "1/5")
* Infix Parser: allow white space after number literal
* Calculus: modified argument order for `taylor` for better currying (breaking!)
* Calculus: new `tangentLine`, `normalLine`
* Calculus: new `differentiateAt` as shortcut for `differentiate >> substitute`

### 0.4.0 - 2014-11-26
* Calculus: add taylor expansion function
* Better Paket compatibility (and NuGet with -ExcludeVersion)
* Use MathNet.Numerics v3.3.0

### 0.3.0 - 2014-09-21
* Use official FSharp.Core 3.1.1 NuGet package, drop alpha suffix
* Now using Paket internally to maintain NuGet dependencies

### 0.2.1-alpha - 2014-09-03
* Package fix to include explicit FSharp.Core reference

### 0.2.0-alpha - 2014-09-02
* First actual release
* Added and improved infix and latex expression printers and infix parsers
* C# compatibility work: more idiomatic when used in C# or other .Net languages

### 0.1.0-alpha - 2014-04-07
* Initial version
