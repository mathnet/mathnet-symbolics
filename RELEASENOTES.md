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
