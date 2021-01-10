namespace MathNet.Symbolics.Tests.Operators.Exponential

open NUnit.Framework
open MathNet.Symbolics

open Operators
open VariableSets.Alphabet

module Exp =

    [<Test>]
    let ``Zero, One & Infinity`` () =
        exp undefined ==> "Undefined"
        exp infinity ==> "∞"
        exp negativeInfinity ==> "0"
        exp complexInfinity ==> "Undefined"
        exp 0Q ==> "1"
        exp 1Q ==> "e"

    [<Test>]
    let ``Special Values`` () =
        exp(x + y - (x + y)) ==> "exp(x + y - (x + y))" // "1"

        exp(-1Q) ==> "1/e"
        exp(1Q/2Q*Pi*I) ==> "j"
        exp(2Q/2Q*Pi*I) ==> "-1"
        exp(3Q/2Q*Pi*I) ==> "-j"
        exp(4Q/2Q*Pi*I) ==> "1"
        exp(-3Q/2Q*Pi*I) ==> "j"

        exp(ln x) ==> "x"
        exp(ln(1Q/E)) ==> "1/e"
        exp(ln(1Q/x)) ==> "1/x"
        exp(ln(2Q/3Q)) ==> "2/3"
        exp(ln I) ==> "j"


module Ln =

    [<Test>]
    let ``Zero, One & Infinity`` () =
        ln undefined ==> "Undefined"
        ln infinity ==> "∞"
        ln negativeInfinity ==> "∞"
        ln complexInfinity ==> "∞"
        ln 0Q ==> "-∞"
        ln 1Q ==> "0"
        ln -1Q ==> "π*j"
        ln E ==> "1"

    [<Test>]
    let ``Special Values`` () =
        ln(1Q/x) ==> "ln(1/x)"
        ln(1Q/2Q) ==> "-ln(2)"
        ln(1Q/E) ==> "-1"

        ln I ==> "1/2*π*j"
        ln(2Q*I) ==> "ln(2*j)" // "1/2*π*j + ln(2)"
        ln(-2Q*I) ==> "ln(-2*j)" // "-1/2*π*j + ln(2)"
        ln(-1Q/3Q*I) ==> "ln(-j/3)" // "-1/2*π*j - ln(3)"

        ln(exp(x)) ==> "ln(exp(x))"
        ln(exp(5Q)) ==> "ln(exp(5))" // "5"


module Log10 =

    [<Test>]
    let ``Zero, One & Infinity`` () =
        lg undefined ==> "Undefined"
        lg infinity ==> "∞"
        lg negativeInfinity ==> "∞"
        lg complexInfinity ==> "∞"
        lg 0Q ==> "-∞"
        lg 1Q ==> "0"
        lg 10Q ==> "1"

    [<Test>]
    let ``Special Values`` () =
        lg(10Q) ==> "1"
        lg(1Q/10Q) ==> "lg(1/10)" // "-1"
