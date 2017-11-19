module Exponential

open Expecto
open MathNet.Symbolics
open Operators

let tests =
    testList "Exponential" [

        testList "Exp" [
            test "Zero, One & Infinity" {
                exp undefined ==> "Undefined"
                exp infinity ==> "∞"
                exp negativeInfinity ==> "0"
                exp complexInfinity ==> "Undefined"
                exp 0Q ==> "1"
                exp 1Q ==> "e"
            }

            test "Special Values" {
                exp(x + y - (x + y)) ==> "exp(x + y - (x + y))" // "1"

                exp(-1Q) ==> "exp(-1)" // "1/e"
                exp(1Q/2Q*pi*Constant I) ==> "exp(1/2*π*j)" // "j"
                exp(2Q/2Q*pi*Constant I) ==> "exp(π*j)" // "-1"
                exp(3Q/2Q*pi*Constant I) ==> "exp(3/2*π*j)" // "-j"
                exp(4Q/2Q*pi*Constant I) ==> "exp(2*π*j)" // "1"
                exp(-3Q/2Q*pi*Constant I) ==> "exp(-3/2*π*j)" // "j"

                exp(ln(x)) ==> "exp(ln(x))" // "x"
                exp(ln(1Q/Constant E)) ==> "exp(ln(1/e))" // "1/e"
                exp(ln(1Q/x)) ==> "exp(ln(1/x))" // "1/x"
                exp(ln(2Q/3Q)) ==> "exp(ln(2/3))" // "2/3"
                exp(ln(Constant I)) ==> "exp(ln(j))" // "j"
            }
        ]

        testList "Ln" [
            test "Zero, One & Infinity" {
                ln undefined ==> "Undefined"
                ln infinity ==> "∞"
                ln negativeInfinity ==> "∞"
                ln complexInfinity ==> "∞"
                ln 0Q ==> "-∞"
                ln 1Q ==> "0"
                ln Expression.E ==> "1"
            }

            test "Special Values" {
                ln(1Q/x) ==> "ln(1/x)"
                ln(1Q/2Q) ==> "ln(1/2)" // "-ln(2)"
                ln(1Q/Constant E) ==> "ln(1/e)" // "-1"

                ln(Constant I) ==> "ln(j)" // "1/2*π*j"
                ln(2Q*Constant I) ==> "ln(2*j)" // "1/2*π*j + ln(2)"
                ln(-2Q*Constant I) ==> "ln(-2*j)" // "-1/2*π*j + ln(2)"
                ln(-1Q/3Q*Constant I) ==> "ln(-j/3)" // "-1/2*π*j - ln(3)"

                ln(exp(x)) ==> "ln(exp(x))"
                ln(exp(5Q)) ==> "ln(exp(5))" // "5"
            }
        ]

        testList "Log10" [
            test "Zero, One & Infinity" {
                log10 undefined ==> "Undefined"
                log10 infinity ==> "∞"
                log10 negativeInfinity ==> "∞"
                log10 complexInfinity ==> "∞"
                log10 0Q ==> "-∞"
                log10 1Q ==> "0"
                log10 10Q ==> "1"
            }

            test "Special Values" {
                log10(10Q) ==> "1"
                log10(1Q/10Q) ==> "log(1/10)" // "-1"
            }
        ]

    ]
