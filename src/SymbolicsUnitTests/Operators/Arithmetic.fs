module Arithmetic

open Expecto
open MathNet.Symbolics
open Operators

let tests =
    testList "Arithmetic" [

        testList "Sum" [
            test "Zero, One & Infinity" {
                0Q + undefined ==> "Undefined"
                0Q + infinity ==> "∞"
                0Q + negativeInfinity ==> "-∞"
                0Q + complexInfinity ==> "⧝"

                infinity + undefined ==> "Undefined"
                infinity + 1Q ==> "∞"
                infinity - 1Q ==> "∞"
                infinity + infinity ==> "∞"
                infinity - infinity ==> "Undefined"
                infinity + complexInfinity ==> "Undefined"
                infinity - complexInfinity ==> "Undefined"

                negativeInfinity + undefined ==> "Undefined"
                negativeInfinity + 1Q ==> "-∞"
                negativeInfinity - 1Q ==> "-∞"
                negativeInfinity + infinity ==> "Undefined"
                negativeInfinity - infinity ==> "-∞"
                negativeInfinity + complexInfinity ==> "Undefined"
                negativeInfinity - complexInfinity ==> "Undefined"

                complexInfinity + undefined ==> "Undefined"
                complexInfinity + 1Q ==> "⧝"
                complexInfinity - 1Q ==> "⧝"
                complexInfinity + infinity ==> "Undefined"
                complexInfinity - infinity ==> "Undefined"
                complexInfinity + complexInfinity ==> "Undefined"
                complexInfinity - complexInfinity ==> "Undefined"
            }
        ]

        testList "Abs" [
            test "Zero, One & Infinity" {
                abs undefined ==> "Undefined"
                abs infinity ==> "∞"
                abs negativeInfinity ==> "∞"
                abs complexInfinity ==> "∞"
                abs Expression.I ==> "1"
            }
        ]

        testList "Multiply" [
            test "Zero, One & Infinity" {
                0Q*undefined ==> "Undefined"
                0Q*infinity ==> "Undefined"
                0Q*negativeInfinity ==> "Undefined"
                0Q*complexInfinity ==> "Undefined"

                1Q*undefined ==> "Undefined"
                1Q*infinity ==> "∞"
                1Q*negativeInfinity ==> "-∞"
                1Q*complexInfinity ==> "⧝"

                (-1Q)*undefined ==> "Undefined"
                (-1Q)*infinity ==> "-∞"
                (-1Q)*negativeInfinity ==> "∞"
                (-1Q)*complexInfinity ==> "⧝"

                infinity*undefined ==> "Undefined"
                infinity*0Q ==> "Undefined"
                infinity*1Q ==> "∞"
                infinity*(-1Q) ==> "-∞"
                infinity*infinity ==> "∞"
                infinity*negativeInfinity ==> "-∞"
                infinity*complexInfinity ==> "⧝"

                negativeInfinity*undefined ==> "Undefined"
                negativeInfinity*0Q ==> "Undefined"
                negativeInfinity*1Q ==> "-∞"
                negativeInfinity*(-1Q) ==> "∞"
                negativeInfinity*infinity ==> "-∞"
                negativeInfinity*negativeInfinity ==> "∞"
                negativeInfinity*complexInfinity ==> "⧝"

                complexInfinity*undefined ==> "Undefined"
                complexInfinity*0Q ==> "Undefined"
                complexInfinity*1Q ==> "⧝"
                complexInfinity*(-1Q) ==> "⧝"
                complexInfinity*infinity ==> "⧝"
                complexInfinity*negativeInfinity ==> "⧝"
                complexInfinity*complexInfinity ==> "⧝"
            }
        ]

        testList "Divide" [
            test "Zero, One & Infinity" {
                0Q/undefined ==> "Undefined"
                0Q/0Q ==> "Undefined"
                0Q/1Q ==> "0"
                0Q/infinity ==> "0"
                0Q/negativeInfinity ==> "0"
                0Q/complexInfinity ==> "0"

                1Q/undefined ==> "Undefined"
                1Q/0Q ==> "⧝"
                1Q/1Q ==> "1"
                1Q/(-1Q) ==> "-1"
                1Q/infinity ==> "0"
                1Q/negativeInfinity ==> "0"
                1Q/complexInfinity ==> "0"

                (-1Q)/undefined ==> "Undefined"
                (-1Q)/0Q ==> "⧝"
                (-1Q)/1Q ==> "-1"
                (-1Q)/(-1Q) ==> "1"
                (-1Q)/infinity ==> "0"
                (-1Q)/negativeInfinity ==> "0"
                (-1Q)/complexInfinity ==> "0"

                infinity/undefined ==> "Undefined"
                infinity/0Q ==> "⧝"
                infinity/1Q ==> "∞"
                infinity/(-1Q) ==> "-∞"
                infinity/infinity ==> "Undefined"
                infinity/negativeInfinity ==> "Undefined"
                infinity/complexInfinity ==> "Undefined"

                negativeInfinity/undefined ==> "Undefined"
                negativeInfinity/0Q ==> "⧝"
                negativeInfinity/1Q ==> "-∞"
                negativeInfinity/(-1Q) ==> "∞"
                negativeInfinity/infinity ==> "Undefined"
                negativeInfinity/negativeInfinity ==> "Undefined"
                negativeInfinity/complexInfinity ==> "Undefined"

                complexInfinity/undefined ==> "Undefined"
                complexInfinity/0Q ==> "⧝"
                complexInfinity/1Q ==> "⧝"
                complexInfinity/(-1Q) ==> "⧝"
                complexInfinity/infinity ==> "Undefined"
                complexInfinity/negativeInfinity ==> "Undefined"
                complexInfinity/complexInfinity ==> "Undefined"
            }
        ]

        testList "Invert" [
            test "Zero, One & Infinity" {
                invert undefined ==> "Undefined"
                invert 0Q ==> "⧝"
                invert infinity ==> "0"
                invert negativeInfinity ==> "0"
                invert complexInfinity ==> "0"
            }
        ]

        testList "Power" [
            test "Zero, One & Infinity" {
                0Q**undefined ==> "Undefined"
                0Q**0Q ==> "Undefined"
                0Q**1Q ==> "0"
                0Q**2Q ==> "0"
                0Q**(-1Q) ==> "⧝"
                0Q**(-2Q) ==> "⧝"
                0Q**infinity ==> "0"
                0Q**negativeInfinity ==> "⧝"
                0Q**complexInfinity ==> "0"

                1Q**undefined ==> "Undefined"
                1Q**0Q ==> "1"
                1Q**1Q ==> "1"
                1Q**2Q ==> "1"
                1Q**(-1Q) ==> "1"
                1Q**(-2Q) ==> "1"
                1Q**infinity ==> "Undefined"
                1Q**negativeInfinity ==> "Undefined"
                1Q**complexInfinity ==> "Undefined"

                2Q**undefined ==> "Undefined"
                2Q**0Q ==> "1"
                2Q**1Q ==> "2"
                2Q**2Q ==> "4"
                2Q**(-1Q) ==> "1/2"
                2Q**(-2Q) ==> "1/4"
                2Q**infinity ==> "∞"
                2Q**negativeInfinity ==> "0"
                2Q**complexInfinity ==> "Undefined"

                (-1Q)**undefined ==> "Undefined"
                (-1Q)**0Q ==> "1"
                (-1Q)**1Q ==> "-1"
                (-1Q)**2Q ==> "1"
                (-1Q)**(-1Q) ==> "-1"
                (-1Q)**(-2Q) ==> "1"
                (-1Q)**infinity ==> "Undefined"
                (-1Q)**negativeInfinity ==> "Undefined"
                (-1Q)**complexInfinity ==> "Undefined"

                (-2Q)**undefined ==> "Undefined"
                (-2Q)**0Q ==> "1"
                (-2Q)**1Q ==> "-2"
                (-2Q)**2Q ==> "4"
                (-2Q)**(-1Q) ==> "-1/2"
                (-2Q)**(-2Q) ==> "1/4"
                (-2Q)**infinity ==> "⧝"
                (-2Q)**negativeInfinity ==> "0"
                (-2Q)**complexInfinity ==> "Undefined"

                infinity**undefined ==> "Undefined"
                infinity**0Q ==> "Undefined"
                infinity**1Q ==> "∞"
                infinity**(-1Q) ==> "0"
                infinity**(-2Q) ==> "0"
                infinity**infinity ==> "⧝"
                infinity**negativeInfinity ==> "0"
                infinity**complexInfinity ==> "Undefined"

                negativeInfinity**undefined ==> "Undefined"
                negativeInfinity**0Q ==> "Undefined"
                negativeInfinity**1Q ==> "-∞"
                negativeInfinity**2Q ==> "∞"
                negativeInfinity**(-1Q) ==> "0"
                negativeInfinity**(-2Q) ==> "0"
                negativeInfinity**infinity ==> "⧝"
                negativeInfinity**negativeInfinity ==> "0"
                negativeInfinity**complexInfinity ==> "Undefined"

                complexInfinity**undefined ==> "Undefined"
                complexInfinity**0Q ==> "Undefined"
                complexInfinity**1Q ==> "⧝"
                complexInfinity**(-1Q) ==> "0"
                complexInfinity**(-2Q) ==> "0"
                complexInfinity**infinity ==> "⧝"
                complexInfinity**negativeInfinity ==> "0"
                complexInfinity**complexInfinity ==> "Undefined"

                root 0Q x ==> "Undefined"
                root infinity x ==> "1"
                x**(1/infinity) ==>"1"
                root negativeInfinity x ==> "1"
                root complexInfinity x ==> "1"
                root infinity 0Q ==> "Undefined"
                root negativeInfinity 0Q ==> "Undefined"
                root complexInfinity 0Q ==> "Undefined" // In WolframAlpha, root(complexInfinity, 0) returns 0
                0Q**(1/complexInfinity) ==> "Undefined"

                // Todo - Imaginary related
                //Expression.I*infinity ==> "∞*j"
                //0Q**Expression.I ==> "Undefined"
                1Q**Expression.I ==> "1"
            }
        ]

    ]
