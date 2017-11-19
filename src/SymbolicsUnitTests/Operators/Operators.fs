module Operators

open Expecto
open MathNet.Symbolics
open Operators

let tests =
    testList "Operators" [

        Arithmetic.tests
        Exponential.tests
        Trigonometry.tests

    ]
