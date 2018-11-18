module Compilation

open Expecto
open MathNet.Numerics
open MathNet.Symbolics
open Operators

let tests =
    testList "Compilation" [
        test "Expression to delegate compilation" {

            let symX = Symbol "x"
            let symY = Symbol "y"

            let toComplex f = complex f 0.0

            let expr1 = x
            (Compile.compileExpression1OrThrow expr1 symX).Invoke(3.0) --> 3.0

            let expr2 = x*x
            (Compile.compileExpression1OrThrow expr2 symX).Invoke(3.0) --> 9.0

            let expr3 = x + y
            (Compile.compileExpression2OrThrow expr3 symX symY).Invoke(3.0, 3.0) --> 6.0

            let expr4 = ln x
            (Compile.compileExpression1OrThrow expr4 symX).Invoke(3.0) --> System.Math.Log(3.0)

            let expr5 = (Constant E) ** x
            (Compile.compileExpression1OrThrow expr5 symX).Invoke(3.0) --> System.Math.Exp(3.0)

            let expr6 = sqrt x
            (Compile.compileExpression1OrThrow expr6 symX).Invoke(12.5) --> System.Math.Sqrt(12.5)

            let expr7 = x ** y
            (Compile.compileExpression2OrThrow expr7 symX symY).Invoke(12.5, 5.7) --> System.Math.Pow(12.5, 5.7)

            let expr8 = abs x
            (Compile.compileExpression1OrThrow expr8 symX).Invoke(-14.0) --> 14.0

            let expr9 = x + 1
            (Compile.compileExpression1OrThrow expr9 symX).Invoke(1.0) --> 2.0

            let expr1' = x
            (Compile.compileComplexExpression1OrThrow expr1' symX).Invoke(toComplex 3.0) --> toComplex 3.0

            let expr2' = x*x
            (Compile.compileComplexExpression1OrThrow expr2' symX).Invoke(toComplex 3.0) --> toComplex 9.0

            let expr3' = x + y
            (Compile.compileComplexExpression2OrThrow expr3' symX symY).Invoke(toComplex 3.0, toComplex 3.0) --> toComplex 6.0

            let expr4' = ln x
            (Compile.compileComplexExpression1OrThrow expr4' symX).Invoke(toComplex 3.0) --> System.Numerics.Complex.Log(toComplex 3.0)

            let expr5' = (Constant E) ** x
            (Compile.compileComplexExpression1OrThrow expr5' symX).Invoke(toComplex 3.0) --> System.Numerics.Complex.Exp(toComplex 3.0)

            let expr6' = sqrt x
            (Compile.compileComplexExpression1OrThrow expr6' symX).Invoke(toComplex 12.5) --> System.Numerics.Complex.Sqrt(toComplex 12.5)

            let expr7' = x ** y
            (Compile.compileComplexExpression2OrThrow expr7' symX symY).Invoke(toComplex 12.5, toComplex 5.7) --> System.Numerics.Complex.Pow(toComplex 12.5, toComplex 5.7)

            let expr8' = abs x
            (Compile.compileComplexExpression1OrThrow expr8' symX).Invoke(System.Numerics.Complex.Create(-14.0, 0.0)) --> toComplex 14.0

            let expr9' = x + 1
            (Compile.compileComplexExpression1OrThrow expr9' symX).Invoke(System.Numerics.Complex.One) --> toComplex 2.0
        }
    ]
