namespace MathNet.Symbolics

open System
open System.Linq.Expressions
open MathNet.Symbolics
open MathNet.Numerics

module Compile =

    let compileExpression expr args = Option.map (fun (x : LambdaExpression) -> x.Compile()) (Linq.formatLambda expr args)
    let compileComplexExpression expr args = Option.map (fun (x : LambdaExpression) -> x.Compile()) (Linq.formatComplexLambda expr args)

    let compileExpressionOrThrow expr args = (Linq.formatLambda expr args).Value.Compile()
    let compileComplexExpressionOrThrow expr args = (Linq.formatComplexLambda expr args).Value.Compile()

    let compileExpression1 expr arg = Option.map (fun (x : Delegate) -> x :?> Func<float, float>) (compileExpression expr [ arg ])
    let compileExpression2 expr arg1 arg2 = Option.map (fun (x : Delegate) -> x :?> Func<float, float, float>) (compileExpression expr [ arg1; arg2 ])
    let compileExpression3 expr arg1 arg2 arg3 = Option.map (fun (x : Delegate) -> x :?> Func<float, float, float, float>) (compileExpression expr [ arg1; arg2; arg3 ])
    let compileExpression4 expr arg1 arg2 arg3 arg4 = Option.map (fun (x : Delegate) -> x :?> Func<float, float, float, float, float>) (compileExpression expr [ arg1; arg2; arg3; arg4 ])

    let compileExpression1OrThrow expr arg = compileExpressionOrThrow expr [ arg ] :?> Func<float, float>
    let compileExpression2OrThrow expr arg1 arg2 = compileExpressionOrThrow expr [ arg1; arg2 ] :?> Func<float, float, float>
    let compileExpression3OrThrow expr arg1 arg2 arg3 = compileExpressionOrThrow expr [ arg1; arg2; arg3 ] :?> Func<float, float, float, float>
    let compileExpression4OrThrow expr arg1 arg2 arg3 arg4 = compileExpressionOrThrow expr [ arg1; arg2; arg3; arg4 ] :?> Func<float, float, float, float, float>
    
    let compileComplexExpression1 expr arg = Option.map (fun (x : Delegate) -> x :?> Func<complex, complex>) (compileComplexExpression expr [ arg ])
    let compileComplexExpression2 expr arg1 arg2 = Option.map (fun (x : Delegate) -> x :?> Func<complex, complex, complex>) (compileComplexExpression expr [ arg1; arg2 ])
    let compileComplexExpression3 expr arg1 arg2 arg3 = Option.map (fun (x : Delegate) -> x :?> Func<complex, complex, complex, complex>) (compileComplexExpression expr [ arg1; arg2; arg3 ])
    let compileComplexExpression4 expr arg1 arg2 arg3 arg4 = Option.map (fun (x : Delegate) -> x :?> Func<complex, complex, complex, complex, complex>) (compileComplexExpression expr [ arg1; arg2; arg3; arg4 ])

    let compileComplexExpression1OrThrow expr arg = compileComplexExpressionOrThrow expr [ arg ] :?> Func<complex, complex>
    let compileComplexExpression2OrThrow expr arg1 arg2 = compileComplexExpressionOrThrow expr [ arg1; arg2 ] :?> Func<complex, complex, complex>
    let compileComplexExpression3OrThrow expr arg1 arg2 arg3 = compileComplexExpressionOrThrow expr [ arg1; arg2; arg3 ] :?> Func<complex, complex, complex, complex>
    let compileComplexExpression4OrThrow expr arg1 arg2 arg3 arg4 = compileComplexExpressionOrThrow expr [ arg1; arg2; arg3; arg4 ] :?> Func<complex, complex, complex, complex, complex>

    type MathNet.Symbolics.Expression with
        member this.Compile ([<ParamArray>] args : Symbol array) = compileExpressionOrThrow this (Array.toList args)
        member this.Compile (arg : Symbol) = compileExpression1OrThrow this arg
        member this.Compile (arg1 : Symbol, arg2 : Symbol) = compileExpression2OrThrow this arg1 arg2
        member this.Compile (arg1 : Symbol, arg2 : Symbol, arg3 : Symbol) = compileExpression3OrThrow this arg1 arg2 arg3
        member this.Compile (arg1 : Symbol, arg2 : Symbol, arg3 : Symbol, arg4 : Symbol) = compileExpression4OrThrow this arg1 arg2 arg3 arg4

        member this.CompileComplex ([<ParamArray>] args : Symbol array) = compileComplexExpressionOrThrow this (Array.toList args)
        member this.CompileComplex (arg : Symbol) = compileComplexExpression1OrThrow this arg
        member this.CompileComplex (arg1 : Symbol, arg2 : Symbol) = compileComplexExpression2OrThrow this arg1 arg2
        member this.CompileComplex (arg1 : Symbol, arg2 : Symbol, arg3 : Symbol) = compileComplexExpression3OrThrow this arg1 arg2 arg3
        member this.CompileComplex (arg1 : Symbol, arg2 : Symbol, arg3 : Symbol, arg4 : Symbol) = compileComplexExpression4OrThrow this arg1 arg2 arg3 arg4
