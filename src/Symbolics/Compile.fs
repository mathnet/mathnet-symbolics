namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
module Option =
     let map2 f a b =
        a |> Option.bind (fun a' -> b |> Option.map (fun b' -> f a' b'))

module Compile =
    open MathNet.Symbolics.ExpressionPatterns
    open MathNet.Symbolics.Operators
    open System.Linq.Expressions
    type private Expression = MathNet.Symbolics.Expression
    type private LExpression = System.Linq.Expressions.Expression

    let rec private numerator = function
        | NegPower _ -> one
        | Product ax -> product <| List.map numerator ax
        | z -> z
    let rec private denominator = function
        | NegPower (r, p) -> r ** -p
        | Product ax -> product <| List.map denominator ax
        | _ -> one

    let compile (expr : Expression) (args : Symbol list) : Delegate option =
        let argName = function |Symbol(n) -> n
        let paramList = List.map (fun x -> LExpression.Parameter(typeof<float>, argName x)) args
        let getParam p = List.fold (fun x (y : ParameterExpression) ->
            match x with
                | None when y.Name = (argName p) -> Some y
                | Some(v) -> Some v
                | _ -> None) None paramList
        let mathType = typeof<System.Math>
        let constant x = (LExpression.Constant(x) :> LExpression)
        let rec convertExpr : Expression -> LExpression option = function
            | Identifier(sym) ->
                Option.map (fun x -> x :> LExpression) (getParam sym)
            | Approximation(Approximation.Real(r)) -> Some (constant r)
            | Approximation(_) -> None
            | ComplexInfinity -> None
            | PositiveInfinity -> Some (constant System.Double.PositiveInfinity)
            | NegativeInfinity -> Some (constant System.Double.NegativeInfinity)
            | Constant(Constant.E) -> Some (constant System.Math.E)
            | Constant(Constant.Pi) -> Some (constant System.Math.PI)
            | Constant(Constant.I) -> None
            | Function(func, par) ->
                let toMathMethod name (arg : LExpression) = (LExpression.Call(mathType.GetMethod(name), arg) :> LExpression)
                let convertFunc : Function -> (LExpression -> LExpression) option = function
                    | Sin  -> Some (toMathMethod "Sin")
                    | Cos  -> Some (toMathMethod "Cos")
                    | Tan  -> Some (toMathMethod "Tan")
                    | Asin -> Some (toMathMethod "Asin")
                    | Acos -> Some (toMathMethod "Acos")
                    | Atan -> Some (toMathMethod "Atan")
                    | Ln   -> Some (toMathMethod "Log")
                    | Log  -> Some (toMathMethod "Log10") 
                    | Sinh -> Some (toMathMethod "Sinh")
                    | Cosh -> Some (toMathMethod "Cosh")
                    | Tanh -> Some (toMathMethod "Tanh")
                    | Exp  -> Some (toMathMethod "Exp")
                    | Abs  -> Some (fun x -> (LExpression.Call(mathType.GetMethod("Abs"), x) :> LExpression))
                    | _    -> None
                let f = convertFunc func
                let e = convertExpr par
                Option.map2 id f e
             | FunctionN(Atan, [x;y]) ->
                let exprX = convertExpr x
                let exprY = convertExpr y
                Option.map2 (fun a b -> (LExpression.Call(mathType.GetMethod("Atan2"), a, b) :> LExpression)) exprX exprY
             | FunctionN(_) -> None
             | Number(n) -> Some (constant <| float n)
             | Power(x, y) ->
                let baseE = convertExpr x
                let exponE = convertExpr y
                Option.map2 (fun a b -> LExpression.Call(mathType.GetMethod("Pow"), a, b) :> LExpression) baseE exponE
            | Sum(xs) ->
                let summands = List.map convertExpr xs
                List.fold (Option.map2 (fun l r -> LExpression.Add(l, r) :> LExpression)) (Some (constant 0.0)) summands
            | Product(_) as p ->
                let n = numerator p
                let d = denominator p
                if isOne d then
                    compileFraction n
                else
                    let nExp = compileFraction n
                    let dExp = compileFraction d
                    Option.map2 (fun x y -> LExpression.Divide(x, y) :> LExpression) nExp dExp
            | Undefined -> None
        and compileFraction = function
                | Product(xs) ->
                    let prods = List.map convertExpr xs
                    List.fold (Option.map2 (fun l r -> LExpression.Multiply(l, r) :> LExpression)) (Some (constant 1.0)) prods
                | x -> convertExpr x
        let simplifiedBody = Trigonometric.simplify expr
        Option.map (fun body -> (LExpression.Lambda(body, paramList).Compile())) (convertExpr simplifiedBody)

    let compileOrThrow expr args =
        match compile expr args with
            | Some e -> e
            | None -> failwith "Invalid expression"

    let compile1 expr arg1 =
        Option.map (fun (x : Delegate) -> x :?> Func<float, float>) (compile expr [arg1])
    let compile2 expr arg1 arg2 =
        Option.map (fun (x : Delegate) -> x :?> Func<float, float, float>) (compile expr [arg1; arg2])
    let compile3 expr arg1 arg2 arg3 =
        Option.map (fun (x : Delegate) -> x :?> Func<float, float, float, float>) (compile expr [arg1; arg2; arg3])
    let compile4 expr arg1 arg2 arg3 arg4 =
        Option.map (fun (x : Delegate) -> x :?> Func<float, float, float, float, float>) (compile expr [arg1; arg2; arg3; arg4])

    let compile1OrThrow expr arg1 =
        compileOrThrow expr [arg1] :?> Func<float, float>
    let compile2OrThrow expr arg1 arg2 =
        compileOrThrow expr [arg1; arg2] :?> Func<float, float, float>
    let compile3OrThrow expr arg1 arg2 arg3 =
        compileOrThrow expr [arg1; arg2; arg3] :?> Func<float, float, float, float>
    let compile4OrThrow expr arg1 arg2 arg3 arg4 =
        compileOrThrow expr [arg1; arg2; arg3; arg4] :?> Func<float, float, float, float, float>

    let compileComplex (expr : Expression) (args : Symbol list) : Delegate option =
        let argName = function |Symbol(n) -> n
        let paramList = List.map (fun x -> LExpression.Parameter(typeof<System.Numerics.Complex>, argName x)) args
        let getParam p = List.fold (fun x (y : ParameterExpression) ->
            match x with
                | None when y.Name = (argName p) -> Some y
                | Some(v) -> Some v
                | _ -> None) None paramList
        let mathType = typeof<complex>
        let constant x = (LExpression.Constant(x) :> LExpression)
        let rec convertExpr : Expression -> LExpression option = function
            | Identifier(sym) ->
                Option.map (fun x -> x :> LExpression) (getParam sym)
            | Approximation(Approximation.Real(r)) -> Some (constant r)
            | Approximation(Approximation.Complex(c)) -> Some (constant c)
            | ComplexInfinity -> None
            | PositiveInfinity -> Some (constant (System.Numerics.Complex.Create (System.Double.PositiveInfinity, 0.0)) )
            | NegativeInfinity -> Some (constant (System.Numerics.Complex.Create (System.Double.NegativeInfinity, 0.0)) )
            | Constant(Constant.E) -> Some (constant (System.Numerics.Complex.Create (System.Math.E, 0.0)) )
            | Constant(Constant.Pi) -> Some (constant (System.Numerics.Complex.Create (System.Math.PI, 0.0)))
            | Constant(Constant.I) -> Some (constant System.Numerics.Complex.ImaginaryOne)
            | Function(func, par) ->
                let toMathMethod name (arg : LExpression) = (LExpression.Call(mathType.GetMethod(name), arg) :> LExpression)
                let convertFunc : Function -> (LExpression -> LExpression) option = function
                    | Sin  -> Some (toMathMethod "Sin")
                    | Cos  -> Some (toMathMethod "Cos")
                    | Tan  -> Some (toMathMethod "Tan")
                    | Asin -> Some (toMathMethod "Asin")
                    | Acos -> Some (toMathMethod "Acos")
                    | Atan -> Some (toMathMethod "Atan")
                    | Ln   -> Some (toMathMethod "Log")
                    | Log  -> Some (toMathMethod "Log10")
                    | Sinh -> Some (toMathMethod "Sinh")
                    | Cosh -> Some (toMathMethod "Cosh")
                    | Tanh -> Some (toMathMethod "Tanh")
                    | Exp  -> Some (toMathMethod "Exp")
                    | Abs  -> Some (fun x -> (LExpression.Call(mathType.GetMethod("Abs"), x) :> LExpression))
                    | _    -> None
                let f = convertFunc func
                let e = convertExpr par
                Option.map2 id f e
             | FunctionN(Atan, [x;y]) ->
                let exprX = convertExpr x
                let exprY = convertExpr y
                Option.map2 (fun a b -> (LExpression.Call(mathType.GetMethod("Atan"), LExpression.Divide(a, b)) :> LExpression)) exprX exprY
             | FunctionN(_) -> None
             | Number(n) -> Some (constant <| float n)
             | Power(x, y) ->
                let baseE = convertExpr x
                let exponE = convertExpr y
                Option.map2 (fun a b -> LExpression.Call(mathType.GetMethod("Pow"), a, b) :> LExpression) baseE exponE
            | Sum(xs) ->
                let summands = List.map convertExpr xs
                List.fold (Option.map2 (fun l r -> LExpression.Call(mathType.GetMethod("Add"), l, r) :> LExpression)) (Some (constant 0.0)) summands
            | Product(_) as p ->
                let n = numerator p
                let d = denominator p
                if isOne d then
                    compileFraction n
                else
                    let nExp = compileFraction n
                    let dExp = compileFraction d
                    Option.map2 (fun l r -> LExpression.Call(mathType.GetMethod("Divide"), l, r) :> LExpression) nExp dExp
            | Undefined -> None
        and compileFraction = function
                | Product(xs) ->
                    let prods = List.map convertExpr xs
                    List.fold (Option.map2 (fun l r -> LExpression.Call(mathType.GetMethod("Multiply"), l, r) :> LExpression)) (Some (constant 1.0)) prods
                | x -> convertExpr x
        let simplifiedBody = Trigonometric.simplify expr
        Option.map (fun body -> (LExpression.Lambda(body, paramList).Compile())) (convertExpr simplifiedBody)
        
    let compileComplexOrThrow expr args =
        match compileComplex expr args with
            | Some e -> e
            | None -> failwith "Invalid expression"

    let compileComplex1 expr arg1 =
        Option.map (fun (x : Delegate) -> x :?> Func<Complex, Complex>) (compile expr [arg1])
    let compileComplex2 expr arg1 arg2 =
        Option.map (fun (x : Delegate) -> x :?> Func<Complex, Complex, Complex>) (compile expr [arg1; arg2])
    let compileComplex3 expr arg1 arg2 arg3 =
        Option.map (fun (x : Delegate) -> x :?> Func<Complex, Complex, Complex, Complex>) (compile expr [arg1; arg2; arg3])
    let compileComplex4 expr arg1 arg2 arg3 arg4 =
        Option.map (fun (x : Delegate) -> x :?> Func<Complex, Complex, Complex, Complex, Complex>) (compile expr [arg1; arg2; arg3; arg4])

    let compileComplex1OrThrow expr arg1 =
        compileOrThrow expr [arg1] :?> Func<Complex, Complex>
    let compileComplex2OrThrow expr arg1 arg2 =
        compileOrThrow expr [arg1; arg2] :?> Func<Complex, Complex, Complex>
    let compileComplex3OrThrow expr arg1 arg2 arg3 =
        compileOrThrow expr [arg1; arg2; arg3] :?> Func<Complex, Complex, Complex, Complex>
    let compileComplex4OrThrow expr arg1 arg2 arg3 arg4 =
        compileOrThrow expr [arg1; arg2; arg3; arg4] :?> Func<Complex, Complex, Complex, Complex, Complex>

    type MathNet.Symbolics.Expression with
        member this.Compile ([<ParamArray>] args : Symbol array) = compileOrThrow this (Array.toList args)
        member this.Compile (arg : Symbol) = compile1OrThrow this arg
        member this.Compile (arg1 : Symbol, arg2 : Symbol) = compile2OrThrow this arg1 arg2
        member this.Compile (arg1 : Symbol, arg2 : Symbol, arg3 : Symbol) = compile3OrThrow this arg1 arg2 arg3
        member this.Compile (arg1 : Symbol, arg2 : Symbol, arg3 : Symbol, arg4 : Symbol) = compile4OrThrow this arg1 arg2 arg3 arg4

        member this.CompileComplex ([<ParamArray>] args : Symbol array) = compileComplexOrThrow this (Array.toList args)
        member this.CompileComplex (arg : Symbol) = compileComplex1OrThrow this arg
        member this.CompileComplex (arg1 : Symbol, arg2 : Symbol) = compileComplex2OrThrow this arg1 arg2
        member this.CompileComplex (arg1 : Symbol, arg2 : Symbol, arg3 : Symbol) = compileComplex3OrThrow this arg1 arg2 arg3
        member this.CompileComplex (arg1 : Symbol, arg2 : Symbol, arg3 : Symbol, arg4 : Symbol) = compileComplex4OrThrow this arg1 arg2 arg3 arg4