namespace MathNet.Symbolics

open System
open MathNet.Symbolics
open MathNet.Numerics
open System.Linq.Expressions
open ExpressionPatterns
open Operators

[<RequireQualifiedAccess>]
module Linq =

    module Option =
        let map2 f a b = a |> Option.bind (fun a' -> b |> Option.map (fun b' -> f a' b'))

    [<CompiledName("Parse")>]
    let rec parse (q:Expression) : MathNet.Symbolics.Expression =
        match q.NodeType, q with
        | ExpressionType.UnaryPlus, (:? UnaryExpression as e) -> +(parse e.Operand)
        | ExpressionType.Negate, (:? UnaryExpression as e) -> -(parse e.Operand)
        | ExpressionType.Add, (:? BinaryExpression as e) -> (parse e.Left) + (parse e.Right)
        | ExpressionType.Subtract, (:? BinaryExpression as e) -> (parse e.Left) - (parse e.Right)
        | ExpressionType.Multiply, (:? BinaryExpression as e) -> (parse e.Left) * (parse e.Right)
        | ExpressionType.Divide, (:? BinaryExpression as e) -> (parse e.Left) / (parse e.Right)
        | ExpressionType.Constant, (:? ConstantExpression as e) -> Expression.FromInt64 (Convert.ToInt64(e.Value))
        | ExpressionType.Parameter, (:? ParameterExpression as e) -> Identifier (Symbol e.Name)
        | ExpressionType.MemberAccess, (:? MemberExpression as e) -> Identifier (Symbol e.Member.Name)
        | ExpressionType.Lambda, (:? LambdaExpression as e) -> parse e.Body
        | ExpressionType.Try, (:? TryExpression as e) -> parse e.Body
        | ExpressionType.Convert, (:? UnaryExpression as e) -> parse e.Operand
        | _ -> failwith "not supported"

    let rec private numerator = function
        | NegPower _ -> one
        | Product ax -> product <| List.map numerator ax
        | z -> z
    let rec private denominator = function
        | NegPower (r, p) -> r ** -p
        | Product ax -> product <| List.map denominator ax
        | _ -> one

    let private toLambda (expr : MathNet.Symbolics.Expression) (args : Symbol list) (valueType : Type) (mathType : Type) constant value add mul div pow atan2 log abs : LambdaExpression option =
        let valueTypeArr1 = [| valueType |]
        let valueTypeArr2 = [| valueType; valueType |]
        let argName = function |Symbol(n) -> n
        let paramList = List.map (fun x -> Expression.Parameter(valueType, argName x)) args
        let getParam p = List.fold (fun x (y : ParameterExpression) ->
            match x with
                | None when y.Name = (argName p) -> Some y
                | Some(v) -> Some v
                | _ -> None) None paramList
        let mathCall1 (name : string) (a : Expression) = Expression.Call(mathType.GetMethod(name, valueTypeArr1), a) :> Expression
        let mathCall2 (name : string) (a : Expression) (b : Expression) = Expression.Call(mathType.GetMethod(name, valueTypeArr2), a, b) :> Expression
        let rec convertExpr : MathNet.Symbolics.Expression -> Expression option = function
            | Identifier(sym) ->
                Option.map (fun x -> x :> Expression) (getParam sym)
            | Values.Value v -> value v
            | Constant c -> constant c
            | Function(func, par) ->
                let convertFunc : Function -> (Expression -> Expression) option = function
                    | Sin  -> Some (mathCall1 "Sin")
                    | Cos  -> Some (mathCall1 "Cos")
                    | Tan  -> Some (mathCall1 "Tan")
                    | Asin -> Some (mathCall1 "Asin")
                    | Acos -> Some (mathCall1 "Acos")
                    | Atan -> Some (mathCall1 "Atan")
                    | Ln   -> Some (mathCall1 "Log")
                    | Log  -> Some (mathCall1 "Log10") 
                    | Sinh -> Some (mathCall1 "Sinh")
                    | Cosh -> Some (mathCall1 "Cosh")
                    | Tanh -> Some (mathCall1 "Tanh")
                    | Exp  -> Some (mathCall1 "Exp")
                    | Abs  -> Some abs
                    | _    -> None
                let f = convertFunc func
                let e = convertExpr par
                Option.map2 id f e
             | FunctionN(Atan, [x;y]) ->
                let exprX = convertExpr x
                let exprY = convertExpr y
                Option.map2 atan2 exprX exprY
             | FunctionN(Log, [x;y]) ->
                let exprX = convertExpr x
                let exprY = convertExpr y
                Option.map2 log exprX exprY
             | FunctionN(_) -> None
             | PosIntPower(x, Number(y)) ->
                let basis = convertExpr x
                let rec exponentiate (power : BigRational) exp  =
                    if  power.Numerator.IsEven then
                        let newBasis = mul exp exp
                        exponentiate (power / 2N) newBasis
                    elif power = 1N then
                        exp
                    else
                        let newBasis = exponentiate (power - 1N) exp
                        mul exp newBasis
                Option.map (exponentiate y) basis
             | Power(x, minusOne) when minusOne = Expression.MinusOne ->
                let a = convertExpr x
                Option.map2 div (value Value.one) a
             | Power (x, Power(n, minusOne)) when minusOne = Expression.MinusOne ->
                let a = convertExpr x
                let b = convertExpr (Power(n, minusOne))
                if n = two then
                    Option.map (mathCall1 "Sqrt") a
                else
                    let a = convertExpr x
                    let b = convertExpr (Power(n, minusOne))
                    Option.map2 pow a b
             | Power(Constant E, y) ->
                let exponent = convertExpr y
                Option.map (mathCall1 "Exp") exponent
             | Power(x, y) ->
                let baseE = convertExpr x
                let exponE = convertExpr y
                Option.map2 pow baseE exponE
            | Sum(xs) ->
                let summands = List.map convertExpr xs
                List.fold (Option.map2 add) (value Value.zero) summands
            | Product(_) as p ->
                let n = numerator p
                let d = denominator p
                if isOne d then
                    compileFraction n
                else
                    let nExp = compileFraction n
                    let dExp = compileFraction d
                    Option.map2 div nExp dExp
            | Undefined -> None
        and compileFraction = function
                | Product(xs) ->
                    let prods = List.map convertExpr xs
                    List.fold (Option.map2 mul) (value Value.one) prods
                | x -> convertExpr x
        let simplifiedBody = Trigonometric.simplify expr
        Option.map (fun body -> Expression.Lambda(body, paramList)) (convertExpr simplifiedBody)

    [<CompiledName("FormatLambda")>]
    let formatLambda (expr : MathNet.Symbolics.Expression) (args : Symbol list) : LambdaExpression option =
        let value = function
            | Value.Approximation a -> Some (Expression.Constant(a.RealValue) :> Expression)
            | Value.NegativeInfinity -> Some (Expression.Constant(System.Double.NegativeInfinity) :> Expression)
            | Value.PositiveInfinity -> Some (Expression.Constant(System.Double.PositiveInfinity) :> Expression)
            | Value.Number n -> Some (Expression.Constant(float n) :> Expression)
            | _ -> None
        let constant = function
            | E -> Some (Expression.Constant(System.Math.E) :> Expression)
            | Pi -> Some (Expression.Constant(System.Math.PI) :> Expression)
            | _ -> None
        let valueType = typeof<float>
        let mathType = typeof<System.Math>
        let add a b = Expression.Add(a, b) :> Expression
        let mul a b = Expression.Multiply(a, b) :> Expression
        let div a b = Expression.Divide(a, b) :> Expression
        let mathCall1 (name : string) (a : Expression) = Expression.Call(mathType.GetMethod(name, [| valueType |]), a) :> Expression
        let mathCall2 (name : string) (a : Expression) (b : Expression) = Expression.Call(mathType.GetMethod(name, [| valueType; valueType |]), a, b) :> Expression
        let pow = mathCall2 "Pow"
        let atan2 = mathCall2 "Atan2"
        let log a b = mathCall2 "Log" b a
        let abs = mathCall1 "Abs"
        toLambda expr args valueType mathType constant value add mul div pow atan2 log abs

    [<CompiledName("FormatComplexLambda")>]
    let formatComplexLambda (expr : MathNet.Symbolics.Expression) (args : Symbol list) : LambdaExpression option =
        let value = function
            | Value.Approximation a -> Some (Expression.Constant(a.ComplexValue) :> Expression)
            | Value.NegativeInfinity -> Some (Expression.Constant(System.Numerics.Complex.Create(System.Double.NegativeInfinity, 0.0)) :> Expression)
            | Value.PositiveInfinity -> Some (Expression.Constant(System.Numerics.Complex.Create(System.Double.PositiveInfinity, 0.0)) :> Expression)
            | Value.Number n -> Some (Expression.Constant(System.Numerics.Complex.Create(float n, 0.0)) :> Expression)
            | _ -> None
        let constant = function
            | E -> Some (Expression.Constant(System.Numerics.Complex.Create(System.Math.E, 0.0)) :> Expression)
            | Pi -> Some (Expression.Constant(System.Numerics.Complex.Create(System.Math.PI, 0.0)) :> Expression)
            | _ -> None
        let valueType = typeof<complex>
        let mathType = typeof<complex>
        let mathCall1 (name : string) (a : Expression) = Expression.Call(mathType.GetMethod(name, [| valueType |]), a) :> Expression
        let mathCall2 (name : string) (a : Expression) (b : Expression) = Expression.Call(mathType.GetMethod(name, [| valueType; valueType |]), a, b) :> Expression
        let add = mathCall2 "Add"
        let mul = mathCall2 "Multiply"
        let div = mathCall2 "Divide"
        let pow = mathCall2 "Pow"
        let atan2 a b = mathCall1 "Atan" (div a b)
        let log a b =
            let ln = mathCall1 "Log"
            div (ln b) (ln a)
        let abs a = Expression.Convert(mathCall1 "Abs" a, valueType) :> Expression
        toLambda expr args valueType mathType constant value add mul div pow atan2 log abs

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
    
