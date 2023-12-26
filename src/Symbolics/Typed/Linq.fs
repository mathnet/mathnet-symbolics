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
        | ExpressionType.Constant, (:? ConstantExpression as e) -> fromInt64 (Convert.ToInt64(e.Value))
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

    let private toLambda (expr : MathNet.Symbolics.Expression) (args : Symbol list) (valueType : Type) (mathType : Type) constant value add mul div pow atan2 log abs besselj bessely besseli besselk besseliratio besselkratio hankelh1 hankelh2 : LambdaExpression option =
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
            | Argument(sym) ->
                Option.map (fun x -> x :> Expression) (getParam sym)
            | Values.Value v -> value v
            | Constant c -> constant c
            | Function(func, par) ->
                let convertFunc : Function -> (Expression -> Expression) option = function
                    | Sin  -> Some (mathCall1 "Sin")
                    | Cos  -> Some (mathCall1 "Cos")
                    | Tan  -> Some (mathCall1 "Tan")
                    | Csc  -> Some (mathCall1 "Csc")
                    | Sec  -> Some (mathCall1 "Sec")
                    | Cot  -> Some (mathCall1 "Cot")
                    | Sinh -> Some (mathCall1 "Sinh")
                    | Cosh -> Some (mathCall1 "Cosh")
                    | Tanh -> Some (mathCall1 "Tanh")
                    | Csch  -> Some (mathCall1 "Csch")
                    | Sech  -> Some (mathCall1 "Sech")
                    | Coth  -> Some (mathCall1 "Coth")
                    | Asin -> Some (mathCall1 "Asin")
                    | Acos -> Some (mathCall1 "Acos")
                    | Atan -> Some (mathCall1 "Atan")
                    | Acsc -> Some (mathCall1 "Acsc")
                    | Asec -> Some (mathCall1 "Asec")
                    | Acot -> Some (mathCall1 "Acot")
                    | Asinh -> Some (mathCall1 "Asinh")
                    | Acosh -> Some (mathCall1 "Acosh")
                    | Atanh -> Some (mathCall1 "Atanh")
                    | Acsch -> Some (mathCall1 "Acsch")
                    | Asech -> Some (mathCall1 "Asech")
                    | Acoth -> Some (mathCall1 "Acoth")
                    | AiryAi -> Some (mathCall1 "AiryAi")
                    | AiryAiPrime -> Some (mathCall1 "AiryAiPrime")
                    | AiryBi -> Some (mathCall1 "AiryBi")
                    | AiryBiPrime -> Some (mathCall1 "AiryBiPrime")
                    | Ln   -> Some (mathCall1 "Log")
                    | Lg  -> Some (mathCall1 "Log10")
                    | Exp  -> Some (mathCall1 "Exp")
                    | Abs  -> Some abs
                let f = convertFunc func
                let e = convertExpr par
                Option.map2 id f e
            | FunctionN(Atan2, [x;y]) ->
                let exprX = convertExpr x
                let exprY = convertExpr y
                Option.map2 atan2 exprX exprY
            | FunctionN(Log, [x;y]) ->
                let exprX = convertExpr x
                let exprY = convertExpr y
                Option.map2 log exprX exprY
            | FunctionN(BesselJ, [nu;x]) ->
                let exprX = convertExpr nu
                let exprY = convertExpr x
                Option.map2 besselj exprX exprY
            | FunctionN(BesselY, [nu;x]) ->
                let exprX = convertExpr nu
                let exprY = convertExpr x
                Option.map2 bessely exprX exprY
            | FunctionN(BesselI, [nu;x]) ->
                let exprX = convertExpr nu
                let exprY = convertExpr x
                Option.map2 besseli exprX exprY
            | FunctionN(BesselK, [nu;x]) ->
                let exprX = convertExpr nu
                let exprY = convertExpr x
                Option.map2 besselk exprX exprY
            | FunctionN(BesselIRatio, [nu;x]) ->
                let exprX = convertExpr nu
                let exprY = convertExpr x
                Option.map2 besseliratio exprX exprY
            | FunctionN(BesselKRatio, [nu;x]) ->
                let exprX = convertExpr nu
                let exprY = convertExpr x
                Option.map2 besselkratio exprX exprY
            | FunctionN(HankelH1, [nu;x]) ->
                let exprX = convertExpr nu
                let exprY = convertExpr x
                Option.map2 hankelh1 exprX exprY
            | FunctionN(HankelH2, [nu;x]) ->
                let exprX = convertExpr nu
                let exprY = convertExpr x
                Option.map2 hankelh2 exprX exprY
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
            | Power(x, m) when m = minusOne ->
                let a = convertExpr x
                Option.map2 div (value Value.one) a
            | Power (x, Power(n, m)) when m = minusOne ->
                let a = convertExpr x
                let b = convertExpr (Power(n, m))
                if n = two then
                    Option.map (mathCall1 "Sqrt") a
                else
                    let a = convertExpr x
                    let b = convertExpr (Power(n, m))
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
            | Value.Approximation a -> Some (Expression.Constant a.RealValue :> Expression)
            | Value.NegativeInfinity -> Some (Expression.Constant System.Double.NegativeInfinity :> Expression)
            | Value.PositiveInfinity -> Some (Expression.Constant System.Double.PositiveInfinity :> Expression)
            | Value.Number n -> Some (Expression.Constant (float n) :> Expression)
            | _ -> None
        let constant = function
            | E -> Some (Expression.Constant Constants.E :> Expression)
            | Pi -> Some (Expression.Constant Constants.Pi :> Expression)
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
        let besselj = mathCall2 "BesselJ"
        let bessely = mathCall2 "BesselY"
        let besseli = mathCall2 "BesselI"
        let besselk = mathCall2 "BesselK"
        let besseliratio = mathCall2 "BesselIRatio"
        let besselkratio = mathCall2 "BesselKRatio"
        let hankelh1 = mathCall2 "HankelH1"
        let hankelh2 = mathCall2 "HankelH2"
        toLambda expr args valueType mathType constant value add mul div pow atan2 log abs besselj bessely besseli besselk besseliratio besselkratio hankelh1 hankelh2

    [<CompiledName("FormatComplexLambda")>]
    let formatComplexLambda (expr : MathNet.Symbolics.Expression) (args : Symbol list) : LambdaExpression option =
        let value = function
            | Value.Approximation a -> Some (Expression.Constant a.ComplexValue :> Expression)
            | Value.NegativeInfinity -> Some (Expression.Constant (complex System.Double.NegativeInfinity 0.0) :> Expression)
            | Value.PositiveInfinity -> Some (Expression.Constant (complex System.Double.PositiveInfinity 0.0) :> Expression)
            | Value.Number n -> Some (Expression.Constant (complex (float n) 0.0) :> Expression)
            | _ -> None
        let constant = function
            | E -> Some (Expression.Constant (complex Constants.E 0.0) :> Expression)
            | Pi -> Some (Expression.Constant (complex Constants.Pi 0.0) :> Expression)
            | I -> Some (Expression.Constant (complex 0.0 1.0) :> Expression)
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
        let besselj = mathCall2 "BesselJ"
        let bessely = mathCall2 "BesselY"
        let besseli = mathCall2 "BesselI"
        let besselk = mathCall2 "BesselK"
        let besseliratio = mathCall2 "BesselIRatio"
        let besselkratio = mathCall2 "BesselKRatio"
        let hankelh1 = mathCall2 "HankelH1"
        let hankelh2 = mathCall2 "HankelH2"
        toLambda expr args valueType mathType constant value add mul div pow atan2 log abs besselj bessely besseli besselk besseliratio besselkratio hankelh1 hankelh2
