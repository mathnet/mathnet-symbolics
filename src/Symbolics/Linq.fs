namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

open System.Linq.Expressions

type SE = MathNet.Symbolics.Expression

module Linq =

    let rec parse (q:Expression) : SE =
        match q.NodeType, q with
        | ExpressionType.UnaryPlus, (:? UnaryExpression as e) -> +(parse e.Operand)
        | ExpressionType.Negate, (:? UnaryExpression as e) -> -(parse e.Operand)
        | ExpressionType.Add, (:? BinaryExpression as e) -> (parse e.Left) + (parse e.Right)
        | ExpressionType.Subtract, (:? BinaryExpression as e) -> (parse e.Left) - (parse e.Right)
        | ExpressionType.Multiply, (:? BinaryExpression as e) -> (parse e.Left) * (parse e.Right)
        | ExpressionType.Divide, (:? BinaryExpression as e) -> (parse e.Left) / (parse e.Right)
        | ExpressionType.Constant, (:? ConstantExpression as e) -> Number (BigRational.FromBigInt (BigInteger (Convert.ToInt64(e.Value))))
        | ExpressionType.Parameter, (:? ParameterExpression as e) -> Identifier (Symbol e.Name)
        | ExpressionType.MemberAccess, (:? MemberExpression as e) -> Identifier (Symbol e.Member.Name)
        | ExpressionType.Lambda, (:? LambdaExpression as e) -> parse e.Body
        | ExpressionType.Try, (:? TryExpression as e) -> parse e.Body
        | ExpressionType.Convert, (:? UnaryExpression as e) -> parse e.Operand
        | _ -> failwith "not supported"
