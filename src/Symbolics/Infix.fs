namespace MathNet.Symbolics

open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics
open FParsec

[<RequireQualifiedAccess>]
module Infix =

    open Operators

    type 'a parser = Parser<'a, unit>

    let ws = spaces
    let str_ws s = pstring s .>> ws
    let parens p = between (str_ws "(") (str_ws ")") p
    let abs p = between (str_ws "|") (str_ws "|") p |>> Expression.Abs

    let number : Expression parser =
        let options = NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowPlusSign
        numberLiteral options "number"
        |>> fun nl -> BigInteger.Parse (nl.String) |> Expression.FromInteger

    let identifier : Expression parser =
        let isIdentifierFirstChar c = isLetter c
        let isIdentifierChar c = isLetter c || isDigit c
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
        |>> Expression.Symbol

    let value : Expression parser = number <|> identifier

    let functionName : Function parser =
        // TODO: consider reflection approach
        choice [ str_ws "abs" |>> fun _ -> Function.Abs
                 str_ws "ln"  |>> fun _ -> Function.Ln
                 str_ws "exp" |>> fun _ -> Function.Exp
                 str_ws "sin" |>> fun _ -> Function.Sin
                 str_ws "cos" |>> fun _ -> Function.Cos
                 str_ws "tan" |>> fun _ -> Function.Tan ] .>> ws

    let applyFunction = function
        | f, [arg] -> Expression.Apply(f, arg)
        | f, args -> Expression.ApplyN(f, args)

    let expression : Expression parser =

        let opp = OperatorPrecedenceParser<Expression,unit,unit>()
        let expr = opp.ExpressionParser

        let parensTerm = parens expr
        let absTerm = abs expr

        let functionArgsSimple = value |>> fun x -> [x]
        let functionArgsList = sepBy expr (str_ws ",") |> parens
        let functionTerm = functionName .>>. (functionArgsList <|> functionArgsSimple) |>> applyFunction

        let term = number <|> parensTerm <|> absTerm <|> attempt functionTerm <|> identifier

        opp.TermParser <- term
        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, add))
        opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, subtract))
        opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, multiply))
        opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, divide))
        opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, pow))
        opp.AddOperator(PrefixOperator("+", ws, 4, true, plus))
        opp.AddOperator(PrefixOperator("-", ws, 4, true, negate))
        expr

    let parser : Expression parser = ws >>. expression .>> eof
