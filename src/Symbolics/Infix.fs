namespace MathNet.Symbolics

open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics
open FParsec

[<RequireQualifiedAccess>]
module Infix =

    open Microsoft.FSharp.Reflection
    open Operators

    type 'a parser = Parser<'a, unit>

    type ParseResult =
        | Success of Expression
        | Failure of string

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
        let cases =
            FSharpType.GetUnionCases typeof<Function>
            |> Array.map (fun case -> (case.Name.ToLower(), FSharpValue.MakeUnion(case, [||]) :?> Function))
        choice [ for (name, union) in cases -> str_ws name |>> fun _ -> union ] .>> ws

    let applyFunction = function
        | f, [arg] -> Expression.Apply(f, arg)
        | f, args -> Expression.ApplyN(f, args)

    let expression : Expression parser =

        let opp = OperatorPrecedenceParser<Expression,unit,unit>()
        let expr = opp.ExpressionParser

        let parensTerm = parens expr
        let absTerm = abs expr

        let functionArgs = sepBy expr (str_ws ",") |> parens
        let functionTerm = functionName .>>. functionArgs |>> applyFunction

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

    [<CompiledName("Parse")>]
    let parse (infix: string) =
        match run parser infix with
        | ParserResult.Success (result,_,_) -> Success result
        | ParserResult.Failure (error,_,_) -> Failure error

    [<CompiledName("TryParse")>]
    let tryParse (infix: string) =
        match run parser infix with
        | ParserResult.Success (result,_,_) -> Some result
        | _ -> None

    [<CompiledName("ParseOrThrow")>]
    let parseOrThrow (infix: string) =
        match run parser infix with
        | ParserResult.Success (result,_,_) -> result
        | ParserResult.Failure (error,_,_) -> failwith error

    [<CompiledName("ParseOrUndefined")>]
    let parseOrUndefined (infix: string) =
        match run parser infix with
        | ParserResult.Success (result,_,_) -> result
        | _ -> Expression.Undefined
