namespace MathNet.Symbolics

open System.IO
open System.Text
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

type ParseResult =
    | ParsedExpression of Expression
    | ParseFailure of string


module private InfixParser =

    open Microsoft.FSharp.Reflection
    open FParsec
    open Operators

    type 'a parser = Parser<'a, unit>

    let ws = spaces
    let str_ws s = pstring s .>> ws
    let parens p = between (str_ws "(") (str_ws ")") p
    let abs p = between (str_ws "|") (str_ws "|") p |>> Expression.Abs

    let str_num : string parser =
        let options = NumberLiteralOptions.None
        numberLiteral options "number"
        |>> fun nl -> nl.String

    let integer : BigInteger parser =
        str_num
        |>> BigInteger.Parse

    let fraction : BigRational parser =
        str_num
        |>> fun num ->
            let ival = BigInteger.Parse num
            if ival = BigInteger.Zero then BigRational.Zero
            else BigRational.FromBigIntFraction (ival, BigInteger.Pow (10I, num.Length))

    let numberZ : Expression parser =
        integer .>> ws
        |>> Expression.FromInteger

    let numberQ : Expression parser =
        integer .>>. ((pstring "." >>. fraction) <|>% BigRational.Zero) .>> ws
        |>> fun (intPart, fractionPart) -> (BigRational.FromBigInt intPart) + fractionPart |> Expression.FromRational

    let number : Expression parser =
        let options = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowFractionWOIntegerPart ||| NumberLiteralOptions.AllowInfinity
        numberLiteral options "number" .>> ws
        |>> fun num ->
            if num.IsInfinity then Expression.Infinity
            elif num.IsInteger then BigInteger.Parse(num.String) |> Expression.FromInteger
            else Expression.Real(float num.String)

    let identifier : Expression parser =
        let isMathChar = function | '\u03C0' | '\u221E' | '\u29DD' -> true | _ -> false
        let isIdentifierFirstChar c = isLetter c || isMathChar c
        let isIdentifierChar c = isLetter c || isDigit c || isMathChar c
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
        |>> function // differentating between constants and identifiers
            | "pi" | "\u03C0" -> Expression.Constant Pi
            | "e" -> Expression.Constant E
            | "oo" | "inf" | "\u221E" -> Expression.Infinity // 'oo' from sympy
            | "\u29DD" -> Expression.ComplexInfinity
            | "j" -> Expression.Constant I
            | id -> Expression.Symbol id

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

    let parse text =
        match run parser text with
        | ParserResult.Success (result,_,_) -> ParsedExpression result
        | ParserResult.Failure (error,_,_) -> ParseFailure error


module private InfixPrinter =

    open Operators
    open ExpressionPatterns

    let functionName = function
        | Abs -> "abs"
        | Ln -> "ln"
        | Exp -> "exp"
        | ArcCos -> "acos" | ArcSin -> "asin" | ArcTan -> "atan"
        | Sinh -> "sinh" | Cosh -> "cosh" | Tanh -> "tanh"
        | Sin -> "sin" | Cos -> "cos" | Tan -> "tan"

    // priority: 1=additive 2=product 3=power

    // Strict Formatting:

    let rec strict write priority = function
        | Number n ->
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write "("
            write (n.ToString());
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write ")"
        | Identifier (Symbol name) -> write name
        | Undefined -> write "Undefined"
        | Infinity -> write "Infinity"
        | ComplexInfinity -> write "ComplexInfinity"
        | Constant E -> write "e"
        | Constant Pi -> write "pi"
        | Constant I -> write "j"
        | Constant (Constant.Real fp) ->
            if fp >= 0.0 then write (fp.ToString())
            else
                if priority > 0 then write "("
                write (fp.ToString());
                if priority > 0 then write ")"
        | Sum (x::xs) ->
            if priority > 1 then write "("
            strict write 1 x
            xs |> List.iter (fun x -> write " + "; strict write 1 x)
            if priority > 1 then write ")"
        | Product (x::xs) ->
            if priority > 2 then write "("
            strict write 2 x
            xs |> List.iter (fun x -> write "*"; strict write 2 x)
            if priority > 2 then write ")"
        | Power (r, p) ->
            if priority > 2 then write "("
            strict write 3 r
            write "^"
            strict write 3 p
            if priority > 2 then write ")"
        | Function (Abs, x) ->
            write "|"
            strict write 0 x
            write "|"
        | Function (fn, x) ->
            write (functionName fn)
            write "("
            strict write 0 x
            write ")"
        | FunctionN (fn, x::xs) ->
            write (functionName fn)
            write "("
            strict write 0 x
            xs |> List.iter (fun x -> write ","; strict write 0 x)
            write ")"
        | Sum [] | Product [] | FunctionN (_, []) -> failwith "invalid expression"


    // Nice Formatting:

    let rec numerator = function
        | NegRationalPower _ -> one
        | Product ax -> product <| List.map numerator ax
        | z -> z
    let rec denominator = function
        | NegRationalPower (r, p) -> r ** -p
        | Product ax -> product <| List.map denominator ax
        | _ -> one

    let rec private niceFractionPart write priority = function
        | Product (x::xs) ->
            if priority > 2 then write "("
            nice write 2 x
            xs |> List.iter (fun x -> write "*"; nice write 2 x)
            if priority > 2 then write ")"
        | x -> nice write priority x
    and private niceSummand write first = function
        | Number n as x when n.IsNegative ->
            write "-";
            nice write 1 (-x)
        | Product ((Number n)::xs) when n.IsNegative ->
            if first then write "-"; nice write 2 (product ((Number -n)::xs))
            else write " - "; nice write 2 (product ((Number -n)::xs))
        | Product _ as p ->
            if first then nice write 1 p
            else write " + "; nice write 1 p
        | x ->
            if first then nice write 1 x
            else write " + "; nice write 1 x
    and nice write priority = function
        | Number n ->
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write "("
            write (n.ToString());
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write ")"
        | Identifier (Symbol name) -> write name
        | Undefined -> write "Undefined"
        | Infinity -> write "\u221E" // "∞"
        | ComplexInfinity -> write "\u29DD" // "⧝"
        | Constant E -> write "e"
        | Constant Pi -> write "\u03C0" // "π"
        | Constant I -> write "j"
        | Constant (Constant.Real fp) ->
            if fp >= 0.0 then write (fp.ToString())
            else
                if priority > 0 then write "("
                write (fp.ToString());
                if priority > 0 then write ")"
        | Sum (x::xs) ->
            if priority > 1 then write "("
            niceSummand write true x
            xs |> List.iter (niceSummand write false)
            if priority > 1 then write ")"
        | Product (Number n::xs) when n.IsNegative ->
            write "-";
            nice write 2 (product ((Number -n)::xs))
        | Product _ as p ->
            let n = numerator p
            let d = denominator p
            if d = one then
                if priority > 2 then write "("
                niceFractionPart write 2 n
                if priority > 2 then write ")"
            else
                if priority > 2 then write "("
                niceFractionPart write 3 n
                write "/"
                niceFractionPart write 3 d
                if priority > 2 then write ")"
        | NegIntPower (r, p) ->
            if priority > 2 then write "("
            write "1/"
            nice write 3 r
            if (p <> Expression.MinusOne) then
                write "^"
                nice write 3 -p
            if priority > 2 then write ")"
        | Power (r, p) ->
            if priority > 3 then write "("
            nice write 4 r
            write "^"
            nice write 4 p
            if priority > 3 then write ")"
        | Function (Abs, x) ->
            write "|"
            nice write 0 x
            write "|"
        | Function (fn, x) ->
            write (functionName fn)
            write "("
            nice write 0 x
            write ")"
        | FunctionN (fn, x::xs) ->
            write (functionName fn)
            write "("
            nice write 0 x
            xs |> List.iter (fun x -> write ","; nice write 0 x)
            write ")"
        | Sum [] | Product [] | FunctionN (_, []) -> failwith "invalid expression"


/// Print and parse infix expression string
[<RequireQualifiedAccess>]
module Infix =

    /// Strict formatting, prints an exact representation of the expression tree
    [<CompiledName("PrintStrict")>]
    let printStrict q =
        let sb = StringBuilder()
        InfixPrinter.strict (sb.Append >> ignore) 0 q
        sb.ToString()

    /// Strict formatting, prints an exact representation of the expression tree
    [<CompiledName("PrintStrictToTextWriter")>]
    let printStrictTextWriter (writer:TextWriter) q = InfixPrinter.strict (writer.Write) 0 q

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("Print")>]
    let print q =
        let sb = StringBuilder()
        InfixPrinter.nice (sb.Append >> ignore) 0 q
        sb.ToString()

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("PrintToTextWriter")>]
    let printTextWriter (writer:TextWriter) q = InfixPrinter.nice (writer.Write) 0 q

    [<CompiledName("Parse")>]
    let parse (infix: string) = InfixParser.parse infix

    [<CompiledName("TryParse")>]
    let tryParse (infix: string) =
        match InfixParser.parse infix with
        | ParsedExpression expression -> Some expression
        | _ -> None

    [<CompiledName("ParseOrThrow")>]
    let parseOrThrow (infix: string) =
        match InfixParser.parse infix with
        | ParsedExpression expression -> expression
        | ParseFailure error -> failwith error

    [<CompiledName("ParseOrUndefined")>]
    let parseOrUndefined (infix: string) =
        match InfixParser.parse infix with
        | ParsedExpression expression -> expression
        | _ -> Expression.Undefined
