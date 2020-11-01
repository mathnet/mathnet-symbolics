namespace MathNet.Symbolics

open System.IO
open System.Text
open MathNet.Symbolics


module private InfixParser =

    open FParsec
    open Microsoft.FSharp.Core

    type 'a parser = Parser<'a, unit>

    let ws = spaces
    let str_ws s = pstring s .>> ws
    let parens p = between (str_ws "(") (str_ws ")") p |>> VisualExpression.Parenthesis
    let abs p = between (str_ws "|") (str_ws "|") p |>> VisualExpression.Abs

    let integer : BigInteger parser =
        let options = NumberLiteralOptions.None
        numberLiteral options "number" .>> ws
        |>> fun num -> BigInteger.Parse(num.String)

    let number : VisualExpression parser =
        let options =
            NumberLiteralOptions.AllowFraction
            ||| NumberLiteralOptions.AllowFractionWOIntegerPart
            ||| NumberLiteralOptions.AllowInfinity
            ||| NumberLiteralOptions.AllowExponent
        numberLiteral options "number" .>> ws
        |>> fun num ->
            if num.IsInfinity then VisualExpression.Infinity
            elif num.IsInteger then BigInteger.Parse(num.String) |> VisualExpression.PositiveInteger
            else VisualExpression.PositiveFloatingPoint(float num.String)

    let symbolName : string parser =
        let isSymbolFirstChar c = isLetter c
        let isSymbolChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isSymbolFirstChar isSymbolChar "symbol" .>> ws

    let identifier : VisualExpression parser =
        let isMathChar = function | '\u03C0' | '\u221E' | '\u29DD' -> true | _ -> false
        let isIdentifierFirstChar c = isLetter c || isMathChar c
        let isIdentifierChar c = isLetter c || isDigit c || isMathChar c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
        |>> function // differentating between constants and identifiers
            | "pi" | "\u03C0" -> VisualExpression.RealPi
            | "e" -> VisualExpression.RealE
            | "oo" | "inf" | "\u221E" -> VisualExpression.Infinity // 'oo' from sympy
            | "\u29DD" -> VisualExpression.ComplexInfinity
            | "j" -> VisualExpression.ComplexI
            | id -> VisualExpression.Symbol id

    let expression : VisualExpression parser =

        let opp = OperatorPrecedenceParser<VisualExpression,unit,unit>()
        let expr = opp.ExpressionParser

        let functionArgs = sepBy expr (str_ws ",") |> between (str_ws "(") (str_ws ")")

        let functionTerm = symbolName .>>. functionArgs |>> function
            | f, args -> VisualExpression.Function (f, BigInteger.One, args)

        let functionPowerTerm = symbolName .>>. (str_ws "^" >>. integer) .>>. functionArgs |>> function
            | (f, power), args -> VisualExpression.Function (f, power, args)

        let sqrtTerm = str_ws "sqrt" >>. (between (str_ws "(") (str_ws ")") expr) |>> function
            | arg -> VisualExpression.Root (arg, bigint 2)

        let powTerm = str_ws "pow" >>. functionArgs |>> function
            | [VisualExpression.Negative _ as a; b]
            | [VisualExpression.Sum _ as a; b]
            | [VisualExpression.Product _ as a; b]
            | [VisualExpression.Fraction _ as a; b]
            | [VisualExpression.Power _ as a; b]
            | [VisualExpression.Function _ as a; b]
                -> VisualExpression.Power (VisualExpression.Parenthesis a, b)
            | [a; b] -> VisualExpression.Power (a, b)
            | _ -> failwith "Pow expects exactly two arguments"

        let term =
            number <|> parens expr <|> abs expr
            <|> attempt sqrtTerm <|> attempt powTerm
            <|> attempt functionTerm <|> attempt functionPowerTerm
            <|> identifier

        let sum a b = match a, b with | VisualExpression.Sum ax, b -> VisualExpression.Sum (ax @ [b]) | a, b -> VisualExpression.Sum [a; b]
        let product a b = match a, b with | VisualExpression.Product ax, b -> VisualExpression.Product (ax @ [b]) | a, b -> VisualExpression.Product [a; b]
        let fraction a b =
            let patchParanthesis = function
                | VisualExpression.Parenthesis (VisualExpression.Fraction _) as x -> x
                | VisualExpression.Parenthesis x -> x
                | x -> x
            VisualExpression.Fraction (patchParanthesis a, patchParanthesis b)

        opp.TermParser <- term
        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, sum))
        opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun a b -> sum a (VisualExpression.Negative b)))
        opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, product))
        opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fraction))
        opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, fun a b -> VisualExpression.Power (a, b)))
        opp.AddOperator(PrefixOperator("+", ws, 4, true, id))
        opp.AddOperator(PrefixOperator("-", ws, 4, true, VisualExpression.Negative))
        expr

    let parser : VisualExpression parser = ws >>. expression .>> eof

    let parse text : Result<VisualExpression, string> =
        match run parser text with
        | ParserResult.Success (result,_,_) -> Ok result
        | ParserResult.Failure (error,_,_) -> Error error


module private InfixFormatter =

    open Operators

    let culture = System.Globalization.CultureInfo.InvariantCulture

    let private dropParenthesis = function
        | VisualExpression.Parenthesis x -> x
        | x -> x

    let rec format write = function
        | VisualExpression.Symbol s ->
            match s with
            | "pi" -> write "\u03C0" // "π"
            | x -> write x
        | VisualExpression.Infinity -> write "\u221E" // "∞"
        | VisualExpression.ComplexInfinity -> write "\u29DD" // "⧝"
        | VisualExpression.Undefined -> write "Undefined"
        | VisualExpression.ComplexI -> write "j"
        | VisualExpression.RealE -> write "e"
        | VisualExpression.RealPi -> write "\u03C0" // "π"
        | VisualExpression.PositiveInteger n -> write (n.ToString())
        | VisualExpression.PositiveFloatingPoint fp ->
            let s = fp.ToString(culture)
            if s.IndexOf('.') = -1 then write (s + ".0") else write s
        | VisualExpression.Parenthesis x ->
            write "("
            format write x
            write ")"
        | VisualExpression.Abs x ->
            write "|"
            format write x
            write "|"
        | VisualExpression.Negative x ->
            write "-"
            format write x
        | VisualExpression.Sum (x::xs) ->
            format write x
            xs |> List.iter (function
                | VisualExpression.Negative x -> write " - "; format write x
                | x -> write " + "; format write x)
        | VisualExpression.Product (x::xs) ->
            format write x
            xs |> List.iter (fun x -> write "*"; format write x)
        | VisualExpression.Fraction (n, d) ->
            format write n
            write "/"
            format write d
        | VisualExpression.Power (r, p) ->
            format write r
            write "^"
            format write p
        | VisualExpression.Root (r, p) when p = bigint 2 ->
            write "sqrt("
            format write (dropParenthesis r)
            write ")"
        | VisualExpression.Root (r, p) ->
            format write r
            write "^(1/"
            write (p.ToString())
            write ")"
        | VisualExpression.Function (fn, power, [x]) ->
            write fn
            if power.IsOne |> not then
                write "^"
                write (power.ToString())
            write "("
            format write x
            write ")"
        | VisualExpression.Function (fn, power, x::xs) ->
            write fn
            if power.IsOne |> not then
                write "^"
                write (power.ToString())
            write "("
            format write x
            xs |> List.iter (fun x -> write ","; format write x)
            write ")"
        | VisualExpression.Sum [] | VisualExpression.Product [] | VisualExpression.Function (_, _, []) -> failwith "invalid expression"


/// Print and parse infix expression string
[<RequireQualifiedAccess>]
module Infix =

    open Microsoft.FSharp.Core

    let defaultStyle = { VisualExpressionStyle.CompactPowersOfFunctions = false }

    [<CompiledName("FormatVisual")>]
    let formatVisual visualExpression =
        let sb = StringBuilder()
        InfixFormatter.format (sb.Append >> ignore) visualExpression
        sb.ToString()

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("FormatStyle")>]
    let formatStyle visualStyle expression =
        let sb = StringBuilder()
        let visual = VisualExpression.fromExpression visualStyle expression
        InfixFormatter.format (sb.Append >> ignore) visual
        sb.ToString()

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("Format")>]
    let format expression = formatStyle defaultStyle expression

    [<CompiledName("FormatVisualWriter")>]
    let formatVisualWriter (writer:TextWriter) visualExpression =
        InfixFormatter.format (writer.Write) visualExpression

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("FormatStyleWriter")>]
    let formatStyleWriter visualStyle (writer:TextWriter) expression =
        VisualExpression.fromExpression visualStyle expression |> formatVisualWriter writer

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("FormatWriter")>]
    let formatWriter (writer:TextWriter) expression =
        VisualExpression.fromExpression defaultStyle expression |> formatVisualWriter writer

    [<CompiledName("ParseVisual")>]
    let parseVisual (infix: string) =
        InfixParser.parse infix

    [<CompiledName("Parse")>]
    let parse (infix: string) =
        parseVisual infix |> Result.map VisualExpression.toExpression

    [<CompiledName("TryParse")>]
    let tryParse (infix: string) =
        match parse infix with
        | Ok expression -> Some expression
        | _ -> None

    [<CompiledName("ParseOrThrow")>]
    let parseOrThrow (infix: string) =
        match parse infix with
        | Ok expression -> expression
        | Error error -> failwith error

    [<CompiledName("ParseOrUndefined")>]
    let parseOrUndefined (infix: string) =
        match parse infix with
        | Ok expression -> expression
        | _ -> Expression.Undefined
