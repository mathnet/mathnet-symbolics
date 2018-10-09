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
    open System.Reflection

    type Pseudo =
        | Sqrt
        | Pow

    type 'a parser = Parser<'a, unit>

    let ws = spaces
    let str_ws s = pstring s .>> ws
    let parens p = between (str_ws "(") (str_ws ")") p
    let abs p = between (str_ws "|") (str_ws "|") p |>> Expression.Abs

    let number : Expression parser =
        let options =
            NumberLiteralOptions.AllowFraction
            ||| NumberLiteralOptions.AllowFractionWOIntegerPart
            ||| NumberLiteralOptions.AllowInfinity
            ||| NumberLiteralOptions.AllowExponent

        numberLiteral options "number" .>> ws
        |>> fun num ->
            if num.IsInfinity then Expression.PositiveInfinity
            elif num.IsInteger then BigInteger.Parse(num.String) |> Expression.FromInteger
            else Expression.Real(float num.String)

    let identifier : Expression parser =
        let isMathChar = function | '\u03C0' | '\u221E' | '\u29DD' -> true | _ -> false
        let isIdentifierFirstChar c = isLetter c || isMathChar c
        let isIdentifierChar c = isLetter c || isDigit c || isMathChar c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
        |>> function // differentating between constants and identifiers
            | "pi" | "\u03C0" -> Expression.Constant Pi
            | "e" -> Expression.Constant E
            | "oo" | "inf" | "\u221E" -> Expression.PositiveInfinity // 'oo' from sympy
            | "\u29DD" -> Expression.ComplexInfinity
            | "j" -> Expression.Constant I
            | id -> Expression.Symbol id

    let value : Expression parser = number <|> identifier

    let functionName : Function parser =
        let cases =
            FSharpType.GetUnionCases typeof<Function>
            |> Array.map
                (fun case -> (case.Name.ToLower(), FSharpValue.MakeUnion(case, [||]) :?> Function))
            |> Array.sortBy (fun (name,_) -> -name.Length)

        choice [ for (name, union) in cases -> str_ws name |>> fun _ -> union ] .>> ws

    let applyFunction = function
        | f, [arg] -> Expression.Apply(f, arg)
        | f, args -> Expression.ApplyN(f, args)

    let pseudoName : Pseudo parser =
        let flags = BindingFlags.NonPublic ||| BindingFlags.Public
        let cases =
            FSharpType.GetUnionCases (typeof<Pseudo>, flags)
            |> Array.map
                (fun case -> (case.Name.ToLower(), FSharpValue.MakeUnion(case, [||], flags) :?> Pseudo))
            |> Array.sortBy (fun (name,_) -> -name.Length)

        choice [ for (name, union) in cases -> str_ws name |>> fun _ -> union ] .>> ws

    let applyPseudo =
        function
        | Sqrt, [arg] -> arg |> Operators.sqrt
        | Pow, [x;y] -> (x,y) |> Expression.Pow
        | _ -> failwith "wrong matching"

    let expression : Expression parser =

        let opp = OperatorPrecedenceParser<Expression,unit,unit>()
        let expr = opp.ExpressionParser

        let parensTerm = parens expr
        let absTerm = abs expr

        let functionArgs = sepBy expr (str_ws ",") |> parens
        let functionTerm = functionName .>>. functionArgs |>> applyFunction

        let pseudoTerm = pseudoName .>>. functionArgs |>> applyPseudo

        let term = number <|> parensTerm <|> absTerm <|> attempt functionTerm <|> attempt pseudoTerm  <|> identifier

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


module private InfixFormatter =

    open Operators
    open ExpressionPatterns

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
        | VisualExpression.PositiveInteger n -> write (n.ToString())
        | VisualExpression.PositiveFloatingPoint fp -> write (fp.ToString(culture))
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
        | VisualExpression.Function (fn, x) ->
            write fn
            write "("
            format write x
            write ")"
        | VisualExpression.FunctionN (fn, x::xs) ->
            write fn
            write "("
            format write x
            xs |> List.iter (fun x -> write ","; format write x)
            write ")"
        | VisualExpression.Sum [] | VisualExpression.Product [] | VisualExpression.FunctionN (_, []) -> failwith "invalid expression"

    let functionName = function
        | Abs -> "abs"
        | Ln -> "ln" | Log -> "log"
        | Exp -> "exp"
        | Sin -> "sin" | Cos -> "cos" | Tan -> "tan"
        | Csc -> "csc" | Sec -> "sec" | Cot -> "cot"
        | Sinh -> "sinh" | Cosh -> "cosh" | Tanh -> "tanh"
        | Csch -> "csch" | Sech -> "sech" | Coth -> "coth"
        | Acos -> "acos" | Asin -> "asin" | Atan -> "atan"
        | Acsc -> "acsc" | Asec -> "asec" | Acot -> "acot"
        | Acosh -> "acosh" | Asinh -> "asinh" | Atanh -> "atanh"
        | Asech -> "asech" | Acsch -> "acsch" | Acoth -> "acoth"
        | BesselJ -> "besselj"
        | BesselI -> "besseli"
        | BesselY -> "bessely"
        | BesselK -> "besselk"
        | HankelH1 -> "hankelh1"
        | HankelH2 -> "hankelh2"

    // priority: 1=additive 2=product 3=power

    // Strict Formatting:

    let rec strict write priority = function
        | Number n ->
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write "("
            write (n.ToString());
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write ")"
        | Identifier (Symbol name) -> write name
        | Undefined -> write "Undefined"
        | ComplexInfinity -> write "ComplexInfinity"
        | PositiveInfinity -> write "Infinity"
        | NegativeInfinity ->
            if priority > 0 then write "("
            write "-Infinity"
            if priority > 0 then write ")"
        | Constant E -> write "e"
        | Constant Pi -> write "pi"
        | Constant I -> write "j"
        | Approximation (Approximation.Real fp) ->
            if fp >= 0.0 then write (fp.ToString(culture))
            else
                if priority > 0 then write "("
                write (fp.ToString(culture));
                if priority > 0 then write ")"
        | Approximation (Approximation.Complex fp) ->
            write "("
            write (fp.ToString(culture));
            write ")"
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



/// Print and parse infix expression string
[<RequireQualifiedAccess>]
module Infix =

    let private defaultStyle = DefaultVisualStyle()

    /// Strict formatting, prints an exact representation of the expression tree
    [<CompiledName("FormatStrict")>]
    let formatStrict expression =
        let sb = StringBuilder()
        InfixFormatter.strict (sb.Append >> ignore) 0 expression
        sb.ToString()

    /// Strict formatting, prints an exact representation of the expression tree
    [<CompiledName("FormatStrictWriter")>]
    let formatStrictWriter (writer:TextWriter) expression = InfixFormatter.strict (writer.Write) 0 expression

    [<CompiledName("FormatVisual")>]
    let formatVisual visualExpression =
        let sb = StringBuilder()
        InfixFormatter.format (sb.Append >> ignore) visualExpression
        sb.ToString()

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("Format")>]
    let format expression =
        let sb = StringBuilder()
        let visual = VisualExpression.fromExpression defaultStyle expression
        InfixFormatter.format (sb.Append >> ignore) visual
        sb.ToString()

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("FormatWriter")>]
    let formatWriter (writer:TextWriter) expression =
        let visual = VisualExpression.fromExpression defaultStyle expression
        InfixFormatter.format (writer.Write) visual

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
