namespace MathNet.Symbolics

open System.IO
open System.Text
open MathNet.Symbolics

[<RequireQualifiedAccess>]
module Print =

    open Operators
    open ExpressionPatterns

    let private functionName = function
        | Abs -> "abs"
        | Ln -> "ln" | Exp -> "exp"
        | Sin -> "sin" | Cos -> "cos" | Tan -> "tan"

    // priority: 1=additive 2=product 3=power

    // Strict Formatting:

    let rec private strict write priority = function
        | Number n ->
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write "("
            write (n.ToString());
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write ")"
        | Identifier (Symbol name) -> write name
        | Undefined -> write "Undefined"
        | PositiveInfinity -> write "PositiveInfinity"
        | NegativeInfinity -> write "NegativeInfinity"
        | ComplexInfinity -> write "ComplexInfinity"
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

     /// Strict formatting, prints an exact representation of the expression tree
    [<CompiledName("FormatStrict")>]
    let formatStrict q =
        let sb = StringBuilder()
        strict (sb.Append >> ignore) 0 q
        sb.ToString()

    [<CompiledName("FormatStrictToTextWriter")>]
    let formatStrictTextWriter (writer:TextWriter) q = strict (writer.Write) 0 q


    // Nice Formatting:

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
    and private nice write priority = function
        | Number n ->
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write "("
            write (n.ToString());
            if not(n.IsInteger) && priority > 1 || n.IsInteger && priority > 0 && n.Sign < 0 then write ")"
        | Identifier (Symbol name) -> write name
        | Undefined -> write "Undefined"
        | PositiveInfinity -> write "PositiveInfinity"
        | NegativeInfinity -> write "NegativeInfinity"
        | ComplexInfinity -> write "ComplexInfinity"
        | Sum (x::xs) ->
            if priority > 1 then write "("
            niceSummand write true x
            xs |> List.iter (niceSummand write false)
            if priority > 1 then write ")"
        | Product (Number n::xs) when n.IsNegative ->
            write "-";
            nice write 2 (product ((Number -n)::xs))
        | Product _ as p ->
            let n = Rational.numerator p
            let d = Rational.denominator p
            if d = one then
                niceFractionPart write 2 n
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

    /// Nicer human readable but slightly denormalized output
    [<CompiledName("Format")>]
    let format q =
        let sb = StringBuilder()
        nice (sb.Append >> ignore) 0 q
        sb.ToString()

    [<CompiledName("FormatToTextWriter")>]
    let formatTextWriter (writer:TextWriter) q = nice (writer.Write) 0 q


    // LaTeX Formatting

    let rec private texFractionPart write priority = function
        | Product (x) ->
            if priority > 2 then write "\\left("
            x |> List.iter (fun x -> tex write 2 x)
            if priority > 2 then write "\\right)"
        | x -> tex write priority x
    and private texSummand write first = function
        | Number n as x when n.IsNegative ->
            write "-";
            tex write 1 (-x)
        | Product ((Number n)::xs) when n.IsNegative ->
            if first then write "-"; tex write 2 (product ((Number -n)::xs))
            else write " - "; tex write 2 (product ((Number -n)::xs))
        | Product _ as p ->
            if first then tex write 1 p
            else write " + "; tex write 1 p
        | x ->
            if first then tex write 1 x
            else write " + "; tex write 1 x
    and private tex write priority = function
        | Number n ->
            if n.IsInteger then
                if n.Sign >= 0 then write (n.ToString());
                else
                    if priority > 0 then write "\\left("
                    write "{"
                    write (n.ToString());
                    write "}"
                    if priority > 0 then write "\\right)"
            else
                if priority > 2 then write "\\left("
                write "\\frac{"
                write (n.Numerator.ToString());
                write "}{"
                write (n.Denominator.ToString());
                write "}"
                if priority > 2 then write "\\right)"
        | Identifier (Symbol name) -> write name
        | Undefined -> write "\\mathrm{undefined}"
        | PositiveInfinity -> write "\\infty"
        | NegativeInfinity ->
            if priority > 0 then write "\\left("
            write "-\\infty"
            if priority > 0 then write "\\right)"
        | ComplexInfinity -> write "\\infty"
        | Sum (x::xs) ->
            if priority > 1 then write "\\left("
            texSummand write true x
            xs |> List.iter (texSummand write false)
            if priority > 1 then write "\\right)"
        | Product (Number n::xs) when n.IsNegative ->
            if priority > 1 then write "\\left("
            write "-";
            tex write 2 (product ((Number -n)::xs))
            if priority > 1 then write "\\right)"
        | Product _ as p ->
            let n = Rational.numerator p
            let d = Rational.denominator p
            if d = one then
                texFractionPart write 2 n
            else
                if priority > 2 then write "\\left("
                write "\\frac{"
                texFractionPart write 0 n
                write "}{"
                texFractionPart write 0 d
                write "}"
                if priority > 2 then write "\\right)"
        | NegIntPower (r, p) ->
            if priority > 2 then write "\\left("
            write "\\frac{1}{"
            tex write 3 r
            if (p <> Expression.MinusOne) then
                write "^"
                tex write 3 -p
            write "}"
            if priority > 2 then write "\\right)"
        | Power (r, p) ->
            if priority > 3 then write "\\left("
            tex write 4 r
            write "^"
            tex write 4 p
            if priority > 3 then write "\\right)"
        | Function (Abs, x) ->
            write "\\left|"
            tex write 0 x
            write "\\right|"
        | Function (fn, x) ->
            if priority > 3 then write "\\left("
            write "\\mathrm{"
            write (functionName fn)
            write "}\\,"
            tex write 3 x
            if priority > 3 then write "\\right)" else write "\\;"
        | FunctionN (fn, x::xs) ->
            if priority > 3 then write "\\left("
            write "\\mathrm{"
            write (functionName fn)
            write "}\\left("
            tex write 0 x
            xs |> List.iter (fun x -> write ","; tex write 0 x)
            write "\\right)"
            if priority > 3 then write "\\right)"
        | Sum [] | Product [] | FunctionN (_, []) -> failwith "invalid expression"

    /// LaTeX output
    [<CompiledName("LaTeX")>]
    let latex q =
        let sb = StringBuilder()
        tex (sb.Append >> ignore) 0 q
        sb.ToString()

    [<CompiledName("LaTeXToTextWriter")>]
    let latexTextWriter (writer:TextWriter) q = tex (writer.Write) 0 q
