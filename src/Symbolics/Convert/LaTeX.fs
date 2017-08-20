namespace MathNet.Symbolics

open System.IO
open System.Text
open MathNet.Symbolics

module private LaTeXHelper =
    open System
    let addBracets (str : string) =

        let sb = Text.StringBuilder()
        let mutable count = 0

        for c in str do
            sb.Append c |> ignore
            if c = '_' then
                sb.Append '{' |> ignore
                count <- count + 1

        new String('}', count)
        |> sb.Append |> ignore

        sb.ToString()

module private LaTeXFormatter =

    open Operators
    open ExpressionPatterns

    let culture = System.Globalization.CultureInfo.InvariantCulture

    // priority: 1=additive 2=product 3=power

    let rec numerator = function
        | NegPower _ -> one
        | Product ax -> product <| List.map numerator ax
        | z -> z
    let rec denominator = function
        | NegPower (r, p) -> r ** -p
        | Product ax -> product <| List.map denominator ax
        | _ -> one

    let functionName = function
        | Abs -> "\\mathrm{abs}"
        | Ln -> "\\ln" | Log -> "\\log"
        | Exp -> "\\exp"
        | Sin -> "\\sin" | Cos -> "\\cos" | Tan -> "\\tan"
        | Sinh -> "\\sinh" | Cosh -> "\\cosh" | Tanh -> "\\tanh"
        | Asin -> "\\arcsin" | Acos -> "\\arccos" | Atan -> "\\arctan"
        | Cot -> "\\cot" | Sec -> "\\sec" | Csc -> "\\csc"

    let private nextNumber = function
         | Power (Number _, _)
            -> true
         | _ -> false

    let rec texFractionPart write priority = function
        | Product (h::t) ->
            if priority > 2 then write "\\left("
            tex write 2 h
            match h with
            | Number _ when t.Head |> nextNumber ->
                    write "*"
            | _ -> ()
            t |> List.iter (fun x -> tex write 2 x)
            if priority > 2 then write "\\right)"
        | x -> tex write priority x
    and texSummand write first = function
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
    and tex write priority = function
        | Number n ->
            if n.IsInteger then
                if n.Sign >= 0 then write (n.ToString())
                else
                    if priority > 0 then write "\\left({"
                    write (n.ToString());
                    if priority > 0 then write "}\\right)"
            else
                if priority > 2 then write "\\left("
                write "\\frac{"
                write (n.Numerator.ToString());
                write "}{"
                write (n.Denominator.ToString());
                write "}"
                if priority > 2 then write "\\right)"
        | Approximation (Approximation.Real fp) ->
            if fp >= 0.0 then write (fp.ToString(culture))
            else
                if priority > 0 then write "\\left({"
                write (fp.ToString(culture));
                if priority > 0 then write "}\\right)"
        | Approximation (Approximation.Complex fp) ->
            write "\\left({"
            write (fp.ToString(culture));
            write "}\\right)"
        | Constant Pi -> write "\\pi"
        | Constant E -> write "e"
        | Constant I -> write "\\jmath"
        | ComplexInfinity -> write "\\infty"
        | PositiveInfinity -> write "\\infty"
        | NegativeInfinity ->
            if priority > 0 then write "\\left({"
            write "-\\infty"
            if priority > 0 then write "}\\right)"
        | Identifier (Symbol name) ->
            if name.Length > 1 then write "{"
            LaTeXHelper.addBracets name |> write
            if name.Length > 1 then write "}"
        | Undefined -> write "\\mathrm{undefined}"
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
            let n = numerator p
            let d = denominator p
            if isOne d then
                if priority > 2 then write "\\left("
                texFractionPart write 2 n
                if priority > 2 then write "\\right)"
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
        | Power (x, Power(n, minusOne)) when minusOne = Expression.MinusOne ->
            if priority > 3 then write "\\left("
            write "\\sqrt["
            tex write 4 n
            write "]{"
            tex write 4 x
            write "}"
            if priority > 3 then write "\\right)"
        | Power (r, p) ->
            if priority > 3 then write "\\left("
            write "{"
            tex write 4 r
            write "}"
            write "^"
            write "{"
            tex write 4 p
            write "}"
            if priority > 3 then write "\\right)"
        | Function (Abs, x) ->
            write "\\left|"
            tex write 0 x
            write "\\right|"
        | Function (Exp, x) ->
            if priority > 3 then write "\\left("
            write "\\mathrm{e}^"
            tex write 4 x
            if priority > 3 then write "\\right)"
        | Function (Log, x) ->
            if priority > 3 then write "\\left("
            write "\\log_{10}\\left("
            tex write 0 x
            write "\\right)"
            if priority > 3 then write "\\right)"
        | Function (fn, x) ->
            if priority > 3 then write "\\left("
            write (functionName fn)
            write "{"
            tex write 3 x
            write "}"
            if priority > 3 then write "\\right)"
        | FunctionN (Log, [basis; x]) ->
            if priority > 3 then write "\\left("
            write "\\log_{"
            tex write 0 basis
            write "}\\left("
            tex write 0 x
            write "\\right)"
            if priority > 3 then write "\\right)"
        | FunctionN (Atan, [y; x]) ->
            if priority > 3 then write "\\left("
            write "\\operatorname{atan2}\\left({{"
            tex write 0 y
            write "}, {"
            tex write 0 x
            write "}}\\right)"
            if priority > 3 then write "\\right)"
        | FunctionN (fn, x::xs) ->
            if priority > 3 then write "\\left("
            write (functionName fn)
            write "{\\left("
            tex write 0 x
            xs |> List.iter (fun x -> write ","; tex write 0 x)
            write "\\right)}"
            if priority > 3 then write "\\right)"
        | Sum [] | Product [] | FunctionN (_, []) -> failwith "invalid expression"



[<RequireQualifiedAccess>]
module LaTeX =

    /// LaTeX output
    [<CompiledName("Format")>]
    let format expression =
        let sb = StringBuilder()
        LaTeXFormatter.tex (sb.Append >> ignore) 0 expression
        sb.ToString()

    /// LaTeX output
    [<CompiledName("Print")>]
    [<System.Obsolete("Use Format instead")>]
    let print q = format q

    [<CompiledName("FormatWriter")>]
    let formatWriter (writer:TextWriter) expression = LaTeXFormatter.tex (writer.Write) 0 expression

    [<CompiledName("PrintToTextWriter")>]
    [<System.Obsolete("Use FormatWriter instead")>]
    let printTextWriter (writer:TextWriter) q = formatWriter writer q
