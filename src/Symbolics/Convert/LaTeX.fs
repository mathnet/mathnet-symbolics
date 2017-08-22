namespace MathNet.Symbolics

open System.IO
open System.Text
open MathNet.Symbolics

module private LaTeXFormatter =

    open Operators

    let culture = System.Globalization.CultureInfo.InvariantCulture

    // TODO
    let latexFunctionName = function
        | "abs" -> "\\operatorname{abs}"
        | "ln" -> "\\ln" | "log" -> "\\log_{10}"
        | "exp" -> "\\exp"
        | "sin" -> "\\sin" | "cos" -> "\\cos" | "tan" -> "\\tan"
        | "sinh" -> "\\sinh" | "cosh" -> "\\cosh" | "tanh" -> "\\tanh"
        | "asin" -> "\\arcsin" | "acos" -> "\\arccos" | "atan" -> "\\arctan"
        | "cot" -> "\\cot" | "sec" -> "\\sec" | "csc" -> "\\csc"
        | x -> sprintf "\\operatorname{%s}" x
    let latexFunctionNName = function
        | "log" -> "\\log"
        | "atan" -> "\\operatorname{atan2}"
        | x -> sprintf "\\operatorname{%s}" x

    let private dropParenthesis = function
        | VisualExpression.Parenthesis x -> x
        | x -> x

    let private addBracets (str : string) =
        let sb = StringBuilder()
        let mutable count = 0

        for c in str do
            sb.Append c |> ignore
            if c = '_' then
                sb.Append '{' |> ignore
                count <- count + 1

        new System.String('}', count)
        |> sb.Append |> ignore

        sb.ToString()

    let rec format write = function
        | VisualExpression.Symbol s ->
            match s with
            | "pi" -> write "\\pi"
            | name ->
                if name.Length > 1 then write "{"
                addBracets name |> write
                if name.Length > 1 then write "}"
        | VisualExpression.Infinity -> write "\\infty"
        | VisualExpression.ComplexInfinity -> write "\\infty"
        | VisualExpression.Undefined -> write "\\mathrm{undefined}"
        | VisualExpression.ComplexI -> write "\\jmath"
        | VisualExpression.PositiveInteger n -> write (n.ToString())
        | VisualExpression.PositiveFloatingPoint fp -> write (fp.ToString(culture))
        | VisualExpression.Parenthesis x ->
            write "\\left("
            format write x
            write "\\right)"
        | VisualExpression.Abs x ->
            write "\\left|"
            format write x
            write "\\right|"
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
            xs |> List.iter (function
                | VisualExpression.Power (VisualExpression.PositiveInteger _, _) as x ->
                    write "\cdot"
                    format write x
                | VisualExpression.Power (VisualExpression.PositiveFloatingPoint _, _) as x ->
                    write "\cdot"
                    format write x
                | x -> format write x)
         | VisualExpression.Fraction (n, d) ->
            write "\\frac{"
            format write (dropParenthesis n)
            write "}{"
            format write (dropParenthesis d)
            write "}"
        | VisualExpression.Power (r, p) ->
            write "{"
            format write r
            write "}"
            write "^"
            write "{"
            format write (dropParenthesis p)
            write "}"
        | VisualExpression.Function (fn, x) ->
            write (latexFunctionName fn)
            match x with
            | VisualExpression.Sum _ ->
                write "\\left("
                format write x
                write "\\right)"
            | _ ->
                write "{"
                format write x
                write "}"
        | VisualExpression.FunctionN ("log", [basis; x]) ->
            write "\\log_{"
            format write basis
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
        | VisualExpression.FunctionN (fn, x::xs) ->
            write (latexFunctionNName fn)
            write "\\left({"
            format write x
            xs |> List.iter (fun x -> write "}, {"; format write x)
            write "}\\right)"
        | VisualExpression.Sum [] | VisualExpression.Product [] | VisualExpression.FunctionN (_, []) -> failwith "invalid expression"


[<RequireQualifiedAccess>]
module LaTeX =

    let private defaultStyle = DefaultVisualStyle()

    [<CompiledName("FormatVisual")>]
    let formatVisual visualExpression =
        let sb = StringBuilder()
        LaTeXFormatter.format (sb.Append >> ignore) visualExpression
        sb.ToString()

    /// LaTeX output
    [<CompiledName("Format")>]
    let format expression =
        let sb = StringBuilder()
        let visual = VisualExpression.fromExpression defaultStyle expression
        LaTeXFormatter.format (sb.Append >> ignore) visual
        sb.ToString()

    [<CompiledName("FormatWriter")>]
    let formatWriter (writer:TextWriter) expression =
        let visual = VisualExpression.fromExpression defaultStyle expression
        LaTeXFormatter.format (writer.Write) visual
