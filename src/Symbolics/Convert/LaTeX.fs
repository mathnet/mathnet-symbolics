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

    // TODO
    let latexFunctionName = function
        | "abs" -> "\\mathrm{abs}"
        | "ln" -> "\\ln" | "log" -> "\\log_{10}"
        | "exp" -> "\\exp"
        | "sin" -> "\\sin" | "cos" -> "\\cos" | "tan" -> "\\tan"
        | "sinh" -> "\\sinh" | "cosh" -> "\\cosh" | "tanh" -> "\\tanh"
        | "asin" -> "\\arcsin" | "acos" -> "\\arccos" | "atan" -> "\\arctan"
        | "cot" -> "\\cot" | "sec" -> "\\sec" | "csc" -> "\\csc"
        | x -> sprintf "\\mathrm{%s}" x
    let latexFunctionNName = function
        | "log" -> "\\log"
        | "atan" -> "\\mathrm{atan2}"
        | x -> sprintf "\\mathrm{%s}" x

    let private dropParenthesis = function
        | VisualExpression.Parenthesis x -> x
        | x -> x

    let rec visual write = function
        | VisualExpression.Symbol s ->
            match s with
            | "pi" -> write "\\pi"
            | name ->
                if name.Length > 1 then write "{"
                LaTeXHelper.addBracets name |> write
                if name.Length > 1 then write "}"
        | VisualExpression.Infinity -> write "\\infty"
        | VisualExpression.ComplexInfinity -> write "\\infty"
        | VisualExpression.Undefined -> write "\\mathrm{undefined}"
        | VisualExpression.ComplexI -> write "\\jmath"
        | VisualExpression.PositiveInteger n -> write (n.ToString())
        | VisualExpression.PositiveFloatingPoint fp -> write (fp.ToString(culture))
        | VisualExpression.Parenthesis x ->
            write "\\left("
            visual write x
            write "\\right)"
        | VisualExpression.Abs x ->
            write "\\left|"
            visual write x
            write "\\right|"
        | VisualExpression.Negative x ->
            write "-"
            visual write x
        | VisualExpression.Sum (x::xs) ->
            visual write x
            xs |> List.iter (function
                | VisualExpression.Negative x -> write " - "; visual write x
                | x -> write " + "; visual write x)
        | VisualExpression.Product (x::xs) ->
            visual write x
            xs |> List.iter (function
                | VisualExpression.Power (VisualExpression.PositiveInteger _, _) as x ->
                    write "\cdot"
                    visual write x
                | VisualExpression.Power (VisualExpression.PositiveFloatingPoint _, _) as x ->
                    write "\cdot"
                    visual write x
                | x -> visual write x)
         | VisualExpression.Fraction (n, d) ->
            write "\\frac{"
            visual write (dropParenthesis n)
            write "}{"
            visual write (dropParenthesis d)
            write "}"
        | VisualExpression.Power (r, p) ->
            write "{"
            visual write r
            write "}"
            write "^"
            write "{"
            visual write (dropParenthesis p)
            write "}"
        | VisualExpression.Function (fn, x) ->
            write (latexFunctionName fn)
            match x with
            | VisualExpression.Sum _ ->
                write "\\left("
                visual write x
                write "\\right)"
            | _ ->
                write "{"
                visual write x
                write "}"
        | VisualExpression.FunctionN ("log", [basis; x]) ->
            write "\\log_{"
            visual write basis
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                visual write x
                write "\\right)"
            | _ ->
                write "}{"
                visual write x
                write "}"
        | VisualExpression.FunctionN (fn, x::xs) ->
            write (latexFunctionNName fn)
            write "\\left({"
            visual write x
            xs |> List.iter (fun x -> write "}, {"; visual write x)
            write "}\\right)"
        | VisualExpression.Sum [] | VisualExpression.Product [] | VisualExpression.FunctionN (_, []) -> failwith "invalid expression"


[<RequireQualifiedAccess>]
module LaTeX =

    let private defaultStyle = DefaultVisualStyle()

    /// LaTeX output
    [<CompiledName("Format")>]
    let format expression =
        let sb = StringBuilder()
        let visual = VisualExpression.fromExpression defaultStyle expression
        LaTeXFormatter.visual (sb.Append >> ignore) visual
        sb.ToString()

    /// LaTeX output
    [<CompiledName("Print")>]
    [<System.Obsolete("Use Format instead")>]
    let print q = format q

    [<CompiledName("FormatWriter")>]
    let formatWriter (writer:TextWriter) expression =
        let visual = VisualExpression.fromExpression defaultStyle expression
        LaTeXFormatter.visual (writer.Write) visual

    [<CompiledName("PrintToTextWriter")>]
    [<System.Obsolete("Use FormatWriter instead")>]
    let printTextWriter (writer:TextWriter) q = formatWriter writer q
