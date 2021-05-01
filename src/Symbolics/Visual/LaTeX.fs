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
        | "ln" -> "\\ln" | "lg" -> "\\log_{10}"
        | "exp" -> "\\exp"
        | "sin" -> "\\sin" | "cos" -> "\\cos" | "tan" -> "\\tan"
        | "csc" -> "\\csc" | "sec" -> "\\sec" | "cot" -> "\\cot"
        | "sinh" -> "\\sinh" | "cosh" -> "\\cosh" | "tanh" -> "\\tanh"
        | "csch" -> "\\operatorname{csch}" | "sech" -> "\\operatorname{sech}" | "coth" -> "\\coth"
        | "asin" -> "\\arcsin" | "acos" -> "\\arccos" | "atan" -> "\\arctan"
        | "acsc" -> "\\arccsc" | "asec" -> "\\arcsec" | "acot" -> "\\arccot"
        | "asinh" -> "\\operatorname{arsinh}" | "acosh" -> "\\operatorname{arcosh}" | "atanh" -> "\\operatorname{artanh}"
        | "acsch" -> "\\operatorname{arcsch}" | "asech" -> "\\operatorname{arsech}" | "acoth" -> "\\operatorname{arcoth}"
        | "airyai" -> "Ai" | "airyaiprime" -> "Ai^\\prime"
        | "airybi" -> "Bi" | "airybiprime" -> "Bi^\\prime"
        | x -> sprintf "\\operatorname{%s}" x
    let latexFunctionNName = function
        | "log" -> "\\log"
        | "min" -> "\\operatorname{min}"
        | "max" -> "\\operatorname{max}"
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
                if name.Length > 1 then write "\\mathrm{"
                addBracets name |> write
                if name.Length > 1 then write "}"
        | VisualExpression.Infinity -> write "\\infty"
        | VisualExpression.ComplexInfinity -> write "\\infty"
        | VisualExpression.Undefined -> write "\\mathrm{undefined}"
        | VisualExpression.ComplexI -> write "\\jmath"
        | VisualExpression.RealE -> write "e"
        | VisualExpression.RealPi -> write "\\pi"
        | VisualExpression.PositiveInteger n -> write (n.ToString())
        | VisualExpression.PositiveFloatingPoint fp ->
            let s = fp.ToString(culture)
            if s.IndexOf('.') = -1 then write (s + ".0") else write s
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
        | VisualExpression.Product ((x::xs) as xx) when (xx |> List.exists (function | VisualExpression.Symbol s when s.Length > 1 && s <> "pi" -> true | _ -> false)) ->
            format write x
            xs |> List.iter (fun x ->
                write " \cdot "
                format write x)
        | VisualExpression.Product (x::xs) ->
            format write x
            xs |> List.iter (function
                | VisualExpression.Power (VisualExpression.PositiveInteger _, _) as x ->
                    write " \cdot "
                    format write x
                | VisualExpression.Power (VisualExpression.PositiveFloatingPoint _, _) as x ->
                    write " \cdot "
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
        | VisualExpression.Root (r, p) when p = bigint 2 ->
            write "\\sqrt{"
            format write (dropParenthesis r)
            write "}"
        | VisualExpression.Root (r, p) ->
            write "\\sqrt["
            write (p.ToString())
            write "]{"
            format write (dropParenthesis r)
            write "}"
        | VisualExpression.Function (fn, power, [x]) ->
            write (latexFunctionName fn)
            if power.IsOne |> not then
                write "^{"
                write (power.ToString())
                write "}"
            match x with
            | VisualExpression.Sum _ ->
                write "\\left("
                format write x
                write "\\right)"
            | _ ->
                write "{"
                format write x
                write "}"
        | VisualExpression.Function ("log", power, [basis; x]) ->
            write "\\log"
            if power.IsOne |> not then
                write "^{"
                write (power.ToString())
                write "}"
            write "_{"
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
        | VisualExpression.Function ("besselj", power, [nu; x]) when power.IsOne ->
            write "J_{"
            format write nu
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
        | VisualExpression.Function ("bessely", power, [nu; x]) when power.IsOne ->
            write "Y_{"
            format write nu
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
        | VisualExpression.Function ("besseli", power, [nu; x]) when power.IsOne ->
            write "I_{"
            format write nu
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
        | VisualExpression.Function ("besselk", power, [nu; x]) when power.IsOne ->
            write "K_{"
            format write nu
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
        | VisualExpression.Function ("besseliratio", power, [nu; x]) when power.IsOne ->
            write "\\frac{"
            write "I_{"
            format write nu
            write " + 1"
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
            write "}{"
            write "I_{"
            format write nu
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
            write "}"
        | VisualExpression.Function ("besselkratio", power, [nu; x]) when power.IsOne  ->
            write "\\frac{"
            write "K_{"
            format write nu
            write " + 1"
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
            write "}{"
            write "K_{"
            format write nu
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
            write "}"
        | VisualExpression.Function ("hankelh1", power, [nu; x]) when power.IsOne ->
            write "H^{(1)}_{"
            format write nu
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
        | VisualExpression.Function ("hankelh2", power, [nu; x]) when power.IsOne ->
            write "H^{(2)}_{"
            format write nu
            match x with
            | VisualExpression.Sum _ ->
                write "}\\left("
                format write x
                write "\\right)"
            | _ ->
                write "}{"
                format write x
                write "}"
        | VisualExpression.Function (fn, power, x::xs) ->
            write (latexFunctionNName fn)
            if power.IsOne |> not then
                write "^{"
                write (power.ToString())
                write "}"
            write "\\left({"
            format write x
            xs |> List.iter (fun x -> write "}, {"; format write x)
            write "}\\right)"
        | VisualExpression.Sum [] | VisualExpression.Product [] | VisualExpression.Function (_, _, []) -> failwith "invalid expression"


[<RequireQualifiedAccess>]
module LaTeX =

    let defaultStyle = { VisualExpressionStyle.CompactPowersOfFunctions = true }

    [<CompiledName("FormatVisual")>]
    let formatVisual visualExpression =
        let sb = StringBuilder()
        LaTeXFormatter.format (sb.Append >> ignore) visualExpression
        sb.ToString()

    /// LaTeX output
    [<CompiledName("Format")>]
    let formatStyle visualStyle expression =
        let sb = StringBuilder()
        let visual = VisualExpression.fromExpression visualStyle expression
        LaTeXFormatter.format (sb.Append >> ignore) visual
        sb.ToString()

    /// LaTeX output
    [<CompiledName("Format")>]
    let format expression = formatStyle defaultStyle expression

    [<CompiledName("FormatVisualWriter")>]
    let formatVisualWriter (writer:TextWriter) visualExpression =
        LaTeXFormatter.format (writer.Write) visualExpression

    [<CompiledName("FormatStyleWriter")>]
    let formatStyleWriter visualStyle (writer:TextWriter) expression =
        VisualExpression.fromExpression visualStyle expression |> formatVisualWriter writer

    [<CompiledName("FormatWriter")>]
    let formatWriter (writer:TextWriter) expression =
        VisualExpression.fromExpression defaultStyle expression |> formatVisualWriter writer
