namespace MathNet.Symbolics

open System.IO
open System.Text
open MathNet.Symbolics

[<RequireQualifiedAccess>]
module LaTeX =

    open Operators
    open ExpressionPatterns

    // priority: 1=additive 2=product 3=power

    let functionName = function
        | Abs -> "\\mathrm{abs}"
        | Ln -> "\\ln" | Exp -> "\\exp"
        | Sin -> "\\sin" | Cos -> "\\cos" | Tan -> "\\tan"

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
        | Constant (Constant.Real fp) ->
            if fp >= 0.0 then write (fp.ToString())
            else
                if priority > 0 then write "\\left({"
                write (fp.ToString());
                if priority > 0 then write "}\\right)"
        | Constant Pi -> write "\\pi"
        | Constant E -> write "e"
        | Constant I -> write "\\jmath"
        | Infinity -> write "\\infty"
        | ComplexInfinity -> write "\\infty"
        | Identifier (Symbol name) -> write name
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
            let n = InfixPrinter.numerator p
            let d = InfixPrinter.denominator p
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
        | Function (Exp, x) ->
            if priority > 3 then write "\\left("
            write "\\mathrm{e}^"
            tex write 4 x
            if priority > 3 then write "\\right)"
        | Function (fn, x) ->
            if priority > 3 then write "\\left("
            write (functionName fn)
            write "{"
            tex write 3 x
            write "}"
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

    /// LaTeX output
    [<CompiledName("Print")>]
    let print q =
        let sb = StringBuilder()
        tex (sb.Append >> ignore) 0 q
        sb.ToString()

    [<CompiledName("PrintToTextWriter")>]
    let printTextWriter (writer:TextWriter) q = tex (writer.Write) 0 q
