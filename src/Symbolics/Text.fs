namespace MathNet.Symbolics

open System.IO
open System.Text
open MathNet.Symbolics

[<RequireQualifiedAccess>]
module Text =

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
    let formatStrict q =
        let sb = StringBuilder()
        strict (sb.Append >> ignore) 0 q
        sb.ToString()

    let formatStrictTextWriter (writer:TextWriter) q = strict (writer.Write) 0 q


    // Nice Formatting:

    let rec niceNumerator = function
        | NegRationalPower _ -> one
        | Product ax -> product <| List.map niceNumerator ax
        | z -> z
    let rec niceDenominator = function
        | NegRationalPower (r, p) -> r ** -p
        | Product ax -> product <| List.map niceDenominator ax
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
        | Product (Number n::xs) as p when n.IsNegative ->
            write "-";
            nice write 2 (product ((Number -n)::xs))
        | Product xs as p ->
            let n = niceNumerator p
            let d = niceDenominator p
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
    let format q =
        let sb = StringBuilder()
        nice (sb.Append >> ignore) 0 q
        sb.ToString()

    let formatTextWriter (writer:TextWriter) q = nice (writer.Write) 0 q
