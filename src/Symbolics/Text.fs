namespace MathNet.Symbolics

open System
open System.IO
open System.Text
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics


[<RequireQualifiedAccess>]
module Text =

    // priority: 1=additive 2=product 3=power
    let rec private formatImpl write priority (q:Expression) =

        let functionName = function
            | Abs -> "abs"
            | Ln -> "ln" | Exp -> "exp"
            | Sin -> "sin" | Cos -> "cos" | Tan -> "tan"

        match q with
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
            formatImpl write 1 x
            xs |> List.iter (fun x -> write " + "; formatImpl write 1 x)
            if priority > 1 then write ")"
        | Product (x::xs) ->
            if priority > 2 then write "("
            formatImpl write 2 x
            xs |> List.iter (fun x -> write "*"; formatImpl write 2 x)
            if priority > 2 then write ")"
        | Power (r, p) ->
            if priority > 2 then write "("
            formatImpl write 3 r
            write "^"
            formatImpl write 3 p
            if priority > 2 then write ")"
        | Function (Abs, x) ->
            write "|"
            formatImpl write 0 x
            write "|"
        | Function (fn, x) ->
            write (functionName fn)
            write "("
            formatImpl write 0 x
            write ")"
        | FunctionN (fn, x::xs) ->
            write (functionName fn)
            write "("
            formatImpl write 0 x
            xs |> List.iter (fun x -> write ","; formatImpl write 0 x)
            write ")"
        | Sum [] | Product [] | FunctionN (_, []) -> failwith "invalid expression"


    let format q =
        let sb = StringBuilder()
        formatImpl (sb.Append >> ignore) 0 q
        sb.ToString()

    let formatTextWriter (writer:TextWriter) q = formatImpl (writer.Write) 0 q
