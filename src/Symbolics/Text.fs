namespace MathNet.Symbolics

open System
open System.IO
open System.Text
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

module Text =

    // priority: 1=additive 2=product 3=power
    let rec private formatImpl write priority (q:Expression) =
        match q with
        | Number (Integer x) ->
            if priority > 0 && x.Sign < 0 then write "("
            write (x.ToString())
            if priority > 0 && x.Sign < 0 then write ")"
        | Number (Rational x) ->
            if priority > 1 then write "("
            write (x.ToString());
            if priority > 1 then write ")"
        | Identifier (Symbol name) -> write name
        | Identifier Undefined -> write "undefined"
        | Identifier Infinity -> write "infinity"
        | Sum (x::xs) ->
            if priority > 1 then write "("
            formatImpl write 1 x
            xs |> List.iter (fun x -> write "+"; formatImpl write 1 x)
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
        | Sum [] | Product [] -> failwith "invalid expression"


    let format q =
        let sb = StringBuilder()
        formatImpl (sb.Append >> ignore) 0 q
        sb.ToString()

    let formatTextWriter (writer:TextWriter) q = formatImpl (writer.Write) 0 q
