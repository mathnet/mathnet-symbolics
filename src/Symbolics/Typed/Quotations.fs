namespace MathNet.Symbolics

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open MathNet.Symbolics

open Operators

[<RequireQualifiedAccess>]
module Quotations =

    [<CompiledName("Parse")>]
    let rec parse (q:Expr) : Expression =
        match q with
        | SpecificCall <@@ ( + ) @@> (_, _, [xt; yt]) -> (parse xt) + (parse yt)
        | SpecificCall <@@ ( - ) @@> (_, _, [xt; yt]) -> (parse xt) - (parse yt)
        | SpecificCall <@@ ( ~- ) @@> (_, _, [xt]) -> -(parse xt)
        | SpecificCall <@@ ( ~+ ) @@> (_, _, [xt]) -> +(parse xt)
        | SpecificCall <@@ ( * ) @@> (_, _, [xt; yt]) -> (parse xt) * (parse yt)
        | SpecificCall <@@ ( / ) @@> (_, _, [xt; yt]) -> (parse xt) / (parse yt)
        | SpecificCall <@@ ( ** ) @@> (_, _, [xt; yt]) -> (parse xt) ** (parse yt)
        | Int16 k -> fromInt32 (int k)
        | Int32 k -> fromInt32 k
        | Int64 k -> fromInt64 k
        | UInt16 k -> fromInt32 (int k)
        | UInt32 k -> fromInt64 (int64 k)
        | UInt64 k -> fromInteger (BigInteger k)
        | Double d -> fromDouble d
        | Single f -> fromSingle f
        | Var x -> symbol x.Name
        | PropertyGet (_, info, _) -> symbol info.Name
        | Let (_, _, t) -> parse t
        | Lambda (_, t) -> parse t
        | _ -> failwith "not supported"
