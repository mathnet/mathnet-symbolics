namespace MathNet.Symbolics

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open MathNet.Symbolics

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
        | Int16 k -> Expression.Int32 (int k)
        | Int32 k -> Expression.Int32 k
        | Int64 k -> Expression.Int64 k
        | UInt16 k -> Expression.Int32 (int k)
        | UInt32 k -> Expression.Int64 (int64 k)
        | UInt64 k -> Expression.Integer (BigInteger k)
        | Double d -> Expression.Real d
        | Single d -> Expression.Real (float d)
        | Var x -> Identifier (Symbol x.Name)
        | PropertyGet (_, info, _) -> Identifier (Symbol info.Name)
        | Let (_, _, t) -> parse t
        | Lambda (_, t) -> parse t
        | _ -> failwith "not supported"
