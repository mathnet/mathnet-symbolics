namespace MathNet.Symbolics

open System
open System.Numerics
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection
open MathNet.Numerics
open MathNet.Symbolics

module Quotations =

    let rec parse (q:Expr) : Expression =
        match q with
        | SpecificCall <@@ ( + ) @@> (_, _, [xt; yt]) -> (parse xt) + (parse yt)
        | SpecificCall <@@ ( - ) @@> (_, _, [xt; yt]) -> (parse xt) - (parse yt)
        | SpecificCall <@@ ( ~- ) @@> (_, _, [xt]) -> -(parse xt)
        | SpecificCall <@@ ( ~+ ) @@> (_, _, [xt]) -> +(parse xt)
        | SpecificCall <@@ ( * ) @@> (_, _, [xt; yt]) -> (parse xt) * (parse yt)
        | SpecificCall <@@ ( / ) @@> (_, _, [xt; yt]) -> (parse xt) / (parse yt)
        | SpecificCall <@@ ( ** ) @@> (_, _, [xt; yt]) -> (parse xt) ** (parse yt)
        | Int16 k -> Expression.OfInt32 (int k)
        | Int32 k -> Expression.OfInt32 k
        | Int64 k -> Expression.OfInt64 k
        | UInt16 k -> Expression.OfInt32 (int k)
        | UInt32 k -> Expression.OfInt64 (int64 k)
        | UInt64 k -> Expression.OfInteger (BigInteger k)
        | Var x -> Identifier (Symbol x.Name)
        | PropertyGet (_, info, _) -> Identifier (Symbol info.Name)
        | Let (_, _, t) -> parse t
        | Lambda (x, t) -> parse t
        | _ -> failwith "not supported"
