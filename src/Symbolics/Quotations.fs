namespace MathNet.Symbolics

open System
open System.Numerics
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection
open MathNet.Numerics

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
        | Int16 k -> Number (Integer (BigInteger(int k)))
        | Int32 k -> Number (Integer (BigInteger(k)))
        | Int64 k -> Number (Integer (BigInteger(k)))
        | UInt16 k -> Number (Integer (BigInteger(int k)))
        | UInt32 k -> Number (Integer (BigInteger(k)))
        | UInt64 k -> Number (Integer (BigInteger(k)))
        | Var x -> Identifier (Symbol x.Name)
        | PropertyGet (_, info, _) -> Identifier (Symbol info.Name)
        | Let (_, _, t) -> parse t
        | Lambda (x, t) -> parse t
        | _ -> failwith "not supported"
