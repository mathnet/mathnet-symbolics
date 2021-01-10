[<AutoOpen>]
module Global

open FsUnitTyped

open System.Collections.Generic
open MathNet.Symbolics

open Operators

// Test: x should evaluate to expected
let inline (-->) x expected = x |> shouldEqual expected

// Test: x should evaluate to the expected string when formatted
let inline (==>) x expected = (Infix.format x) |> shouldEqual expected

// extra test helpers for tuples, list, arrays and hash-sets - maybe there's a better way?
let inline (==|>) (x1, x2) expected = (Infix.format x1, Infix.format x2) |> shouldEqual expected
let inline (==||>) (x1, x2, x3) expected = (Infix.format x1, Infix.format x2, Infix.format x3) |> shouldEqual expected
let inline (==+>) x expected = (List.map Infix.format x) |> shouldEqual expected
let inline (==->) x expected = (Array.map Infix.format x) |> shouldEqual expected
let inline (==*>) (x:HashSet<Expression>) (expected:string list) = (HashSet(expected).SetEquals(x |> Seq.map Infix.format)) |> shouldEqual true

// extra test helper for MathML (just normalizing XML, really)
let inline (==/>) (x:string) expected = x |> shouldEqual (Xml.normalizeString expected)
