﻿[<AutoOpen>]
module Global

open Expecto

open System.Collections.Generic
open MathNet.Symbolics

open Operators

// Test: x should evaluate to expected
let inline (-->) x expected = Expect.equal x expected ""

// Test: x should evaluate to the expected string when formatted *nicely*
let inline (==>) x expected = Expect.equal (Infix.format x) expected ""

// Test: x should evaluate to the expected string when formatted *strictly* (not denormalized)
let inline (===>) x expected = Expect.equal (Infix.formatStrict x) expected ""

// extra test helpers for tuples, list, arrays and hash-sets - maybe there's a better way?
let inline (==|>) (x1, x2) expected = Expect.equal (Infix.format x1, Infix.format x2) expected ""
let inline (==||>) (x1, x2, x3) expected = Expect.equal (Infix.format x1, Infix.format x2, Infix.format x3) expected ""
let inline (==+>) x expected = Expect.equal (List.map Infix.format x) expected ""
let inline (==->) x expected = Expect.equal (Array.map Infix.format x) expected ""
let inline (==*>) (x:HashSet<Expression>) (expected:string list) = Expect.isTrue (HashSet(expected).SetEquals(x |> Seq.map Infix.format)) ""

// extra test helper for MathML (just normalizing XML, really)
let inline (==/>) (x:string) expected = Expect.equal x (Xml.normalizeString expected) ""

// variables
let x = symbol "x"
let y = symbol "y"
let z = symbol "z"
let a = symbol "a"
let b = symbol "b"
let c = symbol "c"
let d = symbol "d"
let e = symbol "e"
let f = symbol "f"

let n = symbol "n"
