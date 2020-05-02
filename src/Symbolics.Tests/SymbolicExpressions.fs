namespace MathNet.Symbolics.Tests

open NUnit.Framework
open FsUnit
open FsUnitTyped

open MathNet.Numerics
open MathNet.Symbolics
open Operators

module SymbolicExpressions =

    type Expr = SymbolicExpression

    [<Test>]
    let ``Parsing and Formatting``  () =

        Expr.Parse("sin(x)").ToString() |> shouldEqual """sin(x)"""
        Expr.Parse("(sin(x))^2").ToString() |> shouldEqual """(sin(x))^2"""
        Expr.Parse("(sin(x))^2").ToCustomString() |> shouldEqual """(sin(x))^2"""
        Expr.Parse("(sin(x))^2").ToCustomString(false) |> shouldEqual """(sin(x))^2"""
        Expr.Parse("(sin(x))^2").ToCustomString(true) |> shouldEqual """sin^2(x)"""

        Expr.Parse("sin(x)").ToLaTeX() |> shouldEqual """\sin{x}"""
        Expr.Parse("(sin(x))^2").ToLaTeX() |> shouldEqual """{\left(\sin{x}\right)}^{2}"""
        Expr.Parse("(sin(x))^2").ToCustomLaTeX() |> shouldEqual """{\left(\sin{x}\right)}^{2}"""
        Expr.Parse("(sin(x))^2").ToCustomLaTeX(false) |> shouldEqual """{\left(\sin{x}\right)}^{2}"""
        Expr.Parse("(sin(x))^2").ToCustomLaTeX(true) |> shouldEqual """\sin^{2}{x}"""

