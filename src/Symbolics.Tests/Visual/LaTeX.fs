namespace MathNet.Symbolics.Tests.Visual

open NUnit.Framework
open FsUnit
open FsUnitTyped
open MathNet.Symbolics
open Operators

module LaTeX =

    [<Test>]
    let ``Print LaTeX expressions`` () =

        LaTeX.format (1/(a*b)) --> """\frac{1}{ab}"""
        LaTeX.format Expression.MinusOne --> """-1"""
        LaTeX.format Expression.ComplexInfinity --> """\infty"""
        LaTeX.format Expression.Pi --> """\pi"""
        LaTeX.format (Expression.Real -0.23) --> string -0.23
        LaTeX.format (a**b) --> """{a}^{b}"""
        LaTeX.format (a**(b+c)) --> """{a}^{b + c}"""
        LaTeX.format ((a+b)**c) --> """{\left(a + b\right)}^{c}"""
        LaTeX.format (a**(b**c)) --> """{a}^{{b}^{c}}"""
        LaTeX.format ((a**b)**c) --> """{\left({a}^{b}\right)}^{c}"""
        LaTeX.format (a*b*(symbol "def")) --> """ab{def}"""

        LaTeX.format (3Q*2Q**x) --> """3\cdot{2}^{x}"""
        LaTeX.format (3.0*(real 2.0)**x) --> """3\cdot{2}^{x}"""
        LaTeX.format (5Q*x) --> """5x"""
        LaTeX.format (Expression.Pi * 10Q) --> """10\pi"""
        LaTeX.format (Expression.E * 2Q**(4Q*x)) --> """e\cdot{2}^{4x}"""
        LaTeX.format (4Q * Expression.E ** x) --> """4{e}^{x}"""

        LaTeX.format (log10 x) --> """\log_{10}{x}"""
        LaTeX.format (log10 (x+y)) --> """\log_{10}\left(x + y\right)"""
        LaTeX.format (log 8Q y) --> """\log_{8}{y}"""
        LaTeX.format (log 8Q (x+y)) --> """\log_{8}\left(x + y\right)"""
        LaTeX.format (log (sin x) (tanh y)) --> """\log_{\sin{x}}{\tanh{y}}"""
        LaTeX.format (arctan x) --> """\arctan{x}"""
        LaTeX.format (arctan2 x (3Q*y)) --> """\operatorname{atan2}\left({x}, {3y}\right)"""

        LaTeX.format (sin (x+y)) --> """\sin\left(x + y\right)"""
        LaTeX.format (sin ((x+y) ** 2)) --> """\sin{{\left(x + y\right)}^{2}}"""
        LaTeX.format ((sin (x+y)) ** 2) --> """\sin^{2}\left(x + y\right)"""
        LaTeX.format ((sin x)*(cos x)+(tan x)) --> """\sin{x}\cos{x} + \tan{x}"""
        LaTeX.format ((sin (x+y))*(cos (x+y))+(tan (x+y))) --> """\sin\left(x + y\right)\cos\left(x + y\right) + \tan\left(x + y\right)"""

        LaTeX.format (x**(1Q/2)) --> "\sqrt{x}"
        LaTeX.format (x**(1Q/3)) --> "\sqrt[3]{x}"

    [<Test>]
    let ``Function Powers`` () =
        VisualExpression.Function ("sin", BigInteger.One, VisualExpression.Symbol "x") |> LaTeX.formatVisual |> shouldEqual """\sin{x}"""
        VisualExpression.Function ("sin", bigint 2, VisualExpression.Symbol "x") |> LaTeX.formatVisual |> shouldEqual """\sin^{2}{x}"""
        LaTeX.format ((sin x)*(sin x)) --> """\sin^{2}{x}"""
        LaTeX.formatStyle { CompactPowersOfFunctions=false } ((sin x)*(sin x)) --> """{\left(\sin{x}\right)}^{2}"""
