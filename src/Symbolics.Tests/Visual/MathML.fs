namespace MathNet.Symbolics.Tests.Visual

open NUnit.Framework
open MathNet.Symbolics
open Operators

module MathML =

    [<Test>]
    let ``Format MathML3 Strict Content`` () =
        MathML.formatContentStrict 1Q ==/> """<cn>1</cn>"""
        MathML.formatContentStrict -1Q ==/> """<cn>-1</cn>"""
        MathML.formatContentStrict (1Q/2) ==/> """<apply><csymbol cd="nums1">rational</csymbol><cn>1</cn><cn>2</cn></apply>"""
        MathML.formatContentStrict x ==/> """<ci>x</ci>"""
        MathML.formatContentStrict -x ==/> """<apply><csymbol cd="arith1">unary_minus</csymbol><ci>x</ci></apply>"""
        MathML.formatContentStrict (-2*x) ==/> """<apply><csymbol cd="arith1">unary_minus</csymbol><apply><csymbol cd="arith1">times</csymbol><cn>2</cn><ci>x</ci></apply></apply>"""
        MathML.formatContentStrict Pi ==/> """<csymbol cd="nums1">pi</csymbol>"""
        MathML.formatContentStrict (1/x) ==/> """<apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><ci>x</ci></apply>"""
        MathML.formatContentStrict (1/(a*b)) ==/> """<apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><apply><csymbol cd="arith1">times</csymbol><ci>a</ci><ci>b</ci></apply></apply>"""
        MathML.formatContentStrict (x**2) ==/> """<apply><csymbol cd="arith1">power</csymbol><ci>x</ci><cn>2</cn></apply>"""
        MathML.formatContentStrict (x**(1Q/2)) ==/> """<apply><csymbol cd="arith1">root</csymbol><ci>x</ci><cn>2</cn></apply>"""
        MathML.formatContentStrict (x**(1Q/3)) ==/> """<apply><csymbol cd="arith1">root</csymbol><ci>x</ci><cn>3</cn></apply>"""

    [<Test>]
    let ``Format MathML3 Strict Content with Annotations`` () =
        MathML.formatSemanticsAnnotated (1/x) ==/> """
            <semantics>
            <apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><ci>x</ci></apply>
            <annotation encoding="application/x-tex">\frac{1}{x}</annotation>
            <annotation encoding="application/x-mathnet-infix">1/x</annotation>
            </semantics>"""

    [<Test>]
    let ``Parse MathML3 Strict Content`` () =
        MathML.parse """<ci>x</ci>""" ==> "x"
        MathML.parse """<cn>1</cn>""" ==> "1"
        MathML.parse """<csymbol cd="nums1">pi</csymbol>""" ==> "π"
        MathML.parse """<apply> <csymbol cd="nums1">rational</csymbol> <cn>1</cn> <cn>2</cn> </apply>""" ==> "1/2"
        MathML.parse """<apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><ci>x</ci></apply>""" ==> "1/x"
        MathML.parse """<apply><csymbol cd="arith1">divide</csymbol><cn>1</cn><apply><csymbol cd="arith1">times</csymbol><ci>a</ci><ci>b</ci></apply></apply>""" ==> "1/(a*b)"
        MathML.parse """<apply><csymbol cd="arith1">power</csymbol><ci>x</ci><cn>2</cn></apply>""" ==> "x^2"
        MathML.parse """<apply><csymbol cd="arith1">root</csymbol><ci>x</ci><cn>2</cn></apply>""" ==> "sqrt(x)"

    [<Test>]
    let ``Parse MathML Non-Strict Content`` () =
        MathML.parse """<apply><divide/><cn>1</cn><ci>x</ci></apply>""" ==> "1/x"
        MathML.parse """<apply><divide/><cn>1</cn><apply><times/><ci>a</ci><ci>b</ci></apply></apply>""" ==> "1/(a*b)"
        MathML.parse """<apply><power/><ci>x</ci><cn>2</cn></apply>""" ==> "x^2"
        MathML.parse """<apply><root/><degree><cn>2</cn></degree><ci>x</ci></apply>""" ==> "sqrt(x)"
        MathML.parse """<apply><root/><degree><cn>3</cn></degree><ci>x</ci></apply>""" ==> "x^(1/3)"
