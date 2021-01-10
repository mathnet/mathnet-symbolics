namespace MathNet.Symbolics.Tests.Operators.Trigonometry

open NUnit.Framework
open MathNet.Symbolics
open Operators

module Sin =

    [<Test>]
    let ``Special Values`` () =
        sin -2Q ==> "-sin(2)"
        sin -Pi ==> "-sin(π)" // "0"
        sin 0Q ==> "0"
        sin(Pi/6Q) ==> "sin(π/6)" // "1/2"
        sin(5Q/7Q*Pi) ==> "sin(5/7*π)"
        sin I ==> "j*sinh(1)"

        sin(arcsin(x)) ==> "x"
        sin(arccos(x)) ==> "sqrt(1 - x^2)"
        sin(arctan(x)) ==> "x/sqrt(1 + x^2)"
        sin(arccsc(x)) ==> "1/x"
        sin(arcsec(x)) ==> "sqrt(1 - 1/x^2)"
        sin(arccot(x)) ==> "1/(sqrt(1 + 1/x^2)*x)"

        sin(arcsin(4Q*Pi + x)) ==> "4*π + x"
        sin(arcsin(4Q*I)) ==> "4*j"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*sin(x)) ==> "sin(x) + x*cos(x)"


module Cos =

    [<Test>]
    let ``Special Values`` () =
        cos -2Q ==> "cos(2)"
        cos -Pi ==> "cos(π)" // "-1"
        cos 0Q ==> "1"
        cos(Pi/6Q) ==> "cos(π/6)" // "2/sqrt(3)"
        cos(5Q/7Q*Pi) ==> "cos(5/7*π)"
        cos I ==> "cosh(1)"

        cos(arcsin(x)) ==> "sqrt(1 - x^2)"
        cos(arccos(x)) ==> "x"
        cos(arctan(x)) ==> "(1 + x^2)^(-1/2)"
        cos(arccsc(x)) ==> "sqrt(1 - 1/x^2)"
        cos(arcsec(x)) ==> "1/x"
        cos(arccot(x)) ==> "(1 + 1/x^2)^(-1/2)"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*cos(x)) ==> "-x*sin(x) + cos(x)"


module Tan =

    [<Test>]
    let ``Special Values`` () =
        tan(-2Q) ==> "-tan(2)"
        tan(-Pi) ==> "-tan(π)" // "0"
        tan(0Q) ==> "0"
        tan(Pi/6Q) ==> "tan(π/6)" // "1/sqrt(3)"
        tan(5Q/7Q*Pi) ==> "tan(5/7*π)"
        tan I ==> "j*tanh(1)"

        tan(arcsin(x)) ==> "x/sqrt(1 - x^2)"
        tan(arccos(x)) ==> "sqrt(1 - x^2)/x"
        tan(arctan(x)) ==> "x"
        tan(arccsc(x)) ==> "1/(sqrt(1 - 1/x^2)*x)"
        tan(arcsec(x)) ==> "sqrt(1 - 1/x^2)*x"
        tan(arccot(x)) ==> "1/x"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*tan(x)) ==> "(2*x)/(1 + cos(2*x)) + tan(x)" // "tan(x) + x*sec(x)^2"


module Csc =

    [<Test>]
    let ``Special Values`` () =
        csc(-2Q) ==> "-csc(2)"
        csc(-Pi) ==> "-csc(π)" // "⧝"
        csc(0Q) ==> "⧝"
        csc(Pi/6Q) ==> "csc(π/6)" // "2"
        csc(5Q/7Q*Pi) ==> "csc(5/7*π)"
        csc(I) ==> "-j*csch(1)"

        csc(arcsin(x)) ==> "1/x"
        csc(arccos(x)) ==> "(1 - x^2)^(-1/2)"
        csc(arctan(x)) ==> "sqrt(1 + x^2)/x"
        csc(arccsc(x)) ==> "x"
        csc(arcsec(x)) ==> "(1 - 1/x^2)^(-1/2)"
        csc(arccot(x)) ==> "sqrt(1 + 1/x^2)*x"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*csc(x)) ==> "csc(x) - x*csc(x)*cot(x)"


module Sec =

    [<Test>]
    let ``Special Values`` () =
        sec(-2Q) ==> "sec(2)"
        sec(-Pi) ==> "sec(π)" // "-1"
        sec(0Q) ==> "1"
        sec(Pi/6Q) ==> "sec(π/6)" // "2/sqrt(3)"
        sec(5Q/7Q*Pi) ==> "sec(5/7*π)"
        sec(I) ==> "sech(1)"

        sec(arcsin(x)) ==> "(1 - x^2)^(-1/2)"
        sec(arccos(x)) ==> "1/x"
        sec(arctan(x)) ==> "sqrt(1 + x^2)"
        sec(arccsc(x)) ==> "(1 - 1/x^2)^(-1/2)"
        sec(arcsec(x)) ==> "x"
        sec(arccot(x)) ==> "sqrt(1 + 1/x^2)"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*sec(x)) ==> "sec(x) + x*tan(x)*sec(x)"


module Cot =

    [<Test>]
    let ``Special Values`` () =
        cot(-2Q) ==> "-cot(2)"
        cot(-Pi) ==> "-cot(π)" // "⧝"
        cot(0Q) ==> "⧝"
        cot(Pi/6Q) ==> "cot(π/6)" // "1/sqrt(3)"
        cot(5Q/7Q*Pi) ==> "cot(5/7*π)"
        cot(I) ==> "-j*coth(1)"

        cot(arcsin(x)) ==> "sqrt(1 - x^2)/x"
        cot(arccos(x)) ==> "x/sqrt(1 - x^2)"
        cot(arctan(x)) ==> "1/x"
        cot(arccsc(x)) ==> "sqrt(1 - 1/x^2)*x"
        cot(arcsec(x)) ==> "1/(sqrt(1 - 1/x^2)*x)"
        cot(arccot(x)) ==> "x"

        cot(arcsec(-2Q)) ==> "-1/(2*sqrt(3/4))" // "-3^(-1/2)" or "-1/sqrt(3)"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*cot(x)) ==> "-x/(sin(x))^2 + cot(x)" // "cot(x) - x*(csc(x))^2"


module Sinh =

    [<Test>]
    let ``Special Values`` () =
        sinh(0Q) ==> "0"
        sinh(-2Q) ==> "-sinh(2)"
        sinh(I) ==> "j*sin(1)"
        sinh(-Pi*I) ==> "-sinh(π*j)" // "0"
        sinh(-1Q/6Q*Pi*I) ==> "-sinh(1/6*π*j)" // "1/2*j"
        sinh(5Q/7Q*Pi*I) ==> "sinh(5/7*π*j)"
        sinh(3Q/2Q*Pi*I) ==> "sinh(3/2*π*j)" // "-j"

        sinh(arcsinh(x)) ==> "x"
        sinh(arccosh(x)) ==> "sqrt((-1 + x)/(1 + x))*(1 + x)"
        sinh(arctanh(x)) ==> "x/sqrt(1 - x^2)"
        sinh(arccsch(x)) ==> "1/x"
        sinh(arcsech(x)) ==> "((1 + x)*sqrt((1 - x)/(1 + x)))/x"
        sinh(arccoth(x)) ==> "1/(sqrt(1 - 1/x^2)*x)"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*sinh(x)) ==> "sinh(x) + x*cosh(x)"


module Cosh =

    [<Test>]
    let ``Special Values`` () =
        cosh(0Q) ==> "1"
        cosh(-2Q) ==> "cosh(2)"
        cosh(I) ==> "cos(1)"
        cosh(-Pi*I) ==> "cosh(π*j)" // "-1"
        cosh(-1Q/6Q*Pi*I) ==> "cosh(1/6*π*j)" // "sqrt(3)/2"
        cosh(5Q/7Q*Pi*I) ==> "cosh(5/7*π*j)"
        cosh(3Q/2Q*Pi*I) ==> "cosh(3/2*π*j)" // "0"

        cosh(arcsinh(x)) ==> "sqrt(1 + x^2)"
        cosh(arccosh(x)) ==> "x"
        cosh(arctanh(x)) ==> "(1 - x^2)^(-1/2)"
        cosh(arccsch(x)) ==> "sqrt(1 + 1/x^2)"
        cosh(arcsech(x)) ==> "1/x"
        cosh(arccoth(x)) ==> "(1 - 1/x^2)^(-1/2)"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*cosh(x)) ==> "x*sinh(x) + cosh(x)"


module Tanh =

    [<Test>]
    let ``Special Values`` () =
        tanh(0Q) ==> "0"
        tanh(-2Q) ==> "-tanh(2)"
        tanh(I) ==> "j*tan(1)"
        tanh(-Pi*I) ==> "-tanh(π*j)" // "0"
        tanh(-1Q/6Q*Pi*I) ==> "-tanh(1/6*π*j)" // "-sqrt(3)/2"
        tanh(5Q/7Q*Pi*I) ==> "tanh(5/7*π*j)" // "-tan(2/7*π)*j"
        tanh(3Q/2Q*Pi*I) ==> "tanh(3/2*π*j)" // "⧝"

        tanh(arcsinh(x)) ==> "x/sqrt(1 + x^2)"
        tanh(arccosh(x)) ==> "(sqrt((-1 + x)/(1 + x))*(1 + x))/x"
        tanh(arctanh(x)) ==> "x"
        tanh(arccsch(x)) ==> "1/(sqrt(1 + 1/x^2)*x)"
        tanh(arcsech(x)) ==> "(1 + x)*sqrt((1 - x)/(1 + x))"
        tanh(arccoth(x)) ==> "1/x"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*tanh(x)) ==> "(2*x)/(1 + cosh(2*x)) + tanh(x)" // "tanh(x) + x*(sech(x))^2"


module Csch =

    [<Test>]
    let ``Special Values`` () =
        csch(0Q) ==> "⧝"
        csch(-2Q) ==> "-csch(2)"
        csch(I) ==> "-j*csc(1)"
        csch(-Pi*I) ==> "-csch(π*j)" // "⧝"
        csch(-1Q/6Q*Pi*I) ==> "-csch(1/6*π*j)" // "2*j"
        csch(5Q/7Q*Pi*I) ==> "csch(5/7*π*j)" // "-csc(2/7*π)*j"
        csch(3Q/2Q*Pi*I) ==> "csch(3/2*π*j)" // "j"

        csch(arcsinh(x)) ==> "1/x"
        csch(arccosh(x)) ==> "1/(sqrt((-1 + x)/(1 + x))*(1 + x))"
        csch(arctanh(x)) ==> "sqrt(1 - x^2)/x"
        csch(arccsch(x)) ==> "x"
        csch(arcsech(x)) ==> "x/((1 + x)*sqrt((1 - x)/(1 + x)))"
        csch(arccoth(x)) ==> "sqrt(1 - 1/x^2)*x"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*csch(x)) ==> "csch(x) - x*csch(x)*coth(x)"


module Sech =

    [<Test>]
    let ``Special Values`` () =
        sech(0Q) ==> "1"
        sech(-2Q) ==> "sech(2)"
        sech(I) ==> "sec(1)"
        sech(-Pi*I) ==> "sech(π*j)" // "-1"
        sech(-1Q/6Q*Pi*I) ==> "sech(1/6*π*j)" // "2/sqrt(3)"
        sech(5Q/7Q*Pi*I) ==> "sech(5/7*π*j)" // "-1/sec(2/7*π)"
        sech(3Q/2Q*Pi*I) ==> "sech(3/2*π*j)" // "⧝"

        sech(arcsinh(x)) ==> "(1 + x^2)^(-1/2)"
        sech(arccosh(x)) ==> "1/x"
        sech(arctanh(x)) ==> "sqrt(1 - x^2)"
        sech(arccsch(x)) ==> "(1 + 1/x^2)^(-1/2)"
        sech(arcsech(x)) ==> "x"
        sech(arccoth(x)) ==> "sqrt(1 - 1/x^2)"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*sech(x)) ==> "sech(x) - x*tanh(x)*sech(x)"


module Coth =

    [<Test>]
    let ``Special Values`` () =
        coth(0Q) ==> "⧝"
        coth(-2Q) ==> "-coth(2)"
        coth(I) ==> "-j*cot(1)"
        coth(-Pi*I) ==> "-coth(π*j)" // "0"
        coth(-1Q/6Q*Pi*I) ==> "-coth(1/6*π*j)" // "-sqrt(3)*j"
        coth(5Q/7Q*Pi*I) ==> "coth(5/7*π*j)" // "cot(2/7*π)*j"
        coth(3Q/2Q*Pi*I) ==> "coth(3/2*π*j)" // "0"

        coth(arcsinh(x)) ==> "sqrt(1 + x^2)/x"
        coth(arccosh(x)) ==> "x/(sqrt((-1 + x)/(1 + x))*(1 + x))"
        coth(arctanh(x)) ==> "1/x"
        coth(arccsch(x)) ==> "sqrt(1 + 1/x^2)*x"
        coth(arcsech(x)) ==> "1/((1 + x)*sqrt((1 - x)/(1 + x)))"
        coth(arccoth(x)) ==> "x"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*coth(x)) ==> "-(2*x)/(-1 + cosh(2*x)) + coth(x)" // "coth(x) - x*(csch(x))^2"


module Asin =

    [<Test>]
    let ``Special Values`` () =
        arcsin(-1Q) ==> "-π/2"
        arcsin(-1Q/3Q) ==> "-asin(1/3)" // "-asin(1/3)"
        arcsin(0Q) ==> "0"
        arcsin(1Q/2Q) ==> "asin(1/2)" // "π/6"
        arcsin(1Q) ==> "π/2"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arcsin(x)) ==> "x/sqrt(1 - x^2) + asin(x)"


module Acos =

    [<Test>]
    let ``Special Values`` () =
        arccos(-1Q) ==> "π"
        arccos(-1Q/3Q) ==> "acos(-1/3)"
        arccos(0Q) ==> "π/2"
        arccos(1Q/2Q) ==> "acos(1/2)" // "π/3"
        arccos(1Q) ==> "0"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arccos(x)) ==> "-x/sqrt(1 - x^2) + acos(x)"


module Atan =

    [<Test>]
    let ``Special Values`` () =
        arctan(-1Q) ==> "-π/4"
        arctan(-1Q/3Q) ==> "-atan(1/3)"
        arctan(0Q) ==> "0"
        arctan(1Q/2Q) ==> "atan(1/2)"
        arctan(1Q) ==> "π/4"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arctan(x)) ==> "x/(1 + x^2) + atan(x)"


module Acsc =

    [<Test>]
    let ``Special Values`` () =
        arccsc(-1Q) ==> "-π/2"
        arccsc(-1Q/3Q) ==> "acsc(-1/3)" // "-acsc(1/3)"
        arccsc(0Q) ==> "⧝"
        arccsc(1Q/2Q) ==> "acsc(1/2)" // "csc(1/2)"
        arccsc(1Q) ==> "π/2"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arccsc(x)) ==> "-1/(sqrt(1 - 1/x^2)*x) + acsc(x)"



module Asec =

    [<Test>]
    let ``Special Values`` () =
        arcsec(-1Q) ==> "π"
        arcsec(-1Q/3Q) ==> "asec(-1/3)"
        arcsec(0Q) ==> "⧝"
        arcsec(1Q/2Q) ==> "asec(1/2)"
        arcsec(1Q) ==> "0"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arcsec(x)) ==> "1/(sqrt(1 - 1/x^2)*x) + asec(x)"


module Acot =

    [<Test>]
    let ``Special Values`` () =
        arccot(-1Q) ==> "-π/4"
        arccot(-1Q/3Q) ==> "-acot(1/3)"
        arccot(0Q) ==> "π/2"
        arccot(1Q/2Q) ==> "acot(1/2)"
        arccot(1Q) ==> "π/4"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arccot(x)) ==> "-x/(1 + x^2) + acot(x)"


module Asinh =

    [<Test>]
    let ``Special Values`` () =
        arcsinh(0Q) ==> "0"
        arcsinh(-I) ==> "-asinh(j)" // "-1/2*π*j"
        arcsinh(-1Q/2Q*I) ==> "-asinh(j/2)" // "1/2*j"
        arcsinh(I) ==> "1/2*π*j"
        arcsinh(2Q*I) ==> "asinh(2*j)" // "asin(2)*j"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arcsinh(x)) ==> "x/sqrt(1 + x^2) + asinh(x)"


module Acosh =

    [<Test>]
    let ``Special Values`` () =
        arccosh(0Q) ==> "1/2*π*j"
        arccosh(-I) ==> "acosh(-j)"
        arccosh(-1Q/2Q*I) ==> "acosh(-j/2)"
        arccosh(I) ==> "acosh(j)"
        arccosh(2Q*I) ==> "acosh(2*j)"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arccosh(x)) ==> "x/(sqrt(-1 + x)*sqrt(1 + x)) + acosh(x)"


module Atanh =

    [<Test>]
    let ``Special Values`` () =
        arctanh(0Q) ==> "0"
        arctanh(-I) ==> "-atanh(j)" // "-1/4*π*j"
        arctanh(-1Q/2Q*I) ==> "-atanh(j/2)" // "-atan(1/2)*j"
        arctanh(I) ==> "1/4*π*j"
        arctanh(2Q*I) ==> "atanh(2*j)" // "atan(2)*j"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arctanh(x)) ==> "x/(1 - x^2) + atanh(x)"


module Acsch =

    [<Test>]
    let ``Special Values`` () =
        arccsch(0Q) ==> "⧝"
        arccsch(-I) ==> "-acsch(j)" // "1/2*π*j"
        arccsch(-1Q/2Q*I) ==> "-acsch(j/2)" // "acsc(1/2)*j"
        arccsch(I) ==> "-1/2*π*j"
        arccsch(2Q*I) ==> "acsch(2*j)" // "-1/6*π*j"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (arccsch(x)) ==> "-1/(sqrt(1 + 1/x^2)*x^2)" // "-1/(sqrt(1 + 1/x^2)*x)"
        Calculus.differentiate x (x*arccsch(x)) ==> "-1/(sqrt(1 + 1/x^2)*x) + acsch(x)" // "-1/(sqrt(1 + 1/x^2)*x) + acsch(x)"


module Asech =

    [<Test>]
    let ``Special Values`` () =
        arcsech(0Q) ==> "∞"
        arcsech(-I) ==> "asech(-j)"
        arcsech(-1Q/2Q*I) ==> "asech(-j/2)"
        arcsech(I) ==> "asech(j)"
        arcsech(2Q*I) ==> "asech(2*j)"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (arcsech(x)) ==> "sqrt((1 - x)/(1 + x))/(x*(-1 + x))"
        Calculus.differentiate x (x*arcsech(x)) ==> "sqrt((1 - x)/(1 + x))/(-1 + x) + asech(x)"


module Acoth =

    [<Test>]
    let ``Special Values`` () =
        arccoth(0Q) ==> "1/2*π*j"
        arccoth(-I) ==> "-acoth(j)" // "1/4*π*j"
        arccoth(-1Q/2Q*I) ==> "-acoth(j/2)" // "acot(1/2)*j"
        arccoth(I) ==> "-1/4*π*j"
        arccoth(2Q*I) ==> "acoth(2*j)" // "-atan(2)*j"

    [<Test>]
    let ``Calculus`` () =
        Calculus.differentiate x (x*arccoth(x)) ==> "x/(1 - x^2) + acoth(x)"
