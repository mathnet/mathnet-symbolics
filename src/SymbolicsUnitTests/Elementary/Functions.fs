module Elementary.Functions

open Expecto
open MathNet.Symbolics
open Operators

let tests =
    testList "Elementary" [
        test "Special values of elementary functions" {

            // Operators.exp
            exp(x + y - (x + y)) ==> "exp(x + y - (x + y))" // "1"

            exp(-1Q) ==> "exp(-1)" // "1/e"
            exp(1Q/2Q*pi*Constant I) ==> "exp(1/2*π*j)" // "j"
            exp(2Q/2Q*pi*Constant I) ==> "exp(π*j)" // "-1"
            exp(3Q/2Q*pi*Constant I) ==> "exp(3/2*π*j)" // "-j"
            exp(4Q/2Q*pi*Constant I) ==> "exp(2*π*j)" // "1"
            exp(-3Q/2Q*pi*Constant I) ==> "exp(-3/2*π*j)" // "j"

            exp(ln(x)) ==> "exp(ln(x))" // "x"
            exp(ln(1Q/Constant E)) ==> "exp(ln(1/e))" // "1/e"
            exp(ln(1Q/x)) ==> "exp(ln(1/x))" // "1/x"
            exp(ln(2Q/3Q)) ==> "exp(ln(2/3))" // "2/3"
            exp(ln(Constant I)) ==> "exp(ln(j))" // "j"

            // Operators.ln
            ln(1Q/x) ==> "ln(1/x)"
            ln(1Q/2Q) ==> "ln(1/2)" // "-ln(2)"
            ln(1Q/Constant E) ==> "ln(1/e)" // "-1"

            ln(Constant I) ==> "ln(j)" // "1/2*π*j"
            ln(2Q*Constant I) ==> "ln(2*j)" // "1/2*π*j + ln(2)"
            ln(-2Q*Constant I) ==> "ln(-2*j)" // "-1/2*π*j + ln(2)"
            ln(-1Q/3Q*Constant I) ==> "ln(-j/3)" // "-1/2*π*j - ln(3)"

            ln(exp(x)) ==> "ln(exp(x))"
            ln(exp(5Q)) ==> "ln(exp(5))" // "5"


            // Operators.log10
            log10(10Q) ==> "1"
            log10(1Q/10Q) ==> "log(1/10)" // "-1"

            // Operators.sin
            sin(-2Q) ==> "-sin(2)"
            sin(-pi) ==> "-sin(π)" // "0"
            sin(0Q) ==> "0"
            sin(pi/6Q) ==> "sin(π/6)" // "1/2"
            sin(5Q/7Q*pi) ==> "sin(5/7*π)"
            sin(Constant I) ==> "sin(j)" // "j*sinh(1)"

            sin(arcsin(x)) ==> "sin(asin(x))" // "x"
            sin(arccos(x)) ==> "sin(acos(x))" // "sqrt(1 - x^2)"
            sin(arctan(x)) ==> "sin(atan(x))" // "x/sqrt(1 + x^2)"
            sin(arccsc(x)) ==> "sin(acsc(x))" // "1/x"
            sin(arcsec(x)) ==> "sin(asec(x))" // "sqrt(1 - 1/x^2)"
            sin(arccot(x)) ==> "sin(acot(x))" // "1/(sqrt(1 + 1/x^2)*x)"

            sin(arcsin(4Q*pi + x)) ==> "sin(asin(4*π + x))" // "4*π + x"
            sin(arcsin(4Q*Constant I)) ==> "sin(asin(4*j))" // "4*j"

            Calculus.differentiate x (x*sin(x)) ==> "sin(x) + x*cos(x)"

            // Operators.cos
            cos(-2Q) ==> "cos(2)"
            cos(-pi) ==> "cos(π)" // "-1"
            cos(0Q) ==> "1"
            cos(pi/6Q) ==> "cos(π/6)" // "2/sqrt(3)"
            cos(5Q/7Q*pi) ==> "cos(5/7*π)"
            cos(Constant I) ==> "cos(j)" // "cosh(1)"

            cos(arcsin(x)) ==> "cos(asin(x))" // "sqrt(1 - x^2)"
            cos(arccos(x)) ==> "cos(acos(x))" // "x"
            cos(arctan(x)) ==> "cos(atan(x))" // "(1 + x^2)^(-1/2)"
            cos(arccsc(x)) ==> "cos(acsc(x))" // "sqrt(1 - 1/x^2)"
            cos(arcsec(x)) ==> "cos(asec(x))" // "1/x"
            cos(arccot(x)) ==> "cos(acot(x))" // "(1 + 1/x^2)^(-1/2)"

            Calculus.differentiate x (x*cos(x)) ==> "-x*sin(x) + cos(x)"

            // Operators.tan
            tan(-2Q) ==> "-tan(2)"
            tan(-pi) ==> "-tan(π)" // "0"
            tan(0Q) ==> "0"
            tan(pi/6Q) ==> "tan(π/6)" // "1/sqrt(3)"
            tan(5Q/7Q*pi) ==> "tan(5/7*π)"
            tan(Constant I) ==> "tan(j)" // "j*tanh(1)"

            tan(arcsin(x)) ==> "tan(asin(x))" // "x/sqrt(1 - x^2)"
            tan(arccos(x)) ==> "tan(acos(x))" // "sqrt(1 - x^2)/x"
            tan(arctan(x)) ==> "tan(atan(x))" // "x"
            tan(arccsc(x)) ==> "tan(acsc(x))" // "1/(sqrt(1 - 1/x^2)*x)"
            tan(arcsec(x)) ==> "tan(asec(x))" // "sqrt(1 - 1/x^2)*x"
            tan(arccot(x)) ==> "tan(acot(x))" // "1/x"

            Calculus.differentiate x (x*tan(x)) ==> "(2*x)/(1 + cos(2*x)) + tan(x)" // "tan(x) + x*sec(x)^2"

            // Operators.csc
            csc(-2Q) ==> "csc(-2)" // "-csc(2)"
            csc(-pi) ==> "csc(-π)" // "-csc(π)" // "⧝"
            csc(0Q) ==> "csc(0)" // "⧝"
            csc(pi/6Q) ==> "csc(π/6)" // "2"
            csc(5Q/7Q*pi) ==> "csc(5/7*π)"
            csc(Constant I) ==> "csc(j)" // "-j*csch(1)"

            csc(arcsin(x)) ==> "csc(asin(x))" // "1/x"
            csc(arccos(x)) ==> "csc(acos(x))" // "(1 - x^2)^(-1/2)"
            csc(arctan(x)) ==> "csc(atan(x))" // "sqrt(1 + x^2)/x"
            csc(arccsc(x)) ==> "csc(acsc(x))" // "x"
            csc(arcsec(x)) ==> "csc(asec(x))" // "(1 - 1/x^2)^(-1/2)"
            csc(arccot(x)) ==> "csc(acot(x))" // "sqrt(1 + 1/x^2)*x"

            Calculus.differentiate x (x*csc(x)) ==> "csc(x) - x*csc(x)*cot(x)"

            // Operators.sec
            sec(-2Q) ==> "sec(-2)" // "sec(2)"
            sec(-pi) ==> "sec(-π)" // "sec(π)" // "-1"
            sec(0Q) ==> "sec(0)" // "1"
            sec(pi/6Q) ==> "sec(π/6)" // "2/sqrt(3)"
            sec(5Q/7Q*pi) ==> "sec(5/7*π)"
            sec(Constant I) ==> "sec(j)" // "sech(1)"

            sec(arcsin(x)) ==> "sec(asin(x))" // "(1 - x^2)^(-1/2)"
            sec(arccos(x)) ==> "sec(acos(x))" // "1/x"
            sec(arctan(x)) ==> "sec(atan(x))" // "sqrt(1 + x^2)"
            sec(arccsc(x)) ==> "sec(acsc(x))" // "(1 - 1/x^2)^(-1/2)"
            sec(arcsec(x)) ==> "sec(asec(x))" // "x"
            sec(arccot(x)) ==> "sec(acot(x))" // "sqrt(1 + 1/x^2)"

            Calculus.differentiate x (x*sec(x)) ==> "sec(x) + x*tan(x)*sec(x)"

            // Operators.cot
            cot(-2Q) ==> "cot(-2)" // "-cot(2)"
            cot(-pi) ==> "cot(-π)" // "-cot(π)" // "⧝"
            cot(0Q) ==> "cot(0)" // "⧝"
            cot(pi/6Q) ==> "cot(π/6)" // "1/sqrt(3)"
            cot(5Q/7Q*pi) ==> "cot(5/7*π)"
            cot(Constant I) ==> "cot(j)" // "-j*coth(1)"

            cot(arcsin(x)) ==> "cot(asin(x))" // "sqrt(1 - x^2)/x"
            cot(arccos(x)) ==> "cot(acos(x))" // "x/sqrt(1 - x^2)"
            cot(arctan(x)) ==> "cot(atan(x))" // "1/x"
            cot(arccsc(x)) ==> "cot(acsc(x))" // "sqrt(1 - 1/x^2)*x"
            cot(arcsec(x)) ==> "cot(asec(x))" // "1/(sqrt(1 - 1/x^2)*x)"
            cot(arccot(x)) ==> "cot(acot(x))" // "x"

            cot(arcsec(-2Q)) ==> "cot(asec(-2))" // "-1/(2*sqrt(3/4))" // "-3^(-1/2)" or "-1/sqrt(3)"

            Calculus.differentiate x (x*cot(x)) ==> "-x/(sin(x))^2 + cot(x)" // "cot(x) - x*(csc(x))^2"

            // Operators.sinh
            sinh(0Q) ==> "sinh(0)" // "0"
            sinh(-2Q) ==> "sinh(-2)" // "-sinh(2)"
            sinh(Constant I) ==> "sinh(j)" // "j*sin(1)"
            sinh(-pi*Constant I) ==> "sinh(-π*j)" // "-sinh(π*j)" // "0"
            sinh(-1Q/6Q*pi*Constant I) ==> "sinh(-1/6*π*j)" // "-sinh(1/6*π*j)" // "1/2*j"
            sinh(5Q/7Q*pi*Constant I) ==> "sinh(5/7*π*j)"
            sinh(3Q/2Q*pi*Constant I) ==> "sinh(3/2*π*j)" // "-j"

            sinh(arcsinh(x)) ==> "sinh(asinh(x))" // "x"
            sinh(arccosh(x)) ==> "sinh(acosh(x))" // "sqrt((-1 + x)/(1 + x))*(1 + x)"
            sinh(arctanh(x)) ==> "sinh(atanh(x))" // "x/sqrt(1 - x^2)"
            sinh(arccsch(x)) ==> "sinh(acsch(x))" // "1/x"
            sinh(arcsech(x)) ==> "sinh(asech(x))" // "((1 + x)*sqrt((1 - x)/(1 + x)))/x"
            sinh(arccoth(x)) ==> "sinh(acoth(x))" // "1/(sqrt(1 - 1/x^2)*x)"

            Calculus.differentiate x (x*sinh(x)) ==> "sinh(x) + x*cosh(x)"

            // Operators.cosh
            cosh(0Q) ==> "cosh(0)" // "1"
            cosh(-2Q) ==> "cosh(-2)" // "cosh(2)"
            cosh(Constant I) ==> "cosh(j)" // "cos(1)"
            cosh(-pi*Constant I) ==> "cosh(-π*j)"  // "cosh(π*j)" // "-1"
            cosh(-1Q/6Q*pi*Constant I) ==> "cosh(-1/6*π*j)" // "cosh(1/6*π*j)" // "sqrt(3)/2"
            cosh(5Q/7Q*pi*Constant I) ==> "cosh(5/7*π*j)"
            cosh(3Q/2Q*pi*Constant I) ==> "cosh(3/2*π*j)" // "0"

            cosh(arcsinh(x)) ==> "cosh(asinh(x))" // "sqrt(1 + x^2)"
            cosh(arccosh(x)) ==> "cosh(acosh(x))" // "x"
            cosh(arctanh(x)) ==> "cosh(atanh(x))" // "(1 - x^2)^(-1/2)"
            cosh(arccsch(x)) ==> "cosh(acsch(x))" // "sqrt(1 + 1/x^2)"
            cosh(arcsech(x)) ==> "cosh(asech(x))" // "1/x"
            cosh(arccoth(x)) ==> "cosh(acoth(x))" // "(1 - 1/x^2)^(-1/2)"

            Calculus.differentiate x (x*cosh(x)) ==> "x*sinh(x) + cosh(x)"

            // Operators.tanh
            tanh(0Q) ==> "tanh(0)" // "0"
            tanh(-2Q) ==> "tanh(-2)" // "-tanh(2)"
            tanh(Constant I) ==> "tanh(j)" // "j*tan(1)"
            tanh(-pi*Constant I) ==> "tanh(-π*j)" // "-tanh(π*j)" // "0"
            tanh(-1Q/6Q*pi*Constant I) ==> "tanh(-1/6*π*j)" // "-tanh(1/6*π*j)" // "-sqrt(3)/2"
            tanh(5Q/7Q*pi*Constant I) ==> "tanh(5/7*π*j)" // "-tan(2/7*π)*j"
            tanh(3Q/2Q*pi*Constant I) ==> "tanh(3/2*π*j)" // "⧝"

            tanh(arcsinh(x)) ==> "tanh(asinh(x))" // "x/sqrt(1 + x^2)"
            tanh(arccosh(x)) ==> "tanh(acosh(x))" // "(sqrt((-1 + x)/(1 + x))*(1 + x))/x"
            tanh(arctanh(x)) ==> "tanh(atanh(x))" // "x"
            tanh(arccsch(x)) ==> "tanh(acsch(x))" // "1/(sqrt(1 + 1/x^2)*x)"
            tanh(arcsech(x)) ==> "tanh(asech(x))" // "(1 + x)*sqrt((1 - x)/(1 + x))"
            tanh(arccoth(x)) ==> "tanh(acoth(x))" // "1/x"

            Calculus.differentiate x (x*tanh(x)) ==> "(2*x)/(1 + cosh(2*x)) + tanh(x)" // "tanh(x) + x*(sech(x))^2"

            // Operators.csch
            csch(0Q) ==> "csch(0)" // "⧝"
            csch(-2Q) ==> "csch(-2)" // "-csch(2)"
            csch(Constant I) ==> "csch(j)" // "-j*csc(1)"
            csch(-pi*Constant I) ==> "csch(-π*j)" // "-csch(π*j)" // "⧝"
            csch(-1Q/6Q*pi*Constant I) ==> "csch(-1/6*π*j)" // "-csch(1/6*π*j)" // "2*j"
            csch(5Q/7Q*pi*Constant I) ==> "csch(5/7*π*j)" // "-csc(2/7*π)*j"
            csch(3Q/2Q*pi*Constant I) ==> "csch(3/2*π*j)" // "j"

            csch(arcsinh(x)) ==> "csch(asinh(x))" // "1/x"
            csch(arccosh(x)) ==> "csch(acosh(x))" // "1/(sqrt((-1 + x)/(1 + x))*(1 + x))"
            csch(arctanh(x)) ==> "csch(atanh(x))" // "sqrt(1 - x^2)/x"
            csch(arccsch(x)) ==> "csch(acsch(x))" // "x"
            csch(arcsech(x)) ==> "csch(asech(x))" // "x/((1 + x)*sqrt((1 - x)/(1 + x)))"
            csch(arccoth(x)) ==> "csch(acoth(x))" // "sqrt(1 - 1/x^2)*x"

            Calculus.differentiate x (x*csch(x)) ==> "csch(x) - x*csch(x)*coth(x)"

            // Operators.sech
            sech(0Q) ==> "sech(0)" // "1"
            sech(-2Q) ==> "sech(-2)" // "sech(2)"
            sech(Constant I) ==> "sech(j)" // "sec(1)"
            sech(-pi*Constant I) ==> "sech(-π*j)" // "sech(π*j)" // "-1"
            sech(-1Q/6Q*pi*Constant I) ==> "sech(-1/6*π*j)" // "sech(1/6*π*j)" // "2/sqrt(3)"
            sech(5Q/7Q*pi*Constant I) ==> "sech(5/7*π*j)" // "-1/sec(2/7*π)"
            sech(3Q/2Q*pi*Constant I) ==> "sech(3/2*π*j)" // "⧝"

            sech(arcsinh(x)) ==> "sech(asinh(x))" // "(1 + x^2)^(-1/2)"
            sech(arccosh(x)) ==> "sech(acosh(x))" // "1/x"
            sech(arctanh(x)) ==> "sech(atanh(x))" // "sqrt(1 - x^2)"
            sech(arccsch(x)) ==> "sech(acsch(x))" // "(1 + 1/x^2)^(-1/2)"
            sech(arcsech(x)) ==> "sech(asech(x))" // "x"
            sech(arccoth(x)) ==> "sech(acoth(x))" // "sqrt(1 - 1/x^2)"

            Calculus.differentiate x (x*sech(x)) ==> "sech(x) - x*tanh(x)*sech(x)"

            // Operators.coth
            coth(0Q) ==> "coth(0)" // "⧝"
            coth(-2Q) ==> "coth(-2)" // "-coth(2)"
            coth(Constant I) ==> "coth(j)" // "-j*cot(1)"
            coth(-pi*Constant I) ==> "coth(-π*j)" // "-coth(π*j)" // "0"
            coth(-1Q/6Q*pi*Constant I) ==> "coth(-1/6*π*j)" // "-coth(1/6*π*j)" // "-sqrt(3)*j"
            coth(5Q/7Q*pi*Constant I) ==> "coth(5/7*π*j)" // "cot(2/7*π)*j"
            coth(3Q/2Q*pi*Constant I) ==> "coth(3/2*π*j)" // "0"

            coth(arcsinh(x)) ==> "coth(asinh(x))" // "sqrt(1 + x^2)/x"
            coth(arccosh(x)) ==> "coth(acosh(x))" // "x/(sqrt((-1 + x)/(1 + x))*(1 + x))"
            coth(arctanh(x)) ==> "coth(atanh(x))" // "1/x"
            coth(arccsch(x)) ==> "coth(acsch(x))" // "sqrt(1 + 1/x^2)*x"
            coth(arcsech(x)) ==> "coth(asech(x))" // "1/((1 + x)*sqrt((1 - x)/(1 + x)))"
            coth(arccoth(x)) ==> "coth(acoth(x))" // "x"

            Calculus.differentiate x (x*coth(x)) ==> "-(2*x)/(-1 + cosh(2*x)) + coth(x)" // "coth(x) - x*(csch(x))^2"

            // Operators.arcsin
            arcsin(-1Q) ==> "asin(-1)" // "-π/2"
            arcsin(-1Q/3Q) ==> "asin(-1/3)" // "-asin(1/3)" // "-asin(1/3)"
            arcsin(0Q) ==> "asin(0)" // "0"
            arcsin(1Q/2Q) ==> "asin(1/2)" // "π/6"
            arcsin(1Q) ==> "asin(1)" // "π/2"

            Calculus.differentiate x (x*arcsin(x)) ==> "x/sqrt(1 - x^2) + asin(x)"

            // Operators.arccos
            arccos(-1Q) ==> "acos(-1)" // "π"
            arccos(-1Q/3Q) ==> "acos(-1/3)"
            arccos(0Q) ==> "acos(0)" // "π/2"
            arccos(1Q/2Q) ==> "acos(1/2)" // "acos(1/2)" // "π/3"
            arccos(1Q) ==> "acos(1)" // "0"

            Calculus.differentiate x (x*arccos(x)) ==> "-x/sqrt(1 - x^2) + acos(x)"

            // Operators.arctan
            arctan(-1Q) ==> "atan(-1)" // "-atan(1)" // "-π/4"
            arctan(-1Q/3Q) ==> "atan(-1/3)" // "-atan(1/3)"
            arctan(0Q) ==> "atan(0)" // "0"
            arctan(1Q/2Q) ==> "atan(1/2)"
            arctan(1Q) ==> "atan(1)" // "π/4"

            Calculus.differentiate x (x*arctan(x)) ==> "x/(1 + x^2) + atan(x)"

            // Operators.arccsc
            arccsc(-1Q) ==> "acsc(-1)" // "-π/2"
            arccsc(-1Q/3Q) ==> "acsc(-1/3)" // "-acsc(1/3)"
            arccsc(0Q) ==> "acsc(0)" // "⧝"
            arccsc(1Q/2Q) ==> "acsc(1/2)" // "csc(1/2)"
            arccsc(1Q) ==> "acsc(1)" // "π/2"

            Calculus.differentiate x (x*arccsc(x)) ==> "-1/(sqrt(1 - 1/x^2)*x) + acsc(x)"

            // Operators.arcsec
            arcsec(-1Q) ==> "asec(-1)" // "π"
            arcsec(-1Q/3Q) ==> "asec(-1/3)"
            arcsec(0Q) ==> "asec(0)" // "⧝"
            arcsec(1Q/2Q) ==> "asec(1/2)"
            arcsec(1Q) ==> "asec(1)" // "0"

            Calculus.differentiate x (x*arcsec(x)) ==> "1/(sqrt(1 - 1/x^2)*x) + asec(x)"

            // Operators.arccot
            arccot(-1Q) ==> "acot(-1)" // "-π/4"
            arccot(-1Q/3Q) ==> "acot(-1/3)" // "-acot(1/3)"
            arccot(0Q) ==> "acot(0)" // "π/2"
            arccot(1Q/2Q) ==> "acot(1/2)"
            arccot(1Q) ==> "acot(1)" // "π/4"

            Calculus.differentiate x (x*arccot(x)) ==> "-x/(1 + x^2) + acot(x)"

            // Operators.arcsinh
            arcsinh(0Q) ==> "asinh(0)" // "0"
            arcsinh(-Constant I) ==> "asinh(-j)" // "-asinh(j)" // "-1/2*π*j"
            arcsinh(-1Q/2Q*Constant I) ==> "asinh(-j/2)" // "-asinh(j/2)" // "1/2*j"
            arcsinh(Constant I) ==> "asinh(j)" // "1/2*π*j"
            arcsinh(2Q*Constant I) ==> "asinh(2*j)" // "asin(2)*j"

            Calculus.differentiate x (x*arcsinh(x)) ==> "x/sqrt(1 + x^2) + asinh(x)"

            // Operators.arccosh
            arccosh(0Q) ==> "acosh(0)" // "1/2*π*j"
            arccosh(-Constant I) ==> "acosh(-j)"
            arccosh(-1Q/2Q*Constant I) ==> "acosh(-j/2)"
            arccosh(Constant I) ==> "acosh(j)"
            arccosh(2Q*Constant I) ==> "acosh(2*j)"

            Calculus.differentiate x (x*arccosh(x)) ==> "x/(sqrt(-1 + x)*sqrt(1 + x)) + acosh(x)"

            // Operators.arctanh
            arctanh(0Q) ==> "atanh(0)" // "0"
            arctanh(-Constant I) ==> "atanh(-j)" // "-atanh(j)" // "-1/4*π*j"
            arctanh(-1Q/2Q*Constant I) ==> "atanh(-j/2)" // "-atanh(j/2)" // "-atan(1/2)*j"
            arctanh(Constant I) ==> "atanh(j)" // "1/4*π*j"
            arctanh(2Q*Constant I) ==> "atanh(2*j)" // "atan(2)*j"

            Calculus.differentiate x (x*arctanh(x)) ==> "x/(1 - x^2) + atanh(x)"

            // Operators.arccsch
            arccsch(0Q) ==> "acsch(0)" // "⧝"
            arccsch(-Constant I) ==> "acsch(-j)" // "-acsch(j)" // "1/2*π*j"
            arccsch(-1Q/2Q*Constant I) ==> "acsch(-j/2)" // "-acsch(j/2)" // "acsc(1/2)*j"
            arccsch(Constant I) ==> "acsch(j)" // "-1/2*π*j"
            arccsch(2Q*Constant I) ==> "acsch(2*j)" // "-1/6*π*j"

            Calculus.differentiate x (arccsch(x)) ==> "-1/(sqrt(1 + 1/x^2)*x^2)" // "-1/(sqrt(1 + 1/x^2)*x)"
            Calculus.differentiate x (x*arccsch(x)) ==> "-1/(sqrt(1 + 1/x^2)*x) + acsch(x)" // "-1/(sqrt(1 + 1/x^2)*x) + acsch(x)"

            // Operators.arcsech
            arcsech(0Q) ==> "asech(0)" // "∞"
            arcsech(-Constant I) ==> "asech(-j)"
            arcsech(-1Q/2Q*Constant I) ==> "asech(-j/2)"
            arcsech(Constant I) ==> "asech(j)"
            arcsech(2Q*Constant I) ==> "asech(2*j)"

            Calculus.differentiate x (arcsech(x)) ==> "sqrt((1 - x)/(1 + x))/(x*(-1 + x))"
            Calculus.differentiate x (x*arcsech(x)) ==> "sqrt((1 - x)/(1 + x))/(-1 + x) + asech(x)"

            // Operators.arccoth
            arccoth(0Q) ==> "acoth(0)" // "1/2*π*j"
            arccoth(-Constant I) ==> "acoth(-j)" // "-acoth(j)" // "1/4*π*j"
            arccoth(-1Q/2Q*Constant I) ==> "acoth(-j/2)" // "-acoth(j/2)" // "acot(1/2)*j"
            arccoth(Constant I) ==> "acoth(j)" // "-1/4*π*j"
            arccoth(2Q*Constant I) ==> "acoth(2*j)" // "-atan(2)*j"

            Calculus.differentiate x (x*arccoth(x)) ==> "x/(1 - x^2) + acoth(x)"
        }
    ]
