namespace MathNet.Symbolics

open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics


[<RequireQualifiedAccess>]
type SymbolicExpressionType =
    | RationalNumber = 1
    | RealNumber = 2
    | ComplexNumber = 3
    | Variable = 4
    | Sum = 5
    | Product = 6
    | Power = 7
    | Function = 9
    | ComplexInfinity = 10
    | PositiveInfinity = 11
    | NegativeInfinity = 12
    | Undefined = 13


[<StructuredFormatDisplay("{Expression}")>]
type SymbolicExpression(expression:Expression) =

    member this.Expression = expression

    member this.Type =
        match expression with
        | Number _ -> SymbolicExpressionType.RationalNumber
        | Approximation (Approximation.Real _) -> SymbolicExpressionType.RealNumber
        | Approximation (Approximation.Complex _) -> SymbolicExpressionType.ComplexNumber
        | Identifier _ -> SymbolicExpressionType.Variable
        | Constant I -> SymbolicExpressionType.ComplexNumber
        | Constant _ -> SymbolicExpressionType.RealNumber
        | Sum _ -> SymbolicExpressionType.Sum
        | Product _ -> SymbolicExpressionType.Product
        | Power _ -> SymbolicExpressionType.Power
        | Function _ | FunctionN _ -> SymbolicExpressionType.Function
        | ComplexInfinity -> SymbolicExpressionType.ComplexInfinity
        | PositiveInfinity -> SymbolicExpressionType.PositiveInfinity
        | NegativeInfinity -> SymbolicExpressionType.NegativeInfinity
        | Undefined -> SymbolicExpressionType.Undefined

    member this.RationalNumberValue =
        match expression with
        | Number n -> n
        | _ -> failwith "Not a rational number"

    member this.RealNumberValue =
        match expression with
        | Approximation (Approximation.Real fp) -> fp
        | Constant Pi -> Constants.Pi
        | Constant E -> Constants.E
        | _ -> failwith "Not a rational number"

    member this.ComplexNumberValue =
        match expression with
        | Approximation (Approximation.Complex fp) -> fp
        | Constant I -> Complex.ImaginaryOne
        | _ -> failwith "Not a rational number"

    member this.VariableName =
        match expression with
        | Identifier (Symbol s) -> s
        | _ -> failwith "Not a variable"


    // LEAFS - Integer
    static member Zero = SymbolicExpression(Expression.Zero)
    static member One = SymbolicExpression(Expression.One)
    static member Two = SymbolicExpression(Expression.Two)
    static member MinusOne = SymbolicExpression(Expression.MinusOne)
    static member FromInt32(x:int32) = SymbolicExpression(Expression.FromInt32(x))
    static member FromInt64(x:int64) = SymbolicExpression(Expression.FromInt64(x))
    static member FromInteger(x:BigInteger) = SymbolicExpression(Expression.FromInteger(x))
    static member FromIntegerFraction(n:BigInteger, d:BigInteger) = SymbolicExpression(Expression.FromIntegerFraction(n, d))
    static member FromRational(x:BigRational) = SymbolicExpression(Expression.FromRational(x))

    // LEAFS - Approximations
    static member Real(approximation:float) = SymbolicExpression(Expression.Real(approximation))

    // LEAFS - Constants
    static member I = SymbolicExpression(Expression.I)
    static member E = SymbolicExpression(Expression.E)
    static member Pi = SymbolicExpression(Expression.Pi)

    // LEAFS - Mathematical Symbols
    static member PositiveInfinity = SymbolicExpression(Expression.PositiveInfinity)
    static member NegativeInfinity = SymbolicExpression(Expression.NegativeInfinity)
    static member ComplexInfinity = SymbolicExpression(Expression.ComplexInfinity)
    static member Undefined = SymbolicExpression(Expression.Undefined)

    // LEAFS - Symbols
    static member Variable(name:string) = SymbolicExpression(Expression.Symbol(name))


    // PARSING

    static member Parse(infix:string) : SymbolicExpression =
        SymbolicExpression(Infix.parseOrThrow infix)

    static member ParseMathML(mathml:string) : SymbolicExpression =
        SymbolicExpression(MathML.parse mathml)

    static member ParseExpression(expression:System.Linq.Expressions.Expression) : SymbolicExpression =
        SymbolicExpression(Linq.parse expression)

    static member ParseQuotation(quotation:Microsoft.FSharp.Quotations.Expr) : SymbolicExpression =
        SymbolicExpression(Quotations.parse quotation)


    // FORMATTING

    override this.ToString() : string =
        Infix.format expression

    member this.ToInternalString() : string =
        Infix.formatStrict expression

    member this.ToLaTeX() : string =
        LaTeX.format expression

    member this.ToMathML() : string =
        MathML.formatContentStrict expression

    member this.ToSemanticMathML() : string =
        MathML.formatSemanticsAnnotated expression


    // EVALUATION & COMPILATION


    // CASTING

    static member op_Implicit (x:Expression) : SymbolicExpression = SymbolicExpression(x)
    static member op_Implicit (x:string) : SymbolicExpression = SymbolicExpression.Parse(x)

    static member op_Implicit (x:int32) : SymbolicExpression = SymbolicExpression.FromInt32(x)
    static member op_Implicit (x:int64) : SymbolicExpression = SymbolicExpression.FromInt64(x)
    static member op_Implicit (x:BigInteger) : SymbolicExpression = SymbolicExpression.FromInteger(x)
    static member op_Implicit (x:BigRational) : SymbolicExpression = SymbolicExpression.FromRational(x)
    static member op_Implicit (x:float) : SymbolicExpression = SymbolicExpression.Real(x)

    // bad idea, don't do this
    // static member op_Implicit (x:SymbolicExpression) : Expression = x.Expression


    // OPERATORS

    static member ( ~+ ) (x:SymbolicExpression) = SymbolicExpression(+x.Expression)
    static member ( ~- ) (x:SymbolicExpression) = SymbolicExpression(-x.Expression)
    static member ( + ) ((x:SymbolicExpression), (y:SymbolicExpression)) = SymbolicExpression(x.Expression + y.Expression)
    static member ( - ) ((x:SymbolicExpression), (y:SymbolicExpression)) = SymbolicExpression(x.Expression - y.Expression)
    static member ( * ) ((x:SymbolicExpression), (y:SymbolicExpression)) = SymbolicExpression(x.Expression * y.Expression)
    static member ( / ) ((x:SymbolicExpression), (y:SymbolicExpression)) = SymbolicExpression(x.Expression / y.Expression)

    static member Sum([<System.ParamArray>] summands : SymbolicExpression array) = SymbolicExpression(summands |> Seq.map (fun x -> x.Expression) |> Operators.sumSeq)
    static member Sum(summands : SymbolicExpression seq) = SymbolicExpression(summands |> Seq.map (fun x -> x.Expression) |> Operators.sumSeq)
    static member Product([<System.ParamArray>] factors : SymbolicExpression array) = SymbolicExpression(factors |> Seq.map (fun x -> x.Expression) |> Operators.productSeq)
    static member Product(factors : SymbolicExpression seq) = SymbolicExpression(factors |> Seq.map (fun x -> x.Expression) |> Operators.productSeq)

    member this.Negate() = -this
    member this.Add(x:SymbolicExpression) = this + x
    member this.Subtract(x:SymbolicExpression) = this - x
    member this.Multiply(x:SymbolicExpression) = this * x
    member this.Divide(x:SymbolicExpression) = this / x

    member this.Pow(power:SymbolicExpression) = SymbolicExpression(Expression.Pow(expression, power.Expression))
    member this.Invert() = SymbolicExpression(Expression.Invert(expression))

    member this.Abs() = SymbolicExpression(Expression.Abs(expression))

    member this.Root(n:SymbolicExpression) = SymbolicExpression(Expression.Root(n.Expression, expression))
    member this.Sqrt() = SymbolicExpression(Expression.Sqrt(expression))

    member this.Exp() = SymbolicExpression(Expression.Exp(expression))
    member this.Ln() = SymbolicExpression(Expression.Ln(expression))
    member this.Log() = SymbolicExpression(Expression.Log(expression))
    member this.Log(basis:SymbolicExpression) = SymbolicExpression(Expression.Log(basis.Expression, expression))

    member this.Sin() = SymbolicExpression(Expression.Sin(expression))
    member this.Cos() = SymbolicExpression(Expression.Cos(expression))
    member this.Tan() = SymbolicExpression(Expression.Tan(expression))
    member this.Sec() = SymbolicExpression(Expression.Sec(expression))
    member this.Csc() = SymbolicExpression(Expression.Csc(expression))
    member this.Cot() = SymbolicExpression(Expression.Cot(expression))
    member this.Sinh() = SymbolicExpression(Expression.Sinh(expression))
    member this.Cosh() = SymbolicExpression(Expression.Cosh(expression))
    member this.Tanh() = SymbolicExpression(Expression.Tanh(expression))
    member this.ArcSin() = SymbolicExpression(Expression.ArcSin(expression))
    member this.ArcCos() = SymbolicExpression(Expression.ArcCos(expression))
    member this.ArcTan() = SymbolicExpression(Expression.ArcTan(expression))


    // STRUCTURE
    member this.Substitute(x:SymbolicExpression, replacement:SymbolicExpression) = SymbolicExpression(expression |> Structure.substitute x.Expression replacement.Expression)


    // ALGEBRAIC
    member this.Expand() = SymbolicExpression(Algebraic.expand expression)
    member this.ExpandMain() = SymbolicExpression(Algebraic.expandMain expression)


    // CALCULUS
    member this.Differentiate(variable:SymbolicExpression) = SymbolicExpression(expression |> Calculus.differentiate variable.Expression)
    member this.DifferentiateAt(variable:SymbolicExpression, value:SymbolicExpression) = SymbolicExpression(expression |> Calculus.differentiateAt variable.Expression value.Expression)
    member this.Taylor(k:int, variable:SymbolicExpression, value:SymbolicExpression) = SymbolicExpression(expression |> Calculus.taylor k variable.Expression value.Expression)
    member this.TangentLine(variable:SymbolicExpression, value:SymbolicExpression) = SymbolicExpression(expression |> Calculus.tangentLine variable.Expression value.Expression)
    member this.NormalLine(variable:SymbolicExpression, value:SymbolicExpression) = SymbolicExpression(expression |> Calculus.normalLine variable.Expression value.Expression)


    // POLYNOMIAL


    // RATIONAL
    member this.Numerator() = SymbolicExpression(Rational.numerator expression)
    member this.Denominator() = SymbolicExpression(Rational.denominator expression)
    member this.Rationalize() = SymbolicExpression(Rational.rationalize expression)
    member this.RationalReduce() = SymbolicExpression(Rational.reduce expression)
    member this.RationalExpand() = SymbolicExpression(Rational.expand expression)
    member this.RationalSimplify(variable:SymbolicExpression) = SymbolicExpression(expression |> Rational.simplify variable.Expression)


    // EXPONENTIAL
    member this.ExponentialExpand() = SymbolicExpression(Exponential.expand expression)
    member this.ExponentialContract() = SymbolicExpression(Exponential.contract expression)
    member this.ExponentialSimplify() = SymbolicExpression(Exponential.simplify expression)


    // TRIGONOMETRIC
    member this.TrigonometricExpand() = SymbolicExpression(Trigonometric.expand expression)
    member this.TrigonometricContract() = SymbolicExpression(Trigonometric.contract expression)
    member this.TrigonometricSubstitute() = SymbolicExpression(Trigonometric.substitute expression)
    member this.TrigonometricSimplify() = SymbolicExpression(Trigonometric.simplify expression)


    // APPROXIMATE
    member this.Approximate() = SymbolicExpression(Approximate.approximate expression)
