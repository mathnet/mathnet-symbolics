namespace MathNet.Symbolics

open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

type SymbolicExpression(expression:Expression) =

    member this.Expression = expression


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
    // OPERATORS
    // ...
