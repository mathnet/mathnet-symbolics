namespace MathNet.Symbolics

[<RequireQualifiedAccess>]
type ValueType =
    | Rational
    | Real
    | Complex
    | Undefined

type ValueTypeWrap =
    | RationalToReal
    | RealToComplex

type TypedExpression = EnrichedExpression<ValueType, ValueTypeWrap>

module TypedExpression =

    let derive (symbols: Map<Symbol, ValueType>) (expr:Expression) =

        let enrichedType e = EnrichedExpression.enrichment e |> Option.defaultValue ValueType.Undefined

        let cast desiredType e =
            match enrichedType e with
            | _ when desiredType = ValueType.Undefined -> e
            | actualType when actualType = desiredType -> e
            | ValueType.Rational when desiredType = ValueType.Real -> TypedExpression.Wrapped (ValueType.Real, RationalToReal, e)
            | ValueType.Rational when desiredType = ValueType.Complex -> TypedExpression.Wrapped (ValueType.Complex, RealToComplex, TypedExpression.Wrapped (ValueType.Real, RationalToReal, e))
            | ValueType.Real when desiredType = ValueType.Complex -> TypedExpression.Wrapped (ValueType.Complex, RealToComplex, e)
            | _ -> failwith "Not Supported"

        let mergeType a b =
            match a, b with
            | ValueType.Undefined, _ | _, ValueType.Undefined -> ValueType.Undefined
            | ValueType.Rational, ValueType.Rational -> ValueType.Rational
            | ValueType.Complex, _ | _, ValueType.Complex -> ValueType.Complex
            | ValueType.Real, _ | _, ValueType.Real -> ValueType.Real

        let deriveMergedTypeWith baseType expressions = expressions |> List.map enrichedType |> List.fold mergeType baseType

        let rec convert = function
            | Number n -> TypedExpression.Number (ValueType.Rational, n)
            | Approximation (Real _ as a) -> TypedExpression.Approximation (ValueType.Real, a)
            | Approximation (Complex _ as a) -> TypedExpression.Approximation (ValueType.Complex, a)
            | Identifier s -> TypedExpression.Identifier (symbols |> Map.find s, s)
            | Argument s -> TypedExpression.Identifier (symbols |> Map.find s, s)
            | Constant ((E | Pi) as c) -> TypedExpression.Constant (ValueType.Real, c)
            | Constant I -> TypedExpression.Constant (ValueType.Complex, I)
            | ComplexInfinity -> TypedExpression.ComplexInfinity ValueType.Complex
            | PositiveInfinity -> TypedExpression.ComplexInfinity ValueType.Real
            | NegativeInfinity -> TypedExpression.ComplexInfinity ValueType.Real
            | Undefined -> TypedExpression.Undefined
            | Sum xs ->
                let cs = xs |> List.map convert
                let t = cs |> deriveMergedTypeWith ValueType.Rational
                TypedExpression.Sum (t, cs |> List.map (cast t))
            | Product xs ->
                let cs = xs |> List.map convert
                let t = cs |> deriveMergedTypeWith ValueType.Rational
                TypedExpression.Product (t, cs |> List.map (cast t))
            | Power (a, b) ->
                let ca = convert a
                let cb = convert b
                let t = [ca; cb] |> deriveMergedTypeWith ValueType.Real
                TypedExpression.Power (t, (ca |> cast t, cb |> cast t))
            | Function (Abs, x) ->
                TypedExpression.Function (ValueType.Real, (Abs, convert x))
            | Function ((Ln | Lg | Exp) as f, x)
            | Function ((Sin | Cos | Tan | Csc | Sec | Cot | Sinh | Cosh | Tanh | Csch | Sech | Coth) as f, x)
            | Function ((Asin | Acos | Atan | Acsc | Asec | Acot | Asinh | Acosh | Atanh | Acsch | Asech | Acoth) as f, x)
            | Function ((AiryAi | AiryAiPrime | AiryBi | AiryBiPrime) as f, x) ->
                let cx = convert x
                TypedExpression.Function (mergeType (enrichedType cx) ValueType.Real, (f, cx))
            | FunctionN (Log, [b; x]) ->
                let cb = convert b
                let cx = convert x
                let t =
                    match enrichedType cb, enrichedType cx with
                    | (ValueType.Real | ValueType.Rational), (ValueType.Real | ValueType.Rational) -> ValueType.Real
                    | (ValueType.Real | ValueType.Rational), ValueType.Complex -> ValueType.Complex
                    | _ -> ValueType.Undefined
                TypedExpression.FunctionN (t, (Log, [cb; cx]))
            | FunctionN (Min, xs) ->
                let cs = xs |> List.map convert
                let t = cs |> List.map enrichedType |> List.fold mergeType ValueType.Real
                TypedExpression.FunctionN (t, (Min, cs))
            | FunctionN (Max, xs) ->
                let cs = xs |> List.map convert
                let t = cs |> List.map enrichedType |> List.fold mergeType ValueType.Real
                TypedExpression.FunctionN (t, (Max, cs))
            | FunctionN (Atan2, xs) ->
                let cs = xs |> List.map convert
                let t = cs |> List.map enrichedType |> List.fold mergeType ValueType.Real
                TypedExpression.FunctionN (t, (Atan2, cs))
            | FunctionN (Min, xs) ->
                let cs = xs |> List.map convert
                let t = cs |> List.map enrichedType |> List.fold mergeType ValueType.Real
                TypedExpression.FunctionN (t, (Min, cs))
            | FunctionN (Max, xs) ->
                let cs = xs |> List.map convert
                let t = cs |> List.map enrichedType |> List.fold mergeType ValueType.Real
                TypedExpression.FunctionN (t, (Max, cs))
            | FunctionN ((BesselJ | BesselY | BesselI | BesselK | BesselIRatio | BesselKRatio | HankelH1 | HankelH2) as f, [nu; z]) ->
                let cnu = convert nu
                let cz = convert z
                let t =
                    match enrichedType cnu, enrichedType cz with
                    | (ValueType.Real | ValueType.Rational), (ValueType.Real | ValueType.Rational) -> ValueType.Real
                    | (ValueType.Real | ValueType.Rational), ValueType.Complex -> ValueType.Complex
                    | _ -> ValueType.Undefined
                TypedExpression.FunctionN (t, (f, [cnu; cz]))
            | FunctionN _ -> failwith "not supported"

        convert expr
