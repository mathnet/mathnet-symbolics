namespace MathNet.Symbolics

open System
open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics


/// General Univariate Polynomial Expressions
module Polynomial =

    open System.Collections.Generic
    open Numbers
    open Elementary
    open ExpressionPatterns

    let rec isMonomial symbol = function
        | x when x = symbol -> true
        | Number _ -> true
        | PosIntPower (r, _) when r = symbol -> true
        | Product ax -> List.forall (isMonomial symbol) ax
        | x -> freeOf symbol x

    let isPolynomial symbol = function
        | Sum ax -> List.forall (isMonomial symbol) ax
        | x when isMonomial symbol x -> true
        | _ -> false

    let rec degreeMonomial symbol = function
        | x when x = zero -> negativeInfinity
        | x when x = symbol -> one
        | Number _ -> zero
        | PosIntPower (r, p) when r = symbol -> p
        | Product ax -> sum <| List.map (degreeMonomial symbol) ax
        | x when freeOf symbol x -> zero
        | _ -> undefined

    let degree symbol x =
        let d = degreeMonomial symbol x
        if d <> undefined then d else
        match x with
        | Sum ax -> max <| List.map (degreeMonomial symbol) ax
        | _ -> undefined

    let rec coefficientDegreeMonomial symbol = function
        | x when x = symbol -> one, one
        | Number _ as x -> x, zero
        | PosIntPower (r, p) when r = symbol -> one, p
        | Product ax ->
            let cds = List.map (coefficientDegreeMonomial symbol) ax
            product <| List.map fst cds, sum <| List.map snd cds
        | x when freeOf symbol x -> x, zero
        | _ -> undefined, undefined

    let coefficient symbol (k:int) x =
        let ke = number k
        let c, d = coefficientDegreeMonomial symbol x
        if d = ke then c else
        match x with
        | Sum ax -> List.map (coefficientDegreeMonomial symbol) ax |> List.filter (fun (_, d) -> d = ke) |> List.map fst |> sum
        | _ -> undefined

    let leadingCoefficientDegree symbol x =
        let c, d = coefficientDegreeMonomial symbol x
        if d <> undefined then c, d else
        match x with
        | Sum ax ->
            let cds = List.map (coefficientDegreeMonomial symbol) ax
            let degree = max <| List.map snd cds
            cds |> List.filter (fun (_, d) -> d = degree) |> List.map fst |> sum, degree
        | _ -> undefined, undefined

    let leadingCoefficient symbol x = leadingCoefficientDegree symbol x |> fst

    let coefficients symbol x =
        let rec collect symbol = function
            | x when x = symbol -> [1, one]
            | Number _ as a -> [0, a]
            | PosIntPower (r, Number n) when r = symbol -> [int n, one]
            | Sum ax -> List.collect (collect symbol) ax
            | Product ax -> List.map (collect symbol) ax |> List.reduce (fun a b -> a |> List.fold (fun s (o1, e1) -> b |> List.fold (fun s (o2, e2) -> (o1+o2,e1*e2)::s) s) [])
            | x when freeOf symbol x -> [0, x]
            | _ -> []
        let c = collect symbol x
        let degree = c |> Seq.map fst |> Seq.max
        c |> List.fold (fun (s:Expression[]) (o,e) -> s.[o] <- s.[o] + e; s) (Array.create (degree+1) zero)

    let rec collectTermsMonomial symbol = function
        | x when x = symbol -> (one, x)
        | Number _ as x-> (x, one)
        | PosIntPower (r, p) as x when r = symbol -> (one, x)
        | Product ax -> List.map (collectTermsMonomial symbol) ax |> List.reduce (fun (c1, v1) (c2, v2) -> (c1*c2, v1*v2))
        | x when freeOf symbol x -> (x, one)
        | _ -> (undefined, undefined)

    let collectTerms symbol = function
        | Sum ax -> List.map (collectTermsMonomial symbol) ax |> Seq.groupBy snd |> Seq.map (fun (v, cs) -> (Seq.map fst cs |> sumSeq) * v) |> sumSeq
        | x -> let c, v = collectTermsMonomial symbol x in if c <> undefined then c*v else undefined

    let polynomialDivision symbol u v =
        let n = degree symbol v
        if compareNumber n one < 0 then (u/v |> algebraicExpand, zero) else
        let lcv = leadingCoefficient symbol v
        let w = v - lcv*symbol**n
        let rec pd q r =
            let m = degree symbol r
            if compareNumber m n < 0 then q, r else
            let lcr = leadingCoefficient symbol r
            let s = lcr / lcv
            let z = symbol**(m-n)
            pd (q + s*z) ((r - lcr*symbol**m) - w*s*z |> algebraicExpand)
        pd zero u

    let quot symbol u v = polynomialDivision symbol u v |> fst
    let remainder symbol u v = polynomialDivision symbol u v |> snd

    let polynomialExpansion symbol t u v =
        let rec pe x =
            if x = zero then zero else
            let q, r = polynomialDivision symbol x v
            t * (pe q) + r |> algebraicExpand
        pe u |> collectTerms t

    /// Naive polynomial GCD (to be replaced)
    let polynomialGcd symbol u v =
        if u = zero && v = zero then zero else
        let rec gcd x y =
            if y = zero then x
            else gcd y (remainder symbol x y)
        let z = gcd u v in z / (leadingCoefficient symbol z) |> algebraicExpand


/// General Multivariate Polynomial Expressions
module MultivariatePolynomial =

    open System.Collections.Generic
    open Numbers
    open Elementary
    open ExpressionPatterns

    let symbols (xs: Expression list) = HashSet(List.toSeq xs, HashIdentity.Structural)

    let variables x =
        let rec impl keep = function
            | Number _ -> ()
            | PosIntPower (r, _) -> keep r
            | Power _ as p -> keep p
            | Sum ax -> ax |> List.iter (impl keep)
            | Product ax -> ax |> List.iter (fun a -> match a with | Sum _ as z -> keep z | _ -> impl keep a)
            | _ as z -> keep z
        let hs = symbols []
        impl (hs.Add >> ignore) x
        hs

    let rec isMonomialMV (symbols: HashSet<Expression>) = function
        | x when symbols.Contains(x) -> true
        | Number _ -> true
        | PosIntPower (r, _) when symbols.Contains(r) -> true
        | Product ax -> List.forall (isMonomialMV symbols) ax
        | x -> freeOfSet symbols x

    let isPolynomialMV (symbols: HashSet<Expression>) = function
        | Sum ax -> List.forall (isMonomialMV symbols) ax
        | x when isMonomialMV symbols x -> true
        | _ -> false

    let rec degreeMonomialMV (symbols: HashSet<Expression>) = function
        | x when x = zero -> negativeInfinity
        | x when symbols.Contains(x) -> one
        | Number _ -> zero
        | PosIntPower (r, p) when symbols.Contains(r) -> p
        | Product ax -> sum <| List.map (degreeMonomialMV symbols) ax
        | x when freeOfSet symbols x -> zero
        | _ -> undefined

    let degreeMV (symbols: HashSet<Expression>) x =
        let d = degreeMonomialMV symbols x
        if d <> undefined then d else
        match x with
        | Sum ax -> max <| List.map (degreeMonomialMV symbols) ax
        | _ -> undefined

    let totalDegree x = degreeMV (variables x) x

    let rec collectTermsMonomialMV (symbols: HashSet<Expression>) = function
        | x when symbols.Contains(x) -> (one, x)
        | Number _ as x-> (x, one)
        | PosIntPower (r, p) as x when symbols.Contains(r) -> (one, x)
        | Product ax -> List.map (collectTermsMonomialMV symbols) ax |> List.reduce (fun (c1, v1) (c2, v2) -> (c1*c2, v1*v2))
        | x when freeOfSet symbols x -> (x, one)
        | _ -> (undefined, undefined)

    let collectTermsMV (symbols: HashSet<Expression>) = function
        | Sum ax -> List.map (collectTermsMonomialMV symbols) ax |> Seq.groupBy snd |> Seq.map (fun (v, cs) -> (Seq.map fst cs |> sumSeq) * v) |> sumSeq
        | x -> let c, v = collectTermsMonomialMV symbols x in if c <> undefined then c*v else undefined


/// Single-Variable Polynomial (2*x+3*x^2)
module SingleVariablePolynomial =

    open Numbers
    open ExpressionPatterns

    let rec isMonomialSV symbol = function
        | x when x = symbol -> true
        | Number _ -> true
        | PosIntPower (r, _) when r = symbol -> true
        | Product ax -> List.forall (isMonomialSV symbol) ax
        | _ -> false

    let isPolynomialSV symbol = function
        | Sum ax -> List.forall (isMonomialSV symbol) ax
        | x when isMonomialSV symbol x -> true
        | _ -> false

    let rec degreeMonomialSV symbol = function
        | x when x = zero -> negativeInfinity
        | x when x = symbol -> one
        | Number _ -> zero
        | PosIntPower (r, p) when r = symbol -> p
        | Product ax -> sum <| List.map (degreeMonomialSV symbol) ax
        | _ -> undefined

    let degreeSV symbol x =
        let d = degreeMonomialSV symbol x
        if d <> undefined then d else
        match x with
        | Sum ax -> max <| List.map (degreeMonomialSV symbol) ax
        | _ -> undefined

    let rec coefficientMonomialSV symbol = function
        | x when x = symbol -> one
        | Number _ as x -> x
        | PosIntPower (r, _) when r = symbol -> one
        | Product ax -> product <| List.map (coefficientMonomialSV symbol) ax
        | _ -> undefined

    let rec coefficientDegreeMonomialSV symbol = function
        | x when x = zero -> x, negativeInfinity
        | x when x = symbol -> one, one
        | Number _ as x -> x, zero
        | PosIntPower (r, p) when r = symbol -> one, p
        | Product ax ->
            let cds = List.map (coefficientDegreeMonomialSV symbol) ax
            product <| List.map fst cds, sum <| List.map snd cds
        | _ -> undefined, undefined

    let coefficientSV symbol (k:int) x =
        let ke = number k
        let c, d = coefficientDegreeMonomialSV symbol x
        if d = ke then c else
        match x with
        | Sum ax -> List.map (coefficientDegreeMonomialSV symbol) ax |> List.filter (fun (_, d) -> d = ke) |> List.map fst |> sum
        | _ -> undefined

    let leadingCoefficientDegreeSV symbol x =
        let c, d = coefficientDegreeMonomialSV symbol x
        if d <> undefined then c, d else
        match x with
        | Sum ax ->
            let cds = List.map (coefficientDegreeMonomialSV symbol) ax
            let degree = max <| List.map snd cds
            cds |> List.filter (fun (_, d) -> d = degree) |> List.map fst |> sum, degree
        | _ -> undefined, undefined

    let leadingCoefficientSV symbol x = leadingCoefficientDegreeSV symbol x |> fst

    let coefficientsSV symbol x =
        let rec collect symbol = function
            | x when x = symbol -> [1, one]
            | Number _ as a -> [0, a]
            | PosIntPower (r, Number n) when r = symbol -> [int n, one]
            | Sum ax -> List.collect (collect symbol) ax
            | Product ax -> List.map (collect symbol) ax |> List.reduce (fun a b -> a |> List.fold (fun s (o1, e1) -> b |> List.fold (fun s (o2, e2) -> (o1+o2,e1*e2)::s) s) [])
            | _ -> []
        let c = collect symbol x
        let degree = c |> Seq.map fst |> Seq.max
        c |> List.fold (fun (s:Expression[]) (o,e) -> s.[o] <- s.[o] + e; s) (Array.create (degree+1) zero)
