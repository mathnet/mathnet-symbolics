namespace MathNet.Symbolics

open System.Collections.Generic
open MathNet.Symbolics

open ExpressionPatterns
open Operators

/// General Polynomial Expressions
module Polynomial =

    [<CompiledName("Symbols")>]
    let symbols (xs: Expression list) = HashSet(List.toSeq xs, HashIdentity.Structural)

    [<CompiledName("Variables")>]
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

    [<CompiledName("IsMonomial")>]
    let rec isMonomial symbol = function
        | x when x = symbol -> true
        | Number _ -> true
        | PosIntPower (r, _) when r = symbol -> true
        | Product ax -> List.forall (isMonomial symbol) ax
        | x -> Structure.freeOf symbol x

    [<CompiledName("IsMultivariateMonomial")>]
    let rec isMonomialMV (symbols: HashSet<Expression>) = function
        | x when symbols.Contains(x) -> true
        | Number _ -> true
        | PosIntPower (r, _) when symbols.Contains(r) -> true
        | Product ax -> List.forall (isMonomialMV symbols) ax
        | x -> Structure.freeOfSet symbols x

    [<CompiledName("IsPolynomial")>]
    let isPolynomial symbol = function
        | Sum ax -> List.forall (isMonomial symbol) ax
        | x when isMonomial symbol x -> true
        | _ -> false

    [<CompiledName("IsMultivariatePolynomial")>]
    let isPolynomialMV (symbols: HashSet<Expression>) = function
        | Sum ax -> List.forall (isMonomialMV symbols) ax
        | x when isMonomialMV symbols x -> true
        | _ -> false

    [<CompiledName("MonomialDegree")>]
    let rec degreeMonomial symbol = function
        | x when x = zero -> NegativeInfinity
        | x when x = symbol -> one
        | Number _ -> zero
        | PosIntPower (r, p) when r = symbol -> p
        | Product ax -> sum <| List.map (degreeMonomial symbol) ax
        | x when Structure.freeOf symbol x -> zero
        | _ -> Undefined

    [<CompiledName("MultivariateMonomialDegree")>]
    let rec degreeMonomialMV (symbols: HashSet<Expression>) = function
        | x when x = zero -> NegativeInfinity
        | x when symbols.Contains(x) -> one
        | Number _ -> zero
        | PosIntPower (r, p) when symbols.Contains(r) -> p
        | Product ax -> sum <| List.map (degreeMonomialMV symbols) ax
        | x when Structure.freeOfSet symbols x -> zero
        | _ -> Undefined

    [<CompiledName("Degree")>]
    let degree symbol x =
        let d = degreeMonomial symbol x
        if d <> Undefined then d else
        match x with
        | Sum ax -> Numbers.max <| List.map (degreeMonomial symbol) ax
        | _ -> Undefined

    [<CompiledName("MultivariateDegree")>]
    let degreeMV (symbols: HashSet<Expression>) x =
        let d = degreeMonomialMV symbols x
        if d <> Undefined then d else
        match x with
        | Sum ax -> Numbers.max <| List.map (degreeMonomialMV symbols) ax
        | _ -> Undefined

    [<CompiledName("TotalDegree")>]
    let totalDegree x = degreeMV (variables x) x

    [<CompiledName("MonomialCoefficientDegree")>]
    let rec coefficientDegreeMonomial symbol = function
        | x when x = symbol -> one, one
        | Number _ as x -> x, zero
        | PosIntPower (r, p) when r = symbol -> one, p
        | Product ax ->
            let cds = List.map (coefficientDegreeMonomial symbol) ax
            product <| List.map fst cds, sum <| List.map snd cds
        | x when Structure.freeOf symbol x -> x, zero
        | _ -> Undefined, Undefined

    [<CompiledName("Coefficient")>]
    let coefficient symbol (k:int) x =
        let ke = number k
        let c, d = coefficientDegreeMonomial symbol x
        if d = ke then c else
        match x with
        | Sum ax -> List.map (coefficientDegreeMonomial symbol) ax |> List.filter (fun (_, d) -> d = ke) |> List.map fst |> sum
        | _ -> Undefined

    [<CompiledName("LeadingCoefficientDegree")>]
    let leadingCoefficientDegree symbol x =
        let c, d = coefficientDegreeMonomial symbol x
        if d <> Undefined then c, d else
        match x with
        | Sum ax ->
            let cds = List.map (coefficientDegreeMonomial symbol) ax
            let degree = Numbers.max <| List.map snd cds
            cds |> List.filter (fun (_, d) -> d = degree) |> List.map fst |> sum, degree
        | _ -> Undefined, Undefined

    [<CompiledName("LeadingCoefficient")>]
    let leadingCoefficient symbol x = leadingCoefficientDegree symbol x |> fst

    [<CompiledName("Coefficients")>]
    let coefficients symbol x =
        let rec collect symbol = function
            | x when x = symbol -> [1, one]
            | Number _ as a -> [0, a]
            | PosIntPower (r, Number n) when r = symbol -> [int n, one]
            | Sum ax -> List.collect (collect symbol) ax
            | Product ax -> List.map (collect symbol) ax |> List.reduce (fun a b -> a |> List.fold (fun s (o1, e1) -> b |> List.fold (fun s (o2, e2) -> (o1+o2,e1*e2)::s) s) [])
            | x when Structure.freeOf symbol x -> [0, x]
            | _ -> []
        let c = collect symbol x
        let degree = c |> Seq.map fst |> Seq.max
        c |> List.fold (fun (s:Expression[]) (o,e) -> s.[o] <- s.[o] + e; s) (Array.create (degree+1) zero)

    [<CompiledName("CollectMonomialTerms")>]
    let rec collectTermsMonomial symbol = function
        | x when x = symbol -> (one, x)
        | Number _ as x-> (x, one)
        | PosIntPower (r, p) as x when r = symbol -> (one, x)
        | Product ax -> List.map (collectTermsMonomial symbol) ax |> List.reduce (fun (c1, v1) (c2, v2) -> (c1*c2, v1*v2))
        | x when Structure.freeOf symbol x -> (x, one)
        | _ -> (Undefined, Undefined)

    [<CompiledName("CollectMultivariateMonomialTerms")>]
    let rec collectTermsMonomialMV (symbols: HashSet<Expression>) = function
        | x when symbols.Contains(x) -> (one, x)
        | Number _ as x-> (x, one)
        | PosIntPower (r, p) as x when symbols.Contains(r) -> (one, x)
        | Product ax -> List.map (collectTermsMonomialMV symbols) ax |> List.reduce (fun (c1, v1) (c2, v2) -> (c1*c2, v1*v2))
        | x when Structure.freeOfSet symbols x -> (x, one)
        | _ -> (Undefined, Undefined)

    [<CompiledName("CollectTerms")>]
    let collectTerms symbol = function
        | Sum ax -> List.map (collectTermsMonomial symbol) ax |> Seq.groupBy snd |> Seq.map (fun (v, cs) -> (Seq.map fst cs |> sumSeq) * v) |> sumSeq
        | x -> let c, v = collectTermsMonomial symbol x in if c <> Undefined then c*v else Undefined

    [<CompiledName("CollectMultivariateTerms")>]
    let collectTermsMV (symbols: HashSet<Expression>) = function
        | Sum ax -> List.map (collectTermsMonomialMV symbols) ax |> Seq.groupBy snd |> Seq.map (fun (v, cs) -> (Seq.map fst cs |> sumSeq) * v) |> sumSeq
        | x -> let c, v = collectTermsMonomialMV symbols x in if c <> Undefined then c*v else Undefined

    /// Euclidean division of polynomials.
    /// Returns a tuple with the quotient q and remainder such that u = q*v + r
    [<CompiledName("Divide")>]
    let divide symbol u v =
        let dv = degree symbol v
        if Numbers.compare dv one < 0 then (u/v |> Algebraic.expand, zero) else
        let lcv = leadingCoefficient symbol v
        let w = v - lcv*symbol**dv
        let rec pd q r =
            let dr = degree symbol r
            if Numbers.compare dr dv < 0 then q, r else
            let lcr = leadingCoefficient symbol r
            let z = (lcr / lcv) * symbol**(dr-dv)
            pd (q + z) ((r - lcr*symbol**dr) - w*z |> Algebraic.expand)
        pd zero u
    [<CompiledName("Quotient")>]
    let quot symbol u v = divide symbol u v |> fst
    [<CompiledName("Remainder")>]
    let remainder symbol u v = divide symbol u v |> snd

    /// Pseudo-division of polynomials (does not require coefficient divisibility and thus also supports integral domains).
    /// Returns a tuple with the pseudo-quotient q, pseudo-remainder r and factor b such that b*u = q*v+r.
    [<CompiledName("PseudoDivide")>]
    let pseudoDivide symbol u v  =
        let dv = degree symbol v
        if Numbers.compare dv one < 0 then (u, zero, v) else
        let lcv = leadingCoefficient symbol v
        let rec pd n q r =
            let dr = degree symbol r
            if Numbers.compare dr dv < 0 then
                let s=lcv**n
                Algebraic.expand (s*q), Algebraic.expand (s*r), lcv**((degree symbol u)-dv+1)
            else
                let lcr = leadingCoefficient symbol r
                let z = lcr * symbol**(dr-dv)
                pd (n-one) (lcv*q + z) (lcv*r - z*v |> Algebraic.expand)
        pd ((degree symbol u) - dv + one) zero u
    [<CompiledName("PseudoQuotient")>]
    let pseudoQuot symbol u v = let q,_,_ = pseudoDivide symbol u v in q
    [<CompiledName("PseudoRemainder")>]
    let pseudoRemainder symbol u v = let _,r,_ = pseudoDivide symbol u v in r

    [<CompiledName("PolynomialExpansion")>]
    let polynomialExpansion symbol t u v =
        let rec pe x =
            if x = zero then zero else
            let q, r = divide symbol x v
            t * (pe q) + r |> Algebraic.expand
        pe u |> collectTerms t

    [<CompiledName("Gcd")>]
    let gcd symbol u v =
        if u = zero && v = zero then zero else
        let rec inner x y =
            if y = zero then x
            else inner y (remainder symbol x y)
        let z = inner u v in z / (leadingCoefficient symbol z) |> Algebraic.expand

    /// Returns a tuple with the gcd and a such that a*u = gcd (mod v)
    [<CompiledName("HalfExtendedGcd")>]
    let halfExtendedGcd symbol u v =
         if u = zero && v = zero then (zero, zero) else
         let rec inner x y a' a'' =
            if y = zero then (x, a'') else
            let q, r = divide symbol x y
            inner y r (a'' - q*a') a'
         let z, a = inner u v zero one
         let c = leadingCoefficient symbol z
         Algebraic.expand (z/c), Algebraic.expand (a/c)

    /// Returns a tuple with gcd, a, b such that a*u + b*v = gcd(u,v)
    [<CompiledName("ExtendedGcd")>]
    let extendedGcd symbol u v =
        let z, a = halfExtendedGcd symbol u v
        let b = quot symbol (z-a*u |> Algebraic.expand) v
        z, a, b

    /// Returns a tuple a, b such that a*u = w (mod v)
    [<CompiledName("halfDiophantineGcd")>]
    let halfDiophantineGcd symbol u v w =
        let (g, s) = halfExtendedGcd symbol u v
        let (q, r) = divide symbol w g
        if r <> zero then Undefined else
        let s' = Algebraic.expand (q*s)
        if s' <> zero && Numbers.compare (degree symbol s') (degree symbol v) >= 0 then
            remainder symbol s' v
        else s'

    /// Returns a tuple a, b such that a*u + b*v = w
    [<CompiledName("DiophantineGcd")>]
    let diophantineGcd symbol u v w =
        let a = halfDiophantineGcd symbol u v w
        let b = quot symbol (w - a*u |> Algebraic.expand) v
        a, b

    /// Partial fraction decomposition (plumbing)
    [<CompiledName("PartialFraction")>]
    let rec partialFraction symbol numerator denominatorFactors =
        let rec impl n df =
            let a0, r = divide symbol n (Product df |> Algebraic.expand)
            match df with
            | [d] -> (a0, [r])
            | d::dx ->
                let a1, n' = diophantineGcd symbol (Product dx |> Algebraic.expand) d r
                let b0, ax = impl n' dx
                ((a0+b0), a1::ax)
            | _ -> (a0, [])
        impl numerator denominatorFactors


/// Single-Variable Polynomial (2*x+3*x^2)
module SingleVariablePolynomial =

    [<CompiledName("IsMonomial")>]
    let rec isMonomialSV symbol = function
        | x when x = symbol -> true
        | Number _ -> true
        | PosIntPower (r, _) when r = symbol -> true
        | Product ax -> List.forall (isMonomialSV symbol) ax
        | _ -> false

    [<CompiledName("IsPolynomial")>]
    let isPolynomialSV symbol = function
        | Sum ax -> List.forall (isMonomialSV symbol) ax
        | x when isMonomialSV symbol x -> true
        | _ -> false

    [<CompiledName("MonomialDegree")>]
    let rec degreeMonomialSV symbol = function
        | x when x = zero -> NegativeInfinity
        | x when x = symbol -> one
        | Number _ -> zero
        | PosIntPower (r, p) when r = symbol -> p
        | Product ax -> sum <| List.map (degreeMonomialSV symbol) ax
        | _ -> Undefined

    [<CompiledName("Degree")>]
    let degreeSV symbol x =
        let d = degreeMonomialSV symbol x
        if d <> Undefined then d else
        match x with
        | Sum ax -> Numbers.max <| List.map (degreeMonomialSV symbol) ax
        | _ -> Undefined

    [<CompiledName("MonomialCoefficient")>]
    let rec coefficientMonomialSV symbol = function
        | x when x = symbol -> one
        | Number _ as x -> x
        | PosIntPower (r, _) when r = symbol -> one
        | Product ax -> product <| List.map (coefficientMonomialSV symbol) ax
        | _ -> Undefined

    [<CompiledName("MonomialCoefficientDegree")>]
    let rec coefficientDegreeMonomialSV symbol = function
        | x when x = zero -> x, NegativeInfinity
        | x when x = symbol -> one, one
        | Number _ as x -> x, zero
        | PosIntPower (r, p) when r = symbol -> one, p
        | Product ax ->
            let cds = List.map (coefficientDegreeMonomialSV symbol) ax
            product <| List.map fst cds, sum <| List.map snd cds
        | _ -> Undefined, Undefined

    [<CompiledName("Coefficient")>]
    let coefficientSV symbol (k:int) x =
        let ke = number k
        let c, d = coefficientDegreeMonomialSV symbol x
        if d = ke then c else
        match x with
        | Sum ax -> List.map (coefficientDegreeMonomialSV symbol) ax |> List.filter (fun (_, d) -> d = ke) |> List.map fst |> sum
        | _ -> Undefined

    [<CompiledName("LeadingCoefficientDegree")>]
    let leadingCoefficientDegreeSV symbol x =
        let c, d = coefficientDegreeMonomialSV symbol x
        if d <> Undefined then c, d else
        match x with
        | Sum ax ->
            let cds = List.map (coefficientDegreeMonomialSV symbol) ax
            let degree = Numbers.max <| List.map snd cds
            cds |> List.filter (fun (_, d) -> d = degree) |> List.map fst |> sum, degree
        | _ -> Undefined, Undefined

    [<CompiledName("LeadingCoefficient")>]
    let leadingCoefficientSV symbol x = leadingCoefficientDegreeSV symbol x |> fst

    [<CompiledName("Coefficients")>]
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
