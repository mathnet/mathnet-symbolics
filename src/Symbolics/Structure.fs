namespace MathNet.Symbolics

open MathNet.Symbolics

[<RequireQualifiedAccess>]
module Structure =

    open System.Collections.Generic
    open Operators

    [<CompiledName("NumberOfOperands")>]
    let numberOfOperands = function
        | Sum ax | Product ax -> List.length ax
        | Power _ -> 2
        | Function _ -> 1
        | FunctionN (_, xs) -> List.length xs
        | Number _ | Approximation _ | Identifier _ | Constant _ | ComplexInfinity | PositiveInfinity | NegativeInfinity -> 0
        | Undefined -> 0

    [<CompiledName("Operand")>]
    let operand i = function
        | Sum ax | Product ax | FunctionN (_, ax) -> List.nth ax i
        | Power (r, _) when i = 0 -> r
        | Power (_, p) when i = 1 -> p
        | Function (_, x) when i = 0 -> x
        | _ -> failwith "no such operand"

    [<CompiledName("IsFreeOf")>]
    let rec freeOf symbol x =
        if symbol = x then false else
        match x with
        | Sum ax | Product ax | FunctionN (_, ax) -> List.forall (freeOf symbol) ax
        | Power (r, p) -> freeOf symbol r && freeOf symbol p
        | Function (_, x) -> freeOf symbol x
        | Number _ | Approximation _ | Identifier _ | Constant _ |  ComplexInfinity | PositiveInfinity | NegativeInfinity -> true
        | Undefined -> true

    [<CompiledName("IsFreeOfSet")>]
    let rec freeOfSet (symbols: HashSet<Expression>) x =
        if symbols.Contains(x) then false else
        match x with
        | Sum ax | Product ax | FunctionN (_, ax) -> List.forall (freeOfSet symbols) ax
        | Power (r, p) -> freeOfSet symbols r && freeOfSet symbols p
        | Function (_, x) -> freeOfSet symbols x
        | Number _ | Approximation _ | Identifier _ | Constant _ |  ComplexInfinity | PositiveInfinity | NegativeInfinity -> true
        | Undefined -> true

    [<CompiledName("Substitute")>]
    let rec substitute y r x =
        if y = x then r else
        match x with
        | Sum ax -> sum <| List.map (substitute y r) ax
        | Product ax -> product <| List.map (substitute y r) ax
        | Power (radix, p) -> (substitute y r radix) ** (substitute y r p)
        | Function (fn, x) -> apply fn (substitute y r x)
        | FunctionN (fn, xs) -> applyN fn (List.map (substitute y r) xs)
        | Number _ | Approximation _ | Identifier _ | Constant _ |  ComplexInfinity | PositiveInfinity | NegativeInfinity -> x
        | Undefined -> x

    [<CompiledName("Map")>]
    let map f = function
        | Sum ax -> sum <| List.map f ax
        | Product ax -> product <| List.map f ax
        | Power (r, p) -> (f r) ** (f p)
        | Function (fn, x) -> apply fn (f x)
        | FunctionN (fn, xs) -> applyN fn (List.map f xs)
        | x -> x

    [<CompiledName("Fold")>]
    let fold f s = function
        | Sum ax | Product ax | FunctionN (_, ax) -> List.fold f s ax
        | Power (r, p) -> List.fold f s [r;p]
        | Function (_, x) -> f s x
        | _ -> s

    /// Sort expressions in a list with standard expression ordering.
    [<CompiledName("SortList")>]
    let sortList list =
        List.sortWith (fun a b -> if a = b then 0 elif orderRelation a b then -1 else 1) list

    /// Applies the given function to the expression tree and returns the result
    /// for each node where the function returns Some with some value.
    /// Subexpressions of an expression are only examined if the function returns
    /// None when applied to the expression.
    /// The results are returned as a list in reverse depth-first order.
    [<CompiledName("Collect")>]
    let collect (chooser:Expression->'T option) x =
        let rec impl (acc:'T list) x =
            match chooser x with
            | Some result -> result::acc
            | None -> fold impl acc x
        impl [] x

    /// Like collect but returns each result at most once.
    [<CompiledName("CollectDistinct")>]
    let collectDistinct chooser x =
        collect chooser x |> Seq.distinct |> List.ofSeq //potential for optimization...

    /// Collects all identifers of an expressions and returns their distinct expressions.
    [<CompiledName("CollectIdentifiers")>]
    let collectIdentifiers x =
        x |> collectDistinct (function | Identifier _ as expression -> Some expression | _ -> None) |> sortList

    /// Collects all identifers of an expressions and returns their distinct symbols.
    [<CompiledName("CollectIdentifierSymbols")>]
    let collectIdentifierSymbols x =
        x |> collectDistinct (function | Identifier symbol -> Some symbol | _ -> None) |> List.sort

    /// Collects all numbers of an expressions and returns their distinct expressions.
    [<CompiledName("CollectNumbers")>]
    let collectNumbers x =
        x |> collectDistinct (function | Number _ as expression -> Some expression | _ -> None) |> sortList

    /// Collects all numbers of an expressions and returns their distinct values.
    [<CompiledName("CollectNumberValues")>]
    let collectNumberValues x =
        x |> collectDistinct (function | Number number -> Some number | _ -> None) |> List.sort

    /// Collects all constants of an expressions and returns their distinct expressions.
    [<CompiledName("CollectConstants")>]
    let collectConstants x =
        x |> collectDistinct (function | Constant _ as expression -> Some expression | _ -> None) |> sortList

    /// Collects all constants of an expressions and returns their distinct values.
    [<CompiledName("CollectConstantValues")>]
    let collectConstantValues x =
        x |> collectDistinct (function | Constant constant -> Some constant | _ -> None) |> List.sort

    /// Collects all functions of an expressions and returns their distinct expressions.
    [<CompiledName("CollectFunctions")>]
    let collectFunctions x =
        x |> collectDistinct (function | Function _ | FunctionN _ as expression -> Some expression | _ -> None) |> sortList

    /// Collects all functions of an expressions and returns their distinct function types.
    [<CompiledName("CollectFunctionTypes")>]
    let collectFunctionTypes x =
        x |> collectDistinct (function | Function (f, _) | FunctionN (f, _) -> Some f | _ -> None) |> List.sort
