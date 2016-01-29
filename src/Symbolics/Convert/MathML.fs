namespace MathNet.Symbolics

open System.Numerics
open System.Xml.Linq
open System.IO

open MathNet.Numerics
open MathNet.Symbolics

[<RequireQualifiedAccess>]
module MathML =

    open Operators
    open ExpressionPatterns

    let textToXml (reader:TextReader) = (XDocument.Load reader).Root
    let stringToXml (text:string) = use reader = new StringReader(text) in textToXml reader

    let element name values = XElement(XName.Get(name), values)
    let attribute name value = XAttribute(XName.Get(name), value)
    let leaf name (body:string) = element name [|box body|]
    let empty name = element name Array.empty
    let node name (children:XElement list) = element name (children |> List.toArray |> Array.map box)

    let csymbol (dict:string) (name:string) = element "csymbol" [|attribute "cd" dict; name|]
    let cn value = leaf "cn" (sprintf "%A" value)
    let ci (Symbol s) = leaf "ci" s
    let apply dict operator args = node "apply" ((csymbol dict operator)::args)

    /// Format the xml body equivalent to the provided expression, recursively, without headers, root and annotations.
    /// MathML3 2nd Edition; Strict Content.
    let rec formatContentStrict = function
        | Integer i -> cn i.Numerator
        | Number n -> apply "nums1" "rational" [ cn n.Numerator; cn n.Denominator ]
        | Identifier s -> ci s
        | Constant E -> csymbol "nums1" "e"
        | Constant Pi -> csymbol "nums1" "pi"
        | Constant I -> csymbol "nums1" "i"
        | Constant (Constant.Real f) -> cn f
        | Sum xs -> apply "arith1" "plus" (List.map formatContentStrict xs)
        | Product (minusOne::xs) when minusOne = Expression.MinusOne -> apply "arith1" "unary_minus" [ formatContentStrict (product xs) ]
        | Product xs as p ->
            let n = InfixPrinter.numerator p
            let d = InfixPrinter.denominator p
            if d = one then apply "arith1" "times" (List.map formatContentStrict xs)
            else apply "arith1" "divide" [ formatContentStrict n; formatContentStrict d ]
        | NegIntPower (r, p) -> apply "arith1" "divide" [ cn 1; (if (p <> Expression.MinusOne) then apply "arith1" "power" [ formatContentStrict r; formatContentStrict -p ] else formatContentStrict r)]
        | Power (x, Number n) when n.IsPositive && n.Numerator = BigInteger.One -> apply "arith1" "root" [ formatContentStrict x; formatContentStrict (Number (BigRational.Reciprocal n)) ]
        | Power (x, Power(n, minusOne)) when minusOne = Expression.MinusOne -> apply "arith1" "root" [ formatContentStrict x; formatContentStrict n ]
        | Power (r, p) -> apply "arith1" "power" [ formatContentStrict r; formatContentStrict p ]
        | Function (f, x) -> failwith "not implemented"
        | FunctionN (f, xs) -> failwith "not implemented"
        | Infinity -> csymbol "nums1" "infinity"
        | ComplexInfinity -> csymbol "nums1" "infinity"
        | Undefined -> csymbol "nums1" "NaN"

    /// Format a semantics xml element containing both strict content representation and annotations.
    /// MathML3 2nd Edition; Strict Content. Annotations: TeX, Infix
    /// Future: ideally this routine would also provide annotations in MathML Presentation format, and maybe OpenMath.
    let formatSemanticsAnnotated x =
        let contentStrict = formatContentStrict x
        let tex = LaTeX.print x
        let infix = Infix.print x
        [
            contentStrict
            element "annotation" [| attribute "encoding" "application/x-tex"; box tex |]
            element "annotation" [| attribute "encoding" "application/x-mathnet-infix"; box infix |]
        ] |> node "semantics"

    let (|Element|) (xml:XElement) =
        xml.Name.LocalName, (xml.Attributes() |> Seq.toList), (xml.Elements() |> Seq.toList), xml.Value

    let (|Attribute|) (xml:XAttribute) =
        xml.Name.LocalName, xml.Value

    let (|ElementNamed|_|) name = function
        | Element (n, attributes, elements, value) when n=name -> Some (attributes, elements, value)
        | _ -> None

    let (|AttributeNamed|_|) name = function
        | Attribute (n, value) when n=name -> Some value
        | _ -> None

    let (|LeafNamed|_|) name = function
        | ElementNamed name ([], [], value) -> Some value
        | _ -> None

    let (|CSymbol|_|) = function
        | ElementNamed "csymbol" ([AttributeNamed "cd" dict], [], symbol) -> Some (dict, symbol)
        | _ -> None

    let (|Apply|_|) = function
        | ElementNamed "apply" ([], operator::operands, _) -> Some (operator, operands)
        | _ -> None

    /// Parse the provided xml and interpret it as expressions.
    /// Future: ideally could also handle presentation and semantics-tag with annotations.
    let rec parse = function
        | LeafNamed "ci" body -> symbol body
        | LeafNamed "cn" body -> Number (BigRational.Parse body)
        | CSymbol (dict, symbol) ->
            match dict, symbol with
            | "nums1", "pi" -> Expression.Pi
            | "nums1", "e" -> Expression.E
            | "nums1", "i" -> Expression.I
            | "nums1", "infinity" -> Infinity
            | "nums1", "NaN" -> Undefined
            | _ -> Undefined
        // MathML3 Strict Content: application
        | Apply (CSymbol (dict, symbol), operands) ->
            match dict, symbol, operands with
            | "nums1", "rational", [num; denom]
            | "arith1", "divide", [num; denom] -> (parse num)/(parse denom)
            | "arith1", "unary_minus", [op] -> -(parse op)
            | "arith1", "plus", ops -> ops |> List.map parse |> sum
            | "arith1", "times", ops -> ops |> List.map parse |> product
            | "arith1", "power", [radix; power] -> (parse radix)**(parse power)
            | "arith1", "root", [radix; degree] -> (parse radix)**(parse degree |> invert)
            | _ -> Undefined
        // MathML Non-Strict Content: application
        | Apply ((Element (operator, [], [], _)), operands) ->
            match operator, operands with
            | "minus", [op] -> parse op |> negate
            | "minus", [a; b] -> (parse a) - (parse b) |> negate
            | "plus", ops -> ops |> List.map parse |> sum
            | "times", ops -> ops |> List.map parse |> product
            | "divide", [num; denom] -> (parse num)/(parse denom)
            | "power", [radix; power] -> (parse radix)**(parse power)
            | "root", [ElementNamed "degree" ([], [degree], _); radix] -> (parse radix)**(parse degree |> invert)
            | "root", [radix] -> (parse radix) |> sqrt
            | _ -> Undefined
        | _ -> Undefined

    let parseText (reader:TextReader) = textToXml reader |> parse
    let parseString (text:string) = stringToXml text |> parse
