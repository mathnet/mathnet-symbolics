namespace MathNet.Symbolics

open System.Xml.Linq
open System.IO

open MathNet.Numerics
open MathNet.Symbolics


module private MathMLParser =

    open Operators
    open ExpressionPatterns

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
            | "nums1", "pi" -> pi
            | "nums1", "e" -> Expression.E
            | "nums1", "i" -> Expression.I
            | "nums1", "infinity" -> PositiveInfinity
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


module private MathMLFormatter =

    open Operators
    open ExpressionPatterns

    let rec numerator = function
        | NegPower _ -> one
        | Product ax -> product <| List.map numerator ax
        | z -> z
    let rec denominator = function
        | NegPower (r, p) -> r ** -p
        | Product ax -> product <| List.map denominator ax
        | _ -> one

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
        | VisualExpression.Symbol s ->
            match s with
            | "pi" -> csymbol "nums1" "pi"
            | "e" -> csymbol "nums1" "e"
            | x -> leaf "ci" x
        | VisualExpression.Infinity -> csymbol "nums1" "infinity"
        | VisualExpression.ComplexInfinity -> csymbol "nums1" "infinity"
        | VisualExpression.Undefined -> csymbol "nums1" "NaN"
        | VisualExpression.ComplexI -> csymbol "nums1" "i"
        | VisualExpression.RealE -> csymbol "nums1" "e"
        | VisualExpression.RealPi -> csymbol "nums1" "pi"
        | VisualExpression.PositiveInteger i -> cn i
        | VisualExpression.PositiveFloatingPoint fp -> cn fp
        | VisualExpression.Parenthesis x -> formatContentStrict x
        | VisualExpression.Abs x -> failwith "not implemented"
        | VisualExpression.Negative (VisualExpression.PositiveInteger i) ->
            cn -i
        | VisualExpression.Negative (VisualExpression.PositiveFloatingPoint fp) ->
            cn -fp
        | VisualExpression.Negative x ->
            apply "arith1" "unary_minus" [ formatContentStrict x ]
        | VisualExpression.Sum xs ->
            apply "arith1" "plus" (List.map formatContentStrict xs)
        | VisualExpression.Product xs ->
            apply "arith1" "times" (List.map formatContentStrict xs)
        | VisualExpression.Fraction (VisualExpression.PositiveInteger n, VisualExpression.PositiveInteger d) ->
            apply "nums1" "rational" [ cn n; cn d ]
        | VisualExpression.Fraction (n, d) ->
            apply "arith1" "divide" [ formatContentStrict n; formatContentStrict d ]
        | VisualExpression.Power (r, p) ->
            apply "arith1" "power" [ formatContentStrict r; formatContentStrict p ]
        | VisualExpression.Root (r, p) ->
            apply "arith1" "root" [ formatContentStrict r; cn p ]
        | VisualExpression.Function (fn, power, x) -> failwith "not implemented"


    /// Format a semantics xml element containing both strict content representation and annotations.
    /// MathML3 2nd Edition; Strict Content. Annotations: TeX, Infix
    /// Future: ideally this routine would also provide annotations in MathML Presentation format, and maybe OpenMath.
    let formatSemanticsAnnotated x =
        let contentStrict = formatContentStrict x
        let tex = LaTeX.formatVisual x
        let infix = Infix.formatVisual x
        [
            contentStrict
            element "annotation" [| attribute "encoding" "application/x-tex"; box tex |]
            element "annotation" [| attribute "encoding" "application/x-mathnet-infix"; box infix |]
        ] |> node "semantics"


[<RequireQualifiedAccess>]
module Xml =
    let ofReader (reader:TextReader) = (XDocument.Load reader).Root
    let ofString (text:string) = use reader = new StringReader(text) in ofReader reader
    let toString (xml:XElement) = xml.ToString()
    let normalizeString = ofString >> toString


[<RequireQualifiedAccess>]
module MathML =

    let private defaultStyle = { VisualExpressionStyle.CompactPowersOfFunctions = false }

    [<CompiledName("FormatContentStrictVisual")>]
    let formatContentStrictVisual visualExpression =
        MathMLFormatter.formatContentStrict visualExpression |> Xml.toString

    /// Format the xml body equivalent to the provided expression, recursively, without headers, root and annotations.
    /// MathML3 2nd Edition; Strict Content.
    [<CompiledName("FormatContentStrict")>]
    let formatContentStrict expression =
        let visual = VisualExpression.fromExpression defaultStyle expression
        MathMLFormatter.formatContentStrict visual |> Xml.toString

    /// Format the xml body equivalent to the provided expression, recursively, without headers, root and annotations.
    /// MathML3 2nd Edition; Strict Content.
    [<CompiledName("FormatContentStrictXml")>]
    let formatContentStrictXml expression =
        let visual = VisualExpression.fromExpression defaultStyle expression
        MathMLFormatter.formatContentStrict visual

    /// Format a semantics xml element containing both strict content representation and annotations.
    /// MathML3 2nd Edition; Strict Content. Annotations: TeX, Infix
    /// Future: ideally this routine would also provide annotations in MathML Presentation format, and maybe OpenMath.
    [<CompiledName("FormatSemanticsAnnotated")>]
    let formatSemanticsAnnotated expression =
        let visual = VisualExpression.fromExpression defaultStyle expression
        MathMLFormatter.formatSemanticsAnnotated visual |> Xml.toString

    /// Format a semantics xml element containing both strict content representation and annotations.
    /// MathML3 2nd Edition; Strict Content. Annotations: TeX, Infix
    /// Future: ideally this routine would also provide annotations in MathML Presentation format, and maybe OpenMath.
    [<CompiledName("FormatSemanticsAnnotatedXml")>]
    let formatSemanticsAnnotatedXml expression =
        let visual = VisualExpression.fromExpression defaultStyle expression
        MathMLFormatter.formatSemanticsAnnotated visual

    /// Parse the provided xml string and interpret it as expressions.
    /// Future: ideally could also handle presentation and semantics-tag with annotations.
    [<CompiledName("Parse")>]
    let parse (text:string) =
        Xml.ofString text |> MathMLParser.parse

    /// Parse the provided xml text reader and interpret it as expressions.
    /// Future: ideally could also handle presentation and semantics-tag with annotations.
    [<CompiledName("ParseReader")>]
    let parseReader (reader:TextReader) =
        Xml.ofReader reader |> MathMLParser.parse

    /// Parse the provided xml and interpret it as expressions.
    /// Future: ideally could also handle presentation and semantics-tag with annotations.
    [<CompiledName("ParseXml")>]
    let parseXml (xml:XElement) =
        MathMLParser.parse xml
