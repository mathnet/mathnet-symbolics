#nowarn "211"

#I @"..\..\packages\MathNet.Numerics.3.0.0-alpha8\lib\portable-net45+windows8+sl5"
#I @"..\..\packages\MathNet.Numerics.FSharp.3.0.0-alpha8\lib\portable-net45+windows8+sl5"
#I @"..\packages\MathNet.Numerics.3.0.0-alpha8\lib\portable-net45+windows8+sl5"
#I @"..\packages\MathNet.Numerics.FSharp.3.0.0-alpha8\lib\portable-net45+windows8+sl5"
#I @"packages\MathNet.Numerics.3.0.0-alpha8\lib\portable-net45+windows8+sl5"
#I @"packages\MathNet.Numerics.FSharp.3.0.0-alpha8\lib\portable-net45+windows8+sl5"

#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"

#load "Numbers.fs"
#load "Expressions.fs"
#load "Elementary.fs"
#load "Polynomial.fs"
#load "Quotations.fs"
#load "Linq.fs"
#load "Text.fs"

open System
open System.Numerics
open Microsoft.FSharp
open MathNet.Numerics
open MathNet.Symbolics

fsi.AddPrinter Text.format
