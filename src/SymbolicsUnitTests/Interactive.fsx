#nowarn "211"

#load "..\Symbolics\MathNet.Symbolics.fsx"

#I @"..\..\packages\FsUnit\Lib\Net40"
#I @"..\..\packages\NUnit\lib"
#I @"..\packages\FsUnit\Lib\Net40"
#I @"..\packages\NUnit\lib"
#I @"packages\FsUnit\Lib\Net40"
#I @"packages\NUnit\lib"

#r "nunit.framework"
#r "FsUnit.NUnit"

open System
open System.Numerics
open Microsoft.FSharp
open MathNet.Numerics
open MathNet.Symbolics

fsi.AddPrinter Print.format
