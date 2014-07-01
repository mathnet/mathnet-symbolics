#nowarn "211"

#load "..\Symbolics\MathNet.Symbolics.fsx"

#I @"..\..\packages\FsUnit.1.2.1.0\Lib\Net40"
#I @"..\..\packages\NUnit.2.6.2\lib"
#I @"..\packages\FsUnit.1.2.1.0\Lib\Net40"
#I @"..\packages\NUnit.2.6.2\lib"
#I @"packages\FsUnit.1.2.1.0\Lib\Net40"
#I @"packages\NUnit.2.6.2\lib"

#r "nunit.framework"
#r "FsUnit.NUnit"

open System
open System.Numerics
open Microsoft.FSharp
open MathNet.Numerics
open MathNet.Symbolics

fsi.AddPrinter Text.format
