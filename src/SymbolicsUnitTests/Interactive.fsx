#nowarn "211"

#load "..\Symbolics\MathNet.Symbolics.fsx"

#I @"..\..\packages\FsUnit.1.3.0.1\Lib\Net40"
#I @"..\..\packages\NUnit.2.6.2\lib"
#I @"..\packages\FsUnit.1.3.0.1Lib\Net40"
#I @"..\packages\NUnit.2.6.2\lib"
#I @"packages\FsUnit.1.3.0.1\Lib\Net40"
#I @"packages\NUnit.2.6.2\lib"

#r "nunit.framework"
#r "FsUnit.NUnit"

open System
open System.Numerics
open Microsoft.FSharp
open MathNet.Numerics
open MathNet.Symbolics

fsi.AddPrinter Print.format
