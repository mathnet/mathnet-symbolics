let header = """
  __  __       _   _       _   _ ______ _______
 |  \/  |     | | | |     | \ | |  ____|__   __|
 | \  / | __ _| |_| |__   |  \| | |__     | |
 | |\/| |/ _` | __| '_ \  | . ` |  __|    | |
 | |  | | (_| | |_| | | |_| |\  | |____   | |
 |_|  |_|\__,_|\__|_| |_(_)_| \_|______|  |_|

 Math.NET Symbolics - https://symbolics.mathdotnet.com
 Copyright (c) Math.NET - Open Source MIT/X11 License

 FAKE build script, see https://fake.build/
"""

open FSharp.Core
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open System

open Model
open Building
open Testing
open Packaging
open Documentation
open Publishing


// --------------------------------------------------------------------------------------
// PRODUCT DEFINITION
// --------------------------------------------------------------------------------------

// VERSION OVERVIEW

let symbolicsRelease = release "symbolics" "Math.NET Symbolics" "RELEASENOTES.md"
let releases = [ symbolicsRelease ]


// SYMBOLICS PACKAGES

let symbolicsZipPackage = zipPackage "MathNet.Symbolics" "Math.NET Symbolics" symbolicsRelease
let symbolicsNuGetPackage = nugetPackage "MathNet.Symbolics" symbolicsRelease
let symbolicsProject = project "MathNet.Symbolics" "src/Symbolics/Symbolics.fsproj" [symbolicsNuGetPackage]
let symbolicsSolution = solution "Symbolics" "MathNet.Symbolics.sln" [symbolicsProject] [symbolicsZipPackage]


// ALL

let allSolutions = [symbolicsSolution]
let allProjects = allSolutions |> List.collect (fun s -> s.Projects) |> List.distinct


// --------------------------------------------------------------------------------------
// BUILD STEPS FOR DEFINED PRODUCTS
// --------------------------------------------------------------------------------------

let ``Clean`` _ =
    Shell.deleteDirs (!! "src/**/obj/" ++ "src/**/bin/" )
    Shell.cleanDirs [ "out/api"; "out/docs" ]
    allSolutions |> List.iter (fun solution -> Shell.cleanDirs [ solution.OutputZipDir; solution.OutputNuGetDir; solution.OutputLibDir; solution.OutputLibStrongNameDir ])

let ``Apply Version`` _ =
    allProjects |> List.iter Versioning.updateProject

let ``Restore`` _ =
    allSolutions |> List.iter restore

let fingerprint = "490408de3618bed0a28e68dc5face46e5a3a97dd"
let timeserver = "http://time.certum.pl/"

let ``Build`` _ =

    Shell.cleanDirs (!! "src/**/obj/" ++ "src/**/bin/" )
    restore symbolicsSolution
    build symbolicsSolution
    collectBinaries symbolicsSolution
    zip symbolicsZipPackage header symbolicsSolution.OutputZipDir symbolicsSolution.OutputLibDir (fun f -> f.Contains("MathNet.Symbolics.") || f.Contains("System.Threading.") || f.Contains("FSharp.Core."))
    pack symbolicsSolution
    collectNuGetPackages symbolicsSolution

let ``Test Symbolics`` framework _ = test "src/Symbolics.Tests" "Symbolics.Tests.fsproj" framework

let extraDocs =
    [ "LICENSE.md", "License.md"
      "CONTRIBUTING.md", "Contributing.md"
      "CONTRIBUTORS.md", "Contributors.md" ]

let ``Docs Clean`` _ =
    Shell.cleanDirs ["out/docs"]

let ``Docs Build`` _ =
    provideDocExtraFiles extraDocs releases
    buildDocs "out/docs"

let ``Docs Build and Watch`` _ =
    provideDocExtraFiles extraDocs releases
    buildDocs "out/docs"
    watchDocs "out/docs"

let ``API Clean`` _ =
    Shell.cleanDirs ["out/api"]

let ``API Build`` _ =
    let rootDir = Environment.CurrentDirectory
    let result =
        CreateProcess.fromRawCommandLine
            "tools/docu/docu.exe"
            ([
                rootDir </> "src/Symbolics/bin/Release/net48/MathNet.Numerics.dll" |> Path.getFullName
                "--output=" + (rootDir </> "out/api/" |> Path.getFullName)
                "--templates=" + (rootDir </> "tools/docu/templates/" |> Path.getFullName)
             ] |> String.concat " ")
        |> CreateProcess.withWorkingDirectory rootDir
        |> CreateProcess.withTimeout (TimeSpan.FromMinutes 10.)
        |> Proc.run
    if result.ExitCode <> 0 then failwith "Error during API reference generation."


// --------------------------------------------------------------------------------------
// BUILD TARGETS
// --------------------------------------------------------------------------------------

let initTargets incremental =

    // PREPARE
    Target.create "Start" ignore
    Target.create "Clean" ``Clean``
    Target.create "ApplyVersion" ``Apply Version``
    Target.create "Restore" ``Restore``
    "Start" =?> ("Clean", not incremental) ==> "Restore" |> ignore
    Target.create "Prepare" ignore
    "Start" =?> ("Clean", not incremental) ==> "ApplyVersion" ==> "Prepare" |> ignore

    // BUILD, SIGN, COLLECT
    Target.create "Build" ``Build``
    "Prepare" ==> "Build" |> ignore

    // TEST
    Target.create "TestSymbolics" ignore
    Target.create "TestSymbolicsNET80" (``Test Symbolics`` "net8.0")
    Target.create "TestSymbolicsNET48" (``Test Symbolics`` "net48")
    "Build" ==> "TestSymbolicsNET80" ==> "TestSymbolics" |> ignore
    "Build" =?> ("TestSymbolicsNET48", Environment.isWindows) ==> "TestSymbolics" |> ignore
    Target.create "Test" ignore
    "TestSymbolics" ==> "Test" |> ignore

    // DOCS
    Target.create "CleanDocs" ``Docs Clean``
    Target.create "Docs" ``Docs Build``
    Target.create "DocsDev" ``Docs Build``
    Target.create "DocsWatch" ``Docs Build and Watch``
    "Build" ==> "CleanDocs" ==> "Docs" |> ignore
    "Start" =?> ("CleanDocs", not incremental) ==> "DocsDev" ==> "DocsWatch" |> ignore

    // API REFERENCE
    Target.create "CleanApi" ``API Clean``
    Target.create "Api" ``API Build``
    "Build" ==> "CleanApi" ==> "Api" |> ignore

    // PUBLISHING
    // Requires permissions; intended only for maintainers
    Target.create "PublishTag" (fun _ -> publishReleaseTag "Math.NET Symbolics" "" symbolicsRelease)
    Target.create "PublishDocs" (fun _ -> publishDocs symbolicsRelease)
    Target.create "PublishApi" (fun _ -> publishApi symbolicsRelease)
    Target.create "PublishArchive" (fun _ -> publishArchives [symbolicsSolution])
    Target.create "PublishNuGet" (fun _ -> publishNuGet [symbolicsSolution])

    // COMPOSITE TARGETS
    Target.create "Publish" ignore
    "Publish" <== [ "PublishTag"; "PublishDocs"; "PublishApi"; "PublishArchive"; "PublishNuGet" ]

    Target.create "All" ignore
    "All" <== [ "Build"; "Docs"; "Api"; "Test" ]

// --------------------------------------------------------------------------------------
// MAIN PROGRAM
// --------------------------------------------------------------------------------------

[<EntryPoint>]
let main argv =

    Environment.CurrentDirectory <- Path.getFullName (__SOURCE_DIRECTORY__ </> "..")
    Trace.log Environment.CurrentDirectory

    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    Trace.log header
    let titleLength = releases |> List.map (fun r -> r.Title.Length) |> List.max
    for release in releases do
        Trace.log ([ " "; release.Title.PadRight titleLength; "  v"; release.PackageVersion ] |> String.concat "")
    Trace.log ""

    let args = Target.getArguments()
    let isStrongname, isSign, isIncremental =
        match args with
        | Some args ->
            args |> Seq.contains "--strongname",
            args |> Seq.contains "--sign" && Environment.isWindows,
            args |> Seq.contains "--incremental"
        | None -> false, false, false

    if isStrongname then Trace.log " Option: Strongnamed"
    if isSign then Trace.log " Option: Signed"
    if isIncremental then Trace.log " Option: Incremental"
    Trace.log ""

    DotNet.exec id "--info" "" |> ignore<ProcessResult>
    Trace.log ""

    initTargets isStrongname

    Target.runOrDefaultWithArguments "Test"

    0
