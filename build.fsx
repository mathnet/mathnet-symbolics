//  __  __       _   _       _   _ ______ _______
// |  \/  |     | | | |     | \ | |  ____|__   __|
// | \  / | __ _| |_| |__   |  \| | |__     | |
// | |\/| |/ _` | __| '_ \  | . ` |  __|    | |
// | |  | | (_| | |_| | | |_| |\  | |____   | |
// |_|  |_|\__,_|\__|_| |_(_)_| \_|______|  |_|
//
// Math.NET Symbolics - https://symbolics.mathdotnet.com
// Copyright (c) Math.NET - Open Source MIT/X11 License
//
// FAKE build script, see http://fsharp.github.io/FAKE
//

// --------------------------------------------------------------------------------------
// PRELUDE
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "packages/build/FAKE/tools/FakeLib.dll"

open Fake
open Fake.DocuHelper
open System
open System.IO

#load "build/build-framework.fsx"
open BuildFramework


// --------------------------------------------------------------------------------------
// PROJECT INFO
// --------------------------------------------------------------------------------------

// VERSION OVERVIEW

let symbolicsRelease = release "Math.NET Symbolics" "RELEASENOTES.md"
let releases = [ symbolicsRelease ]
traceHeader releases


// SYMBOLICS PACKAGES

let symbolicsZipPackage = zipPackage "MathNet.Symbolics" "Math.NET Symbolics" symbolicsRelease false
let symbolicsNuGetPackage = nugetPackage "MathNet.Symbolics" symbolicsRelease
let symbolicsProject = project "MathNet.Symbolics" "src/Symbolics/Symbolics.fsproj" [symbolicsNuGetPackage]
let symbolicsSolution = solution "Symbolics" "MathNet.Symbolics.sln" [symbolicsProject] [symbolicsZipPackage]


// ALL

let allSolutions = [symbolicsSolution]
let allProjects = allSolutions |> List.collect (fun s -> s.Projects) |> List.distinct


// --------------------------------------------------------------------------------------
// PREPARE
// --------------------------------------------------------------------------------------

Target "Start" DoNothing

Target "Clean" (fun _ ->
    DeleteDirs (!! "src/**/obj/" ++ "src/**/bin/" )
    CleanDirs [ "out/api"; "out/docs" ]
    allSolutions |> List.iter (fun solution -> CleanDirs [ solution.OutputZipDir; solution.OutputNuGetDir; solution.OutputLibDir ])
    allSolutions |> List.iter clean)

Target "ApplyVersion" (fun _ ->
    allProjects |> List.iter patchVersionInProjectFile
    patchVersionInAssemblyInfo "src/Symbolics" symbolicsRelease
    patchVersionInAssemblyInfo "src/Symbolics.Tests" symbolicsRelease)

Target "Restore" (fun _ -> allSolutions |> List.iter restore)
"Start"
  =?> ("Clean", not (hasBuildParam "incremental"))
  ==> "Restore"

Target "Prepare" DoNothing
"Start"
  =?> ("Clean", not (hasBuildParam "incremental"))
  ==> "ApplyVersion"
  ==> "Prepare"



// --------------------------------------------------------------------------------------
// BUILD, SIGN, COLLECT
// --------------------------------------------------------------------------------------

let fingerprint = "490408de3618bed0a28e68dc5face46e5a3a97dd"
let timeserver = "http://time.certum.pl/"

Target "Build" (fun _ ->

    // Normal Build (without strong name, with certificate signature)
    CleanDirs (!! "src/**/obj/" ++ "src/**/bin/" )
    restore symbolicsSolution
    build symbolicsSolution
    if isWindows && hasBuildParam "sign" then sign fingerprint timeserver symbolicsSolution
    collectBinaries symbolicsSolution
    zip symbolicsZipPackage symbolicsSolution.OutputZipDir symbolicsSolution.OutputLibDir (fun f -> f.Contains("MathNet.Symbolics.") || f.Contains("MathNet.Numerics.") || f.Contains("System.Threading.") || f.Contains("FSharp.Core."))
    if isWindows then
        pack symbolicsSolution
        collectNuGetPackages symbolicsSolution

    // NuGet Sign (all or nothing)
    if isWindows && hasBuildParam "sign" then signNuGet fingerprint timeserver symbolicsSolution

    )
"Prepare" ==> "Build"


// --------------------------------------------------------------------------------------
// TEST
// --------------------------------------------------------------------------------------

let testSymbolics framework = test "src/Symbolics.Tests" "Symbolics.Tests.fsproj" framework
Target "TestSymbolics" DoNothing
//Target "TestSymbolicsCore1.1" (fun _ -> testSymbolics "netcoreapp1.1")
Target "TestSymbolicsCore2.0" (fun _ -> testSymbolics "netcoreapp2.0")
Target "TestSymbolicsNET45" (fun _ -> testSymbolics "net45")
Target "TestSymbolicsNET461" (fun _ -> testSymbolics "net461")
Target "TestSymbolicsNET47" (fun _ -> testSymbolics "net47")
//"Build" ==> "TestSymbolicsCore1.1"
"Build" ==> "TestSymbolicsCore2.0" ==> "TestSymbolics"
"Build" =?> ("TestSymbolicsNET45", isWindows)
"Build" =?> ("TestSymbolicsNET461", isWindows) ==> "TestSymbolics"
"Build" =?> ("TestSymbolicsNET47", isWindows)
Target "Test" DoNothing
"TestSymbolics" ==> "Test"


// --------------------------------------------------------------------------------------
// Documentation
// --------------------------------------------------------------------------------------

// DOCS

Target "CleanDocs" (fun _ -> CleanDirs ["out/docs"])

let extraDocs =
    [ "LICENSE.md", "License.md"
      "CONTRIBUTING.md", "Contributing.md"
      "CONTRIBUTORS.md", "Contributors.md" ]

Target "Docs" (fun _ ->
    provideDocExtraFiles extraDocs releases
    generateDocs true false)
Target "DocsDev" (fun _ ->
    provideDocExtraFiles extraDocs releases
    generateDocs true true)
Target "DocsWatch" (fun _ ->
    provideDocExtraFiles extraDocs releases
    use watcher = new FileSystemWatcher(DirectoryInfo("docs/content").FullName, "*.*")
    watcher.EnableRaisingEvents <- true
    watcher.Changed.Add(fun e -> generateDocs false true)
    watcher.Created.Add(fun e -> generateDocs false true)
    watcher.Renamed.Add(fun e -> generateDocs false true)
    watcher.Deleted.Add(fun e -> generateDocs false true)
    traceImportant "Waiting for docs edits. Press any key to stop."
    System.Console.ReadKey() |> ignore
    watcher.EnableRaisingEvents <- false
    watcher.Dispose())

"Build" ==> "CleanDocs" ==> "Docs"

"Start"
  =?> ("CleanDocs", not (hasBuildParam "incremental"))
  ==> "DocsDev"
  ==> "DocsWatch"


// API REFERENCE

Target "CleanApi" (fun _ -> CleanDirs ["out/api"])

Target "Api" (fun _ ->
    !! "src/Symbolics/bin/Release/net45/MathNet.Symbolics.dll"
    |> Docu (fun p ->
        { p with
            ToolPath = "tools/docu/docu.exe"
            TemplatesPath = "tools/docu/templates/"
            TimeOut = TimeSpan.FromMinutes 10.
            OutputPath = "out/api/" }))

"Build" ==> "CleanApi" ==> "Api"


// --------------------------------------------------------------------------------------
// Publishing
// Requires permissions; intended only for maintainers
// --------------------------------------------------------------------------------------

Target "PublishTag" (fun _ -> publishReleaseTag "Math.NET Symbolics" "" symbolicsRelease)

Target "PublishMirrors" (fun _ -> publishMirrors ())
Target "PublishDocs" (fun _ -> publishDocs symbolicsRelease)
Target "PublishApi" (fun _ -> publishApi symbolicsRelease)

Target "PublishArchive" (fun _ -> publishArchive symbolicsSolution)
Target "PublishNuGet" (fun _ -> publishNuGet !! (symbolicsSolution.OutputNuGetDir </> "/*.nupkg"))

Target "Publish" DoNothing
Dependencies "Publish" [ "PublishTag"; "PublishDocs"; "PublishApi"; "PublishArchive"; "PublishNuGet" ]


// --------------------------------------------------------------------------------------
// Default Targets
// --------------------------------------------------------------------------------------

Target "All" DoNothing
Dependencies "All" [ "Build"; "Docs"; "Test" ] // skip "Api"

RunTargetOrDefault "Test"
