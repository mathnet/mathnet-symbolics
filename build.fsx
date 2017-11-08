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


// CORE PACKAGES

let summary = "Math.NET Symbolics is a basic open source computer algebra library for .Net and Mono. Written in F# but works well in C# as well."
let description = "Math.NET Symbolics is a basic open source computer algebra library. Written in F# but works well in C# as well. "
let support = "Supports .Net 4.0 and Mono on Windows, Linux and Mac."
let tags = "math symbolics algebra simplify solve cas fsharp parse"

let libnet40 = "lib/net40"
let libnet45 = "lib/net45"
let libpcl47 = "lib/portable-net45+sl5+netcore45+MonoAndroid1+MonoTouch1"
let libpcl344 = "lib/portable-net45+sl5+netcore45+wpa81+wp8+MonoAndroid1+MonoTouch1"

let symbolicsPack =
    { Id = "MathNet.Symbolics"
      Release = symbolicsRelease
      Title = "Math.NET Symbolics"
      Summary = summary
      Description = description + support
      Tags = tags
      Authors = [ "Christoph Ruegg" ]
      FsLoader = false
      Dependencies =
        [ { FrameworkVersion=""
            Dependencies =
                [ "FParsec", GetPackageVersion "packages" "FParsec"
                  "FSharp.Core", GetPackageVersion "packages" "FSharp.Core"
                  "MathNet.Numerics", GetPackageVersion "packages" "MathNet.Numerics"
                  "MathNet.Numerics.FSharp", GetPackageVersion "packages" "MathNet.Numerics.FSharp" ] } ]
      Files =
        [ @"..\..\out\lib\Net40\MathNet.Symbolics.*", Some libnet40, None;
          @"..\..\out\lib\Profile47\MathNet.Symbolics.*", Some libpcl47, None;
          @"..\..\out\lib\Profile344\MathNet.Symbolics.*", Some libpcl344, None;
          @"MathNet.Symbolics.fsx", None, None;
          @"..\..\src\Symbolics\**\*.fs", Some "src/Common", None ] }

let coreBundle =
    { Id = symbolicsPack.Id
      Release = symbolicsRelease
      Title = symbolicsPack.Title
      Packages = [ symbolicsPack ] }


// --------------------------------------------------------------------------------------
// PREPARE
// --------------------------------------------------------------------------------------

Target "Start" DoNothing

Target "Clean" (fun _ ->
    CleanDirs [ "obj" ]
    CleanDirs [ "out/api"; "out/docs"; "out/packages" ]
    CleanDirs [ "out/lib/Net40"; "out/lib/Profile47"; "out/lib/Profile344" ]
    CleanDirs [ "out/test/Net40"; "out/test/Profile47"; "out/test/Profile344" ])

Target "ApplyVersion" (fun _ ->
    patchVersionInAssemblyInfo "src/Symbolics" symbolicsRelease
    patchVersionInAssemblyInfo "src/SymbolicsUnitTests" symbolicsRelease)

Target "Prepare" DoNothing
"Start"
  =?> ("Clean", not (hasBuildParam "incremental"))
  ==> "ApplyVersion"
  ==> "Prepare"


// --------------------------------------------------------------------------------------
// BUILD
// --------------------------------------------------------------------------------------

Target "BuildMain" (fun _ -> build !! "MathNet.Symbolics.sln")
Target "BuildAll" (fun _ -> build !! "MathNet.Symbolics.All.sln")

Target "Build" DoNothing
"Prepare"
  =?> ("BuildAll", hasBuildParam "all" || hasBuildParam "release")
  =?> ("BuildMain", not (hasBuildParam "all" || hasBuildParam "release"))
  ==> "Build"


// --------------------------------------------------------------------------------------
// TEST
// --------------------------------------------------------------------------------------

Target "Test" (fun _ -> test !! "out/test/**/*UnitTests*.dll")
"Build" ==> "Test"


// --------------------------------------------------------------------------------------
// CODE SIGN
// --------------------------------------------------------------------------------------

Target "Sign" (fun _ ->
    let fingerprint = "5dbea70701b40cab1b2ca62c75401342b4f0f03a"
    let timeserver = "https://time.certum.pl/"
    sign fingerprint timeserver (!! "out/lib/**/MathNet.Symbolics.dll" ))


// --------------------------------------------------------------------------------------
// PACKAGES
// --------------------------------------------------------------------------------------

Target "Pack" DoNothing

// ZIP

Target "Zip" (fun _ ->
    CleanDir "out/packages/Zip"
    coreBundle |> zip "out/packages/Zip" "out/lib" (fun f -> f.Contains("MathNet.Symbolics.") || f.Contains("MathNet.Numerics.") || f.Contains("FParsec") || f.Contains("FSharp.Core")))
"Build" =?> ("Sign", hasBuildParam "sign") ==> "Zip" ==> "Pack"

// NUGET

Target "NuGet" (fun _ ->
    CleanDir "out/packages/NuGet"
    if hasBuildParam "all" || hasBuildParam "release" then
        nugetPack coreBundle "out/packages/NuGet")
"Build" =?> ("Sign", hasBuildParam "sign") ==> "NuGet" ==> "Pack"


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
    !! "out/lib/Net40/MathNet.Symbolics.dll"
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

Target "PublishArchive" (fun _ -> publishArchive "out/packages/Zip" "out/packages/NuGet" [coreBundle])

Target "PublishNuGet" (fun _ -> !! "out/packages/NuGet/*.nupkg" -- "out/packages/NuGet/*.symbols.nupkg" |> publishNuGet)


Target "Publish" DoNothing
Dependencies "Publish" [ "PublishTag"; "PublishDocs"; "PublishApi"; "PublishArchive"; "PublishNuGet" ]


// --------------------------------------------------------------------------------------
// Default Targets
// --------------------------------------------------------------------------------------

Target "All" DoNothing
Dependencies "All" [ "Pack"; "Docs"; "Test" ] // skip "Api"

RunTargetOrDefault "Test"
