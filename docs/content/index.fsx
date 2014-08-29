(*** hide ***)
#I "../../out/lib/net40"

(**
Getting Started
===============

NuGet Packages
--------------

The recommended way to get Math.NET Symbolics is to use NuGet. The following packages are provided and maintained in the public [NuGet Gallery](https://nuget.org/profiles/mathnet/):

Core Package:

- **MathNet.Symbolics** - core package
- **MathNet.Symbolics.Parser** - optional extension to parse expressions from infix strings.

Platform Support and Dependencies
---------------------------------

- .Net 4.0 and Mono: Windows, Linux and Mac.

Package Dependencies:

- MathNet.Symbolics: None
- MathNet.Symbolics.Parser: [FParsec](http://www.nuget.org/packages/FParsec)


Using Math.NET Symbolics with F# and F# Interactive
---------------------------------------------------

Using Math.NET Symbolics with C#
--------------------------------

Building Math.NET Symbolics
---------------------------

If you do not want to use the official binaries, or if you like to modify, debug or contribute, you can compile Math.NET Symbolics locally either using Visual Studio or manually with the build scripts.

* The Visual Studio solutions should build out of the box, without any preparation steps or package restores.
* Instead of a compatible IDE you can also build the solutions with `msbuild`, or on Mono with `xbuild`.
* The full build including unit tests, docs, NuGet and Zip packages is using [FAKE](http://fsharp.github.io/FAKE/).

### How to build with MSBuild/XBuild

    [lang=sh]
    msbuild MathNet.Symbolics.sln            # only build for .Net 4 (main solution)
    xbuild MathNet.Symbolics.sln             # build with Mono, e.g. on Linux or Mac

### How to build with FAKE

    [lang=sh]
    build.cmd    # normal build (.Net 4.0), run unit tests (.Net on Windows)
    ./build.sh   # normal build (.Net 4.0), run unit tests (Mono on Linux/Mac, .Net on Windows)
    
    build.cmd Build              # normal build (.Net 4.0)
    build.cmd Build incremental  # normal build, incremental (.Net 4.0)
    
    build.cmd Test        # normal build (.Net 4.0), run unit tests
    build.cmd Test quick  # normal build (.Net 4.0), run unit tests except long running ones
    
    build.cmd Clean  # cleanup build artifacts
    build.cmd Docs   # generate documentation
    build.cmd Api    # generate api reference
    
    build.cmd NuGet all     # generate normal NuGet packages

    build.cmd All          # build, test, docs, api reference (.Net 4.0)
    build.cmd All release  # release build

FAKE itself is not included in the repository but it will download and bootstrap itself automatically when build.cmd is run the first time. Note that this step is *not* required when using Visual Studio or `msbuild` directly.

If the build or tests fail claiming that FSharp.Core was not be found, see [fsharp.org](http://fsharp.org/use/windows/) or install the [Visual F# 3.0 Tools](http://go.microsoft.com/fwlink/?LinkId=261286) directly.

*)
