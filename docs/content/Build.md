Building Math.NET Symbolics
===========================

If you do not want to use the official binaries, or if you like to modify,
debug or contribute, you can compile locally either using Visual Studio or
manually with the build scripts.

System Requirements
-------------------

* .NET Core SDK 3.1.1 ([download](https://dotnet.microsoft.com/download/dotnet-core/3.1))

VisualStudio or Xamarin Studio
------------------------------

We clearly separate dependency management from the IDE, you should therefore
run `restore.cmd` or `restore.sh` once after every git checkout in order to restore
the dependencies exactly as defined. Otherwise Visual Studio and other IDEs
may fail to compile or provide correct IntelliSense.

Tests can be run with the usual integrated NUnit test runners or ReSharper.

Command Line Tools
------------------

Instead of a compatible IDE you can also build the solutions directly with
the .NET Core SDK, with MsBuild or on Mono with XBuild. You may need to run `restore.cmd` or
`restore.sh` before, once after every git checkout in order to restore the dependencies.

    restore.cmd (or restore.sh)              # restore dependencies (once)
    dotnet build MathNet.Symbolics.sln       # with .NET Core SDK
    msbuild MathNet.Symbolics.sln            # with MsBuild
    xbuild MathNet.Symbolics.sln             # with Mono

FAKE
----

The fully automated build including unit tests, documentation and api
reference, NuGet and Zip packages is using [FAKE](https://fsharp.github.io/FAKE/).

FAKE itself is not included in the repository but it will download and bootstrap
itself automatically when build.cmd is run the first time. Note that this step
is *not* required when using Visual Studio or `msbuild` directly.

    ./build.sh   # normal build and unit tests, when using bash shell on Windows or Linux.
    build.cmd    # normal build and unit tests, when using Windows CMD shell.

    ./build.sh build              # normal build

    ./build.sh test          # normal build (.Net 4.0), run unit tests
    ./build.sh test quick    # normal build (.Net 4.0), run unit tests except long running ones

    ./build.sh clean         # cleanup build artifacts
    ./build.sh docs          # generate documentation
    ./build.sh api           # generate api reference
    
    ./build.sh all           # build, test, docs, api reference

If the build or tests fail claiming that FSharp.Core was not be found, see
[fsharp.org](https://fsharp.org/use/windows/) or install the
[Visual F# 3.0 Tools](https://go.microsoft.com/fwlink/?LinkId=261286) directly.

Dependencies
------------

We manage NuGet and other dependencies with [Paket](https://fsprojects.github.io/Paket/).
You do not normally have to do anything with Paket as it is integrated into our
FAKE build tools, unless you want to actively manage the dependencies.

`.paket/paket.exe restore` will restore the packages
to the exact version specified in the `paket.lock` file,
`.paket/paket.exe install` will install or migrate packages after you have
made changes to the `paket.dependencies` file, `.paket/paket.exe outdated`
will show whether any packages are out of date and `.paket/paket.exe update`
will update all packages within the defined constraints. Have a look at the Paket
website for more commands and details.

Documentation
-------------

This website and documentation is automatically generated from of a set of
[CommonMark](https://commonmark.org/) structured files in `doc/content/` using
[FSharp.Formatting](https://tpetricek.github.io/FSharp.Formatting/).
The final documentation can be built by calling `build.sh docs`.

However, for editing and previewing the docs on your local machine it is more
convenient to run `build.sh DocsWatch` in a separate console instead, which
monitors the content files and incrementally regenerates the HTML output
automatically. DocsWatch will also use local/relative URIs instead of absolute
ones, so that the links and styles will work as expected locally. This can
also be enabled in a full one-time build with `build.sh DocsDev` instead
of just `Docs`.

Creating a Release
------------------

While only maintainers can make official releases published on NuGet and
referred to from the website, you can use the same tools to make your own
releases for your own purposes.

Versioning is controlled by the release notes. Before building a new version,
first add a new release header and change notes on top of the `RELEASENOTES.md`
document in the root directory. The fake builds pick this up and propagate it
to the assembly info files automatically.

The build can then be launched by calling:

    ./build.sh all

The build script will print the current version as part of the the header banner,
which is also included in the release notes document in the build artifacts.
Example:

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
    // Math.NET Symbolics  v2.3.0-beta1

The artifacts are then ready in the `out/packages` directory.

Official Release Process (Maintainers only)
-------------------------------------------

*   Update `RELEASENOTES.md` file with relevant changes, attributed by contributor (if external). Set date.
*   Update `CONTRIBUTORS.md` file (via `git shortlog -sn`)

*   Build Release:

        build.sh all

*   Commit and push release notes and (auto-updated) assembly info files with new "Release: v1.2.3" commit

*   Publish Release:

        build.sh PublishDocs
        build.sh PublishApi
        build.sh PublishTag
        build.sh PublishArchive
        build.sh PublishNuGet

*   Consider a tweet via [@MathDotNet](https://twitter.com/MathDotNet)
*   Consider a post to the [Google+ site](https://plus.google.com/112484567926928665204)
