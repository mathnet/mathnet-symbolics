#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net
  [ ! -f tools/paket/paket.exe ] && tools/paket/paket.bootstrapper.exe
  [ ! -f packages/FAKE/tools/FAKE.exe ] && tools/NuGet/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
  tools/paket/paket.exe install
  packages/FAKE/tools/FAKE.exe build.fsx $@
else
  # use mono
  [ ! -f tools/paket/paket.exe ] && mono --runtime=v4.0 tools/paket/paket.bootstrapper.exe
  [ ! -f packages/FAKE/tools/FAKE.exe ] && mono --runtime=v4.0 tools/NuGet/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
  mono --runtime=v4.0 tools/paket/paket.exe install
  mono --runtime=v4.0 packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
fi
