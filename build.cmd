@echo off
cls
if not exist tools\paket\paket.exe (
  tools\paket\paket.bootstrapper.exe
)
if not exist packages\FAKE\tools\FAKE.exe (
  tools\NuGet\NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
)
tools\paket\paket.exe install
packages\FAKE\tools\FAKE.exe build.fsx %*
