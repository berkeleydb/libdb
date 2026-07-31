---
title: "Building the C# API"
api-name: "Building the C# API"
source: docs/installation/build_win_csharp.html
---
## Building the C# API

The C# support is built by a separate Visual Studio solution and requires version 2.0 (or higher) of the .NET platform. If the Berkeley DB utilities are required, build Berkeley DB first following the instructions under <a href="build_win.md#win_build32" class="xref" title="Building Berkeley DB for 32 bit Windows">Building Berkeley DB for 32 bit Windows</a> or <a href="win_build64.md" class="xref" title="Building Berkeley DB for 64-bit Windows">Building Berkeley DB for 64-bit Windows</a>.

To build the C# API in Visual Studio 2005/Visual Studio 2008, the solution is `build_windows\BDB_dotnet.sln`; in Visual Studio 2010, the solution is `build_windows\BDB_dotnet_vs2010.sln`.

By default, the solution will build the native libraries, the managed assembly and all example programs. The NUnit tests need to be built explicitly because of their dependence upon the NUnit assembly. The native libraries will be placed in one of the following subdirectories, depending upon the chosen configuration:

|                               |
|-------------------------------|
| `build_windows\Win32\Debug`   |
| `build_windows\Win32\Release` |
| `build_windows\x64\Debug`     |
| `build_windows\x64\Release`   |

The managed assembly and all C# example programs will be placed in one of the following subdirectories, depending upon the chosen configuration:

|                                |
|--------------------------------|
| `build_windows\AnyCPU\Debug`   |
| `build_windows\AnyCPU\Release` |

The native libraries need to be locatable by the .NET platform, meaning they must be copied into an application's directory, the Windows or System directory, or their location must be added to the PATH environment variable. The example programs demonstrate how to programmatically edit the PATH variable.
