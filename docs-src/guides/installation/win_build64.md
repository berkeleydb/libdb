---
title: "Building Berkeley DB for 64-bit Windows"
api-name: "Building Berkeley DB for 64-bit Windows"
source: docs/installation/win_build64.html
---
## Building Berkeley DB for 64-bit Windows

<span class="sect2"> [x64 build with Visual Studio 2005 or newer](win_build64.md#idp259672) </span>

The following procedure can be used to build natively on a 64-bit system or to cross-compile from a 32-bit system.

When building 64-bit binaries, the output directory will be one of the following Berkeley DB subdirectories, depending upon the configuration that you chose:

|                                    |
|------------------------------------|
| `build_windows\x64\Debug`          |
| `build_windows\x64\Release`        |
| `build_windows\x64\Debug_static`   |
| `build_windows\x64\Release_static` |

### x64 build with Visual Studio 2005 or newer

1.  Follow the build instructions for your version of Visual Studio, as described in <a href="build_win.md#win_build32" class="xref" title="Building Berkeley DB for 32 bit Windows">Building Berkeley DB for 32 bit Windows</a>.
2.  Select <span class="emphasis">*x64*</span> from the <span class="emphasis">*Platform Configuration*</span> dropdown.
3.  Right click on <span class="emphasis">*Solution 'Berkeley_DB'*</span> in the Solution Explorer, and select <span class="emphasis">*Build Solution*</span>
