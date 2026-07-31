---
title: "Distributing DLLs"
api-name: "Distributing DLLs"
source: docs/installation/win_build_dist_dll.html
---
## Distributing DLLs

When distributing applications linked against the DLL (not static) version of the library, the DLL files you need will be found in one of the following Berkeley DB subdirectories, depending upon the configuration that you chose:

|                                      |
|--------------------------------------|
| `build_windows\Win32\Debug`          |
| `build_windows\Win32\Release`        |
| `build_windows\Win32\Debug_static`   |
| `build_windows\Win32\Release_static` |
| `build_windows\x64\Debug`            |
| `build_windows\x64\Release`          |
| `build_windows\x64\Debug_static`     |
| `build_windows\x64\Release_static`   |

You may also need to redistribute DLL files needed for the compiler's runtime. Generally, these runtime DLL files can be installed in the same directory that will contain your installed Berkeley DB DLLs. This directory may need to be added to your System PATH environment variable. Check your compiler's license and documentation for specifics on redistributing runtime DLLs.
