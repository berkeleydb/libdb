---
title: "Chapter 5.  Building Berkeley DB for Windows"
api-name: "Chapter 5.  Building Berkeley DB for Windows"
source: docs/installation/build_win.html
---
## Chapter 5.  Building Berkeley DB for Windows

**Table of Contents**

<span class="sect1"> [Building Berkeley DB for 32 bit Windows](build_win.md#win_build32) </span>

<span class="sect2"> [Visual C++ .NET 2010](build_win.md#idp242512) </span>

<span class="sect2"> [Visual C++ .NET 2008](build_win.md#idp249264) </span>

<span class="sect2"> [Visual C++ .NET 2005](build_win.md#idp220616) </span>

<span class="sect2"> [Build results](build_win.md#idp205672) </span>

<span class="sect1"> [Building Berkeley DB for 64-bit Windows](win_build64.md) </span>

<span class="sect2"> [x64 build with Visual Studio 2005 or newer](win_build64.md#idp259672) </span>

<span class="sect1"> [Building Berkeley DB with Cygwin](win_build_cygwin.md) </span>

<span class="sect1"> [Building the C++ API](win_build_cxx.md) </span>

<span class="sect1"> [Building the C++ STL API](win_build_stl.md) </span>

<span class="sect1"> [Building the Java API](build_win_java.md) </span>

<span class="sect1"> [Building the SQL API](build_win_sql.md) </span>

<span class="sect2"> [Binary Compatibility With SQLite](build_win_sql.md#idp290248) </span>

<span class="sect2"> [Setting Preprocessor Flags](build_win_sql.md#idp276576) </span>

<span class="sect2"> [Enabling Extensions](build_win_sql.md#idp288280) </span>

<span class="sect2"> [Disabling Log Checksums](build_win_sql.md#win-disablechecksums) </span>

<span class="sect2"> [Building the JDBC Driver](build_win_sql.md#build_jdbc) </span>

<span class="sect2"> [Using the JDBC Driver](build_win_sql.md#idp266616) </span>

<span class="sect2"> [Building the ODBC Driver](build_win_sql.md#idp305704) </span>

<span class="sect2"> [Using the ADO.NET Driver](build_win_sql.md#idp320888) </span>

<span class="sect1"> [Building the Tcl API](build_win_tcl.md) </span>

<span class="sect1"> [Distributing DLLs](win_build_dist_dll.md) </span>

<span class="sect1"> [Additional build options](win_additional_options.md) </span>

<span class="sect1"> [Building a small memory footprint library](build_win_small.md) </span>

<span class="sect1"> [Running the test suite under Windows](build_win_test.md) </span>

<span class="sect2"> [Building the software needed by the tests](build_win_test.md#idp368040) </span>

<span class="sect2"> [Running the test suite under Windows](build_win_test.md#idp379184) </span>

<span class="sect2"> [Building the software needed by the SQL tests](build_win_test.md#build_win_test_sql) </span>

<span class="sect1"> [Windows notes](build_win_notes.md) </span>

<span class="sect1"> [Windows FAQ](build_win_faq.md) </span>

This chapter contains general instructions on building Berkeley DB for specific windows platforms using specific compilers. The <a href="build_win_faq.md" class="xref" title="Windows FAQ">Windows FAQ</a> also contains helpful information.

The `build_windows` directory in the Berkeley DB distribution contains project files for Microsoft Visual Studio:

|      Project File      |            Description             |
|:----------------------:|:----------------------------------:|
|    Berkeley_DB.sln     | Visual Studio 2005 (8.0) workspace |
|       \*.vcproj        | Visual Studio 2005 (8.0) projects  |
| Berkeley_DB_vs2010.sln |    Visual Studio 2010 workspace    |
|       \*.vcxproj       |    Visual Studio 2010 projects     |

These project files can be used to build Berkeley DB for the following platforms: Windows NT/2K/XP/2003/Vista and Windows7; and 64-bit Windows XP/2003/Vista and Windows7.

## Building Berkeley DB for 32 bit Windows

<span class="sect2"> [Visual C++ .NET 2010](build_win.md#idp242512) </span>

<span class="sect2"> [Visual C++ .NET 2008](build_win.md#idp249264) </span>

<span class="sect2"> [Visual C++ .NET 2005](build_win.md#idp220616) </span>

<span class="sect2"> [Build results](build_win.md#idp205672) </span>

### Visual C++ .NET 2010

1.  Choose <span class="emphasis">*File -\> Open -\> Project/Solution...*</span>. In the `build_windows` directory, select `Berkeley_DB_vs2010.sln` and click Open.
2.  Choose the desired project configuration from the drop-down menu on the tool bar (either Debug or Release).
3.  Choose the desired platform configuration from the drop-down menu on the tool bar (usually Win32 or x64).
4.  To build, right-click on the `Berkeley_DB_vs2010` solution and select Build Solution.

### Visual C++ .NET 2008

1.  Choose <span class="emphasis">*File -\> Open -\> Project/Solution...*</span>. In the `build_windows` directory, select `Berkeley_DB.sln` and click Open.
2.  The <span class="emphasis">*Visual Studio Conversion Wizard*</span> will open automatically. Click the <span class="emphasis">*Finish*</span> button.
3.  On the next screen click the <span class="emphasis">*Close*</span> button.
4.  Choose the desired project configuration from the drop-down menu on the tool bar (either Debug or Release).
5.  Choose the desired platform configuration from the drop-down menu on the tool bar (usually Win32 or x64).
6.  To build, right-click on the Berkeley_DB solution and select Build Solution.

### Visual C++ .NET 2005

1.  Choose <span class="emphasis">*File -\> Open -\> Project/Solution...*</span>. In the `build_windows` directory, select `Berkeley_DB.sln` and click Open
2.  Choose the desired project configuration from the drop-down menu on the tool bar (either Debug or Release).
3.  Choose the desired platform configuration from the drop-down menu on the tool bar (usually Win32 or x64).
4.  To build, right-click on the Berkeley_DB solution and select Build Solution.

### Build results

The results of your build will be placed in one of the following Berkeley DB subdirectories, depending on the configuration that you chose:

|                                      |
|--------------------------------------|
| `build_windows\Win32\Debug`          |
| `build_windows\Win32\Release`        |
| `build_windows\Win32\Debug_static`   |
| `build_windows\Win32\Release_static` |

When building your application during development, you should normally use compile options "Debug Multithreaded DLL" and link against `build_windows\Debug\libdb53d.lib`. You can also build using a release version of the Berkeley DB libraries and tools, which will be placed in `build_windows\Win32\Release\libdb53.lib`. When linking against the release build, you should compile your code with the "Release Multithreaded DLL" compile option. You will also need to add the `build_windows` directory to the list of include directories of your application's project, or copy the Berkeley DB include files to another location.
