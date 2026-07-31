---
title: "Chapter 6. C# API"
api-name: "Chapter 6. C# API"
source: docs/programmer_reference/csharp.html
---
## Chapter 6. C# API

**Table of Contents**

<span class="sect1"> [Compatibility](csharp.md#csharp_compat) </span>

You can use Berkeley DB in your application through the C# API. To understand the application concepts relating to Berkeley DB, see the first few chapters of this manual. For a general discussion on how to build Berkeley DB applications, see the Berkeley DB Getting Started Guides of C or C++. You can also review the example code of C and C++ from the examples_c and examples_cxx directories. For a description of all the classes, functions, and enumerations of Berkeley DB C# API, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/csharp/Index.html" class="ulink" target="_top">Berkeley DB C# API Reference Guide.</a>

A separate Visual Studio solution is provided to build the Berkeley DB C# classes, the examples, and the native support library. See <a href="../../guides/installation/build_win_csharp.md#build_win_csharp.title" class="olink">Building the C# API</a> in the Berkeley DB Installation and Build Guide for more information.

The C# API requires .NET framework version 2.0 or above, and expects that it has already been installed on your system. For the sake of discussion, we assume that the Berkeley DB source is in a directory called db-<span class="emphasis">*VERSION*</span>; for example, you downloaded a Berkeley DB archive, and you did not change the top-level directory name. The files related to C# are in four subdirectories of db-<span class="emphasis">*VERSION*</span>: csharp (the C# source files), libdb_csharp (the C++ files that provide the "glue" between C# and Berkeley DB,) examples_csharp (containing all example code) and test\scr037 (containing NUnit tests for the API).

Building the C# API produces a managed assembly `libdb_dotnet`<span class="emphasis">*`VERSION`*</span>`.dll`, containing the API, and two native libraries: `libdb_csharp`<span class="emphasis">*`VERSION`*</span>`.dll` and `libdb`<span class="emphasis">*`VERSION`*</span>`.dll`. (For all three files, <span class="emphasis">*VERSION*</span> is \[MAJOR\]\[MINOR\], i.e. for version 4.8 the managed assembly is `libdb_dotnet48.dll`.) Following the existing convention, native libraries are placed in either `db-`<span class="emphasis">*`VERSION`*</span>`\build_windows\Win32`or `db-`<span class="emphasis">*`VERSION`*</span>`\build_windows\x64`, depending upon the platform being targeted. In all cases, the managed assembly will be placed in `db-`<span class="emphasis">*`VERSION`*</span>`\build_windows\AnyCPU`.

Because the C# API uses P/Invoke, for your application to use Berkeley DB successfully, the .NET framework needs to be able to locate the native libaries. This means the native libraries need to either be copied to your application's directory, the Windows or System directory, or the location of the libraries needs to be added to the `PATH` environment variable. See the MSDN documentation of the DllImport attribute and Dynamic-Link Library Search Order for further information.

If you get the following exception when you run, the .NET platform probably is unable to locate the native libraries:

``` c
System.TypeInitializationException
```

To ensure that everything is running correctly, you may want to try a simple test from the example programs in the `db-`<span class="emphasis">*`VERSION`*</span>`\examples_csharp` directory.

For example, the ex_access sample program will prompt for text input lines, which are then stored in a Btree database named `access.db`. It is designed to be run from either the `db-`<span class="emphasis">*`VERSION`*</span>`\build_windows\Debug` or `db-`<span class="emphasis">*`VERSION`*</span>`\build_windows\Release` directory. Try giving it a few lines of input text and then a blank line. Before it exits, you should see a list of the lines you entered display with data items. This is a simple check to make sure the fundamental configuration is working correctly.

## Compatibility

The Berkeley DB C# API has been tested with the Microsoft .NET Framework versions 2.0, 3.0, 3.5, and 4.0.
