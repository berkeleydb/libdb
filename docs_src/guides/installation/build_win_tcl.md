---
title: "Building the Tcl API"
api-name: "Building the Tcl API"
source: docs/installation/build_win_tcl.html
---
## Building the Tcl API

Tcl support is not built automatically. See <a href="../../guides/programmer_reference/tcl.md#tcl_intro" class="olink">Loading Berkeley DB with Tcl</a> for information on sites from which you can download Tcl and which Tcl versions are compatible with Berkeley DB. These notes assume that Tcl is installed as `d:\tcl`, but you can change that if you want.

The Tcl library must be built as the same build type as the Berkeley DB library (both Release or both Debug). We found that the binary release of Tcl can be used with the Release configuration of Berkeley DB, but you will need to build Tcl from sources for the Debug configuration. Before building Tcl, you will need to modify its makefile to make sure that you are building a debug version, including thread support. This is because the set of DLLs linked into the Tcl executable must match the corresponding set of DLLs used by Berkeley DB.

1.  Set the include directories.
    - In Visual Studio 2005/Visual Studio 2008 - Choose <span class="emphasis">*Tools -\> Options -\> Projects -\> VC++ Directories*</span>. Under the "Show directories for" pull-down, select "Include files". Add the full pathname for `d:\tcl\include`, then click OK.
    - In Visual Studio 2010 - Right-click db_tcl project, choose <span class="emphasis">*Properties-\>Configuration Properties-\> VC++ Directories-\>Include Directories*</span>. Add the full pathnames for `d:\tcl\include`, then click OK.

    This is the directory that contains `tcl.h`.

2.  Set the library files directory.
    - In Visual Studio 2005/Visual Studio 2008 - Choose <span class="emphasis">*Tools -\> Options -\> Projects -\> VC++ Directories*</span>. Under the "Show directories for" pull-down, select "Library files". Add the full pathname for the `d:\tcl\lib` directory, then click OK.
    - In Visual Studio 2010 - Right-click db_tcl project, choose <span class="emphasis">*Properties-\>Configuration Properties-\> VC++ Directories-\>Library Directories*</span>. Add the full pathname for the `d:\tcl\lib` directory, then click OK.

    This is the directory needed to find `tcl85g.lib` (or whatever the library is named in your distribution).

3.  Set the build type to Release or Debug in the drop-down on the tool bar.

4.  To build, right-click on db_tcl and select Build. This builds the Tcl support library for Berkeley DB, placing the result into one of the following Berkeley DB subdirectories, depending upon the configuration that you chose:

    |                                               |
    |-----------------------------------------------|
    | `build_windows\Win32\Debug\libdb_tcl53d.dll`  |
    | `build_windows\Win32\Release\libdb_tcl53.dll` |

If you use a version different from Tcl 8.5.x you will need to change the name of the Tcl library used in the build (for example, `tcl85g.lib`) to the appropriate name. To do this, right click on <span class="emphasis">*db_tcl*</span>, go to <span class="emphasis">*Properties -\> Linker -\> Input -\> Additional dependencies*</span> and change `tcl85g.lib` to match the Tcl version you are using.
