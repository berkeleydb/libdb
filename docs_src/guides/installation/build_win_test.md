---
title: "Running the test suite under Windows"
api-name: "Running the test suite under Windows"
source: docs/installation/build_win_test.html
---
## Running the test suite under Windows

<span class="sect2"> [Building the software needed by the tests](build_win_test.md#idp368040) </span>

<span class="sect2"> [Running the test suite under Windows](build_win_test.md#idp379184) </span>

<span class="sect2"> [Building the software needed by the SQL tests](build_win_test.md#build_win_test_sql) </span>

To build the test suite on Windows platforms, you will need to configure Tcl support. You will also need sufficient main memory (at least 64MB), and disk (around 250MB of disk will be sufficient).

### Building the software needed by the tests

The test suite must be run against a Debug version of Berkeley DB, so you will need a Debug version of the Tcl libraries. This involves building Tcl from its source. See the Tcl sources for more information. Then build the Tcl API - see <a href="build_win_tcl.md" class="xref" title="Building the Tcl API">Building the Tcl API</a> for details.

#### Visual Studio 2005 or newer

To build for testing, perform the following steps:

1.  Open the Berkeley DB solution.
2.  Ensure that the target configuration is Debug
3.  Right click the <span class="emphasis">*db_tcl*</span> project in the Solution Explorer, and select <span class="emphasis">*Build*</span>.
4.  Right click the <span class="emphasis">*db_test*</span> project in the Solution Explorer, and select <span class="emphasis">*Build*</span>.

### Running the test suite under Windows

Before running the tests for the first time, you must edit the file `include.tcl` in your build directory and change the line that reads:

``` c
set tclsh_path SET_YOUR_TCLSH_PATH
```

You will want to use the location of the `tclsh` program (be sure to include the name of the executable). For example, if Tcl is installed in `d:\tcl`, this line should be the following:

``` c
set tclsh_path d:\tcl\bin\tclsh85g.exe
```

If your path includes spaces be sure to enclose it in quotes:

``` c
set tclsh_path "c:\Program Files\tcl\bin\tclsh85g.exe"
```

Make sure that the path to Berkeley DB's tcl library is in your current path. On Windows NT/2000/XP, edit your PATH using the My Computer -\> Properties -\> Advanced -\> Environment Variables dialog. On earlier versions of Windows, you may find it convenient to add a line to c:\AUTOEXEC.BAT:

``` c
SET PATH=%PATH%;c:\db\build_windows
```

Then, in a shell of your choice enter the following commands:

1.  cd build_windows

2.  run `d:\tcl\bin\tclsh85g.exe`, or the equivalent name of the Tcl shell for your system.

    You should get a "%" prompt.

3.  % source ../test/tcl/test.tcl

    If no errors occur, you should get a "%" prompt.

You are now ready to run tests in the test suite; see Running the test suite for more information.

### Building the software needed by the SQL tests

The SQL test suite must be run against a Debug version of Berkeley DB, so you need a Debug version of the Tcl libraries. This involves building Tcl from its source. See the Tcl sources for more information. Then build the Tcl API - see <a href="build_win_tcl.md" class="xref" title="Building the Tcl API">Building the Tcl API</a> for details.

Before building for SQL tests, build the db_tcl and db_sql_testfixture projects. This requires Tcl 8.5 or above. If you are using a later version of Tcl, edit the Tcl library that db_tcl and db_sql_testfixture link to.

To do this right click the <span class="emphasis">*db_tcl*</span>`/`<span class="emphasis">*db_sql_testfixture*</span> project, select <span class="emphasis">*Properties-\>Configuration Properties-\>Linker-\>Input-\>Additional Dependencies*</span> and edit the Tcl library, <span class="emphasis">*tcl85g.lib*</span>, to match the version you are using.

Building the db_sql_testfixture project builds the testfixture.exe program in `../build_windows/Win32/Debug`. It also builds the projects db and db_sql, on which it depends.

#### Visual Studio 2005 or newer

To build for testing, perform the following steps:

1.  Open the Berkeley DB solution.
2.  Ensure that the target configuration is Debug.
3.  Right click the <span class="emphasis">*db_tcl*</span> project in the Solution Explorer, and select <span class="emphasis">*Build*</span>.
4.  Right click the <span class="emphasis">*db_sql_testfixture*</span> project in the Solution Explorer, and select <span class="emphasis">*Build*</span>.

To test extensions, specify the following in the <span class="emphasis">*Preprocessor Definitions*</span> of the <span class="emphasis">*db_sql_testfixture*</span> project:

- `SQLITE_ENABLE_FTS3` to enable the full text search layer
- `SQLITE_ENABLE_RTREE` to enable the R-Tree layer
