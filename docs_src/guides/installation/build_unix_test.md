---
title: "Running the test suite under UNIX"
api-name: "Running the test suite under UNIX"
source: docs/installation/build_unix_test.html
---
## Running the test suite under UNIX

<span class="sect2"> [Building SQL Test Suite on Unix](build_unix_test.md#build_unix_test_sql) </span>

The Berkeley DB test suite is built if you specify <a href="build_unix_conf.md#build_unix_conf.--enable-test" class="link">--enable-test</a> as an argument when configuring Berkeley DB. The test suite also requires that you configure and build the Tcl interface to the library.

Before running the tests for the first time, you may need to edit the `include.tcl` file in your build directory. The Berkeley DB configuration assumes that you intend to use the version of the tclsh utility included in the Tcl installation with which Berkeley DB was configured to run the test suite, and further assumes that the test suite will be run with the libraries prebuilt in the Berkeley DB build directory. If either of these assumptions are incorrect, you will need to edit the `include.tcl` file and change the following line to correctly specify the full path to the version of tclsh with which you are going to run the test suite:

``` c
set tclsh_path ...
```

You may also need to change the following line to correctly specify the path from the directory where you are running the test suite to the location of the Berkeley DB Tcl library you built:

``` c
set test_path ...
```

It may not be necessary that this be a full path if you have configured your system's shared library mechanisms to search the directory where you built or installed the Tcl library.

All Berkeley DB tests are run from within **tclsh**. After starting tclsh, you must source the file `test.tcl` in the test directory. For example, if you built in the `build_unix` directory of the distribution, this would be done using the following command:

``` c
% source ../test/tcl/test.tcl
```

If no errors occur, you should get a "%" prompt.

You are now ready to run tests in the test suite; see Running the test suite for more information.

### Building SQL Test Suite on Unix

The Berkeley DB SQL interface test suite is built if you specify <a href="build_unix_conf.md#build_unix_conf.--enable-test" class="link">--enable-test</a> and <a href="build_unix_conf.md#build_unix_conf.--enable-sql" class="link">--enable-sql</a> as arguments, when configuring Berkeley DB. The test suite also requires that you build the Berkeley DB Tcl API.

``` c
../dist/configure --enable-sql --enable-test --with-tcl=/usr/lib
```

This builds the <span class="emphasis">*testfixture*</span> project in `../build_unix/sql`.

To enable extensions like full text search layer and R-Tree layer in the SQL test suite, configure with --enable-amalgamation.
