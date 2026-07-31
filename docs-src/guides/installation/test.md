---
title: "Chapter 14.  Test Suite"
api-name: "Chapter 14.  Test Suite"
source: docs/installation/test.html
---
## Chapter 14.  Test Suite

**Table of Contents**

<span class="sect1"> [Running the test suite](test.md#test_run) </span>

<span class="sect2"> [Running SQL Test Suite on Unix](test.md#idp1298736) </span>

<span class="sect2"> [Running SQL Test Suite on Windows](test.md#idp1289848) </span>

<span class="sect1"> [Test suite FAQ](test_faq.md) </span>

## Running the test suite

<span class="sect2"> [Running SQL Test Suite on Unix](test.md#idp1298736) </span>

<span class="sect2"> [Running SQL Test Suite on Windows](test.md#idp1289848) </span>

Once you have started tclsh and have loaded the test.tcl source file (see <a href="build_unix_test.md" class="xref" title="Running the test suite under UNIX">Running the test suite under UNIX</a> and <a href="build_win_test.md" class="xref" title="Running the test suite under Windows">Running the test suite under Windows</a> for more information), you are ready to run the test suite. At the tclsh prompt, to run the standard test suite, enter the following:

``` c
% run_std
```

A more exhaustive version of the test suite runs all the tests several more times, testing encryption, replication, and different page sizes. After you have a clean run for run_std, you may choose to run this lengthier set of tests. At the tclsh prompt, enter:

``` c
% run_all
```

Running the standard tests can take from several hours to a few days to complete, depending on your hardware, and running all the tests will take at least twice as long. For this reason, the output from these commands are redirected to a file in the current directory named `ALL.OUT`. Periodically, a line will be written to the standard output, indicating what test is being run. When the test suite has finished, a final message will be written indicating the test suite has completed successfully or that it has failed. If the run failed, you should review the `ALL.OUT` file to determine which tests failed. Errors will appear in that file as output lines, beginning with the string "FAIL".

Tests are run in the directory `TESTDIR`, by default. However, the test files are often large, and you should use a filesystem with at least several hundred megabytes of free space. To use a different directory for the test directory, edit the file include.tcl in your build directory, and change the following line to a more appropriate value for your system:

``` c
set testdir ./TESTDIR
```

For example, you might change it to the following:

``` c
set testdir /var/tmp/db.test
```

Alternatively, you can create a symbolic link named TESTDIR in your build directory to an appropriate location for running the tests. Regardless of where you run the tests, the TESTDIR directory should be on a local filesystem. Using a remote filesystem (for example, an NFS mounted filesystem) will almost certainly cause spurious test failures.

### Running SQL Test Suite on Unix

Once the test suite is built (see <a href="build_unix_test.md#build_unix_test_sql" class="xref" title="Building SQL Test Suite on Unix">Building SQL Test Suite on Unix</a> for more information), run the entire test suite by executing the following command in the `../build_unix/sql` directory:

``` c
sh ../../test/sql/bdb-test.sh
```

This runs a set of tests and lists the errors each test encountered, if any. A detailed list of the test results is written to `test.log`.

To run an individual test, such as insert.test, execute the following command in the `../build_unix/sql` directory:

``` c
./testfixture ../../lang/sql/sqlite/test/insert.test 
```

### Running SQL Test Suite on Windows

After the test suite is built (see <a href="build_win_test.md#build_win_test_sql" class="xref" title="Building the software needed by the SQL tests">Building the software needed by the SQL tests</a> for more information) and before running the entire test suite, go to `../sql/adapter/bdb-test.sh` and edit the line:

``` c
echo $t: `alarm $TIMEOUT ./testfixture.exe 
$tpath 2>&1 | tee -a test.log | grep "errors out of" 
|| echo "failed"`
```

to

``` c
echo $t: `alarm $TIMEOUT Win32/Debug/testfixture.exe 
$tpath 2>&1 | tee -a test.log | grep "errors out of" 
|| echo "failed"`
```

Running the test suite requires an Unix emulator, such as Cygwin. In a Cygwin window go to the `../build_windows` directory and execute the command:

``` c
sh ../sql/adapter/bdb-test.sh
```

This runs a set of tests and lists errors that each test encountered, if any. A detailed list of the test results is written to `test.log`.

To run an individual test, such as insert.test, execute the following command in the `../build_windows` directory:

``` c
Win32/Debug/testfixture.exe ../sql/sqlite/test/insert.test 
```
