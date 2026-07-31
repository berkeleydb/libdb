---
title: "Running the Tests"
api-name: "Running the Tests"
source: docs/porting/testrun.html
---
## Running the Tests

You test your new port of Berkeley DB by running the tests in the following order:

1.  Run the C tests in the following order:

    1.  Tests for mutexes located in the `test_mutex` directory. To run the tests, follow the instructions in the `test_mutex/readme` file.

    2.  Tests for the common code paths located in the `test_micro` directory. To run the tests in a shell script, follow the instructions in the `test_micro/readme` file. To run the tests as simple C tests, follow the instructions in the `test_micro/readme_embedded` file.

2.  If the target platform supports the use of Tcl (version 8.5 or later), run the Test Suite. How you run the Test Suite varies depending on the target platform:

    - If the target platform supports a UNIX-like version of Tcl, then set up Tcl and build the Test Suite as described in "Running the Test Suite under UNIX" in *Berkeley DB Installation and Build Guide* at <a href="http://download.oracle.com/docs/cd/E17076_02/html/installation/build_unix_test.html" class="ulink" target="_top">http://download.oracle.com/docs/cd/E17076_02/html/installation/build_unix_test.html</a> and, then, run the test suite.

    - If the target platform supports a Windows-like version of Tcl, then setup Tcl, and build and run the Test Suite as described in "Running the Test Suite under Windows" in *Berkeley DB Programmer's Reference Guide* at <a href="http://download.oracle.com/docs/cd/E17076_02/html/installation/build_win_test.html" class="ulink" target="_top">http://download.oracle.com/docs/cd/E17076_02/html/installation/build_win_test.html</a>
