---
title: "Chapter 3. Testing and Certifying the Port"
api-name: "Chapter 3. Testing and Certifying the Port"
source: docs/porting/testport.html
---
## Chapter 3. Testing and Certifying the Port

**Table of Contents**

<span class="sect1"> [Types of Tests for Berkeley DB](testport.md#testtypes) </span>

<span class="sect1"> [Modifying the Tests](modifytest.md) </span>

<span class="sect1"> [Running the Tests](testrun.md) </span>

<span class="sect1"> [Reviewing the Results of the Tests](testreview.md) </span>

<span class="sect1"> [Integrating Changes into the Berkeley DB Source Code](sourceintegrate.md) </span>

<span class="sect1"> [Certifying a Port of Berkeley DB](certport.md) </span>

There are several different types of tests available for validating your port of Berkeley DB as discussed in <a href="testport.md#testtypes" class="xref" title="Types of Tests for Berkeley DB">Types of Tests for Berkeley DB</a>. Testing your port involves:

- <a href="modifytest.md" class="xref" title="Modifying the Tests">Modifying the Tests</a>

- <a href="testrun.md" class="xref" title="Running the Tests">Running the Tests</a>

- <a href="testreview.md" class="xref" title="Reviewing the Results of the Tests">Reviewing the Results of the Tests</a>

- <a href="sourceintegrate.md" class="xref" title="Integrating Changes into the Berkeley DB Source Code">Integrating Changes into the Berkeley DB Source Code</a>

- <a href="certport.md" class="xref" title="Certifying a Port of Berkeley DB">Certifying a Port of Berkeley DB</a>

## Types of Tests for Berkeley DB

There are two types of tests available for testing your port of Berkeley DB:

- The C Tests for Berkeley DB

  There are two types of C tests for Berkeley DB. Each of these is in its own directory:

  - `test_mutex` contains files that test the use of mutexes in Berkeley DB.

  - `test_micro` contains the C tests that exercise the most common code paths, but it is not intended to be an exhaustive Test Suite. Additionally, it tests the different versions of Berkeley DB (including the new port) against each other. The `test_micro` tests can either be run in a shell or as simple C tests.

- The Berkeley DB Test Suite

  The `test` directory contains the Berkeley DB Test Suite that tests all of the code in Berkeley DB. Using the Test Suite involves using Tool Command Language (Tcl) version 8.5 or later. Running the standard version of the Test Suite executes tests the major functionality of Berkeley DB. A more exhaustive version of the Test Suite runs all the tests several more times, testing encryption, replication, and different page sizes.

### Note

Contact the Oracle Berkelely DB engineering team for a platform compatibility test suite.
