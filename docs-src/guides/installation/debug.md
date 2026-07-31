---
title: "Chapter 3.  Debugging Applications"
api-name: "Chapter 3.  Debugging Applications"
source: docs/installation/debug.html
---
## Chapter 3.  Debugging Applications

**Table of Contents**

<span class="sect1"> [Introduction to debugging](debug.md#debug_intro) </span>

<span class="sect1"> [Compile-time configuration](debug_compile.md) </span>

<span class="sect1"> [Run-time error information](debug_runtime.md) </span>

<span class="sect1"> [Reviewing Berkeley DB log files](debug_printlog.md) </span>

<span class="sect2"> [Augmenting the Log for Debugging](debug_printlog.md#idp121880) </span>

<span class="sect2"> [Extracting Committed Transactions and Transaction Status](debug_printlog.md#idp53840) </span>

<span class="sect2"> [Extracting Transaction Histories](debug_printlog.md#idp41744) </span>

<span class="sect2"> [Extracting File Histories](debug_printlog.md#idp154152) </span>

<span class="sect2"> [Extracting Page Histories](debug_printlog.md#idp158032) </span>

<span class="sect2"> [Other log processing tools](debug_printlog.md#idp124648) </span>

## Introduction to debugging

Because Berkeley DB is an embedded library, debugging applications that use Berkeley DB is both harder and easier than debugging a separate server. Debugging can be harder because when a problem arises, it is not always readily apparent whether the problem is in the application, is in the database library, or is a result of an unexpected interaction between the two. Debugging can be easier because it is easier to track down a problem when you can review a stack trace rather than deciphering interprocess communication messages. This chapter is intended to assist you with debugging applications and reporting bugs to us so that we can provide you with the correct answer or fix as quickly as possible.

When you encounter a problem, there are a few general actions you can take:

<span class="term">Review the Berkeley DB error output:</span>  
If an error output mechanism has been configured in the Berkeley DB environment, additional run-time error messages are made available to the applications. If you are not using an environment, it is well worth modifying your application to create one so that you can get more detailed error messages. See <a href="debug_runtime.md" class="xref" title="Run-time error information">Run-time error information</a> for more information on configuring Berkeley DB to output these error messages.

<span class="term">Review the options available for the <a href="../../api/c/envset_verbose.md" class="olink">DB_ENV-&gt;set_verbose()</a> method:</span>  
Look to see if it offers any additional informational and/or debugging messages that might help you understand the problem.

<span class="term">Add run-time diagnostics:</span>  
You can configure and build Berkeley DB to perform run-time diagnostics. (By default, these checks are not done because they can seriously impact performance.) See <a href="debug_compile.md" class="xref" title="Compile-time configuration">Compile-time configuration</a> for more information.

<span class="term">Apply all available patches:</span>  
Before reporting a problem in Berkeley DB, please upgrade to the latest Berkeley DB release, if possible, or at least make sure you have applied any updates available for your release from the <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html" class="ulink" target="_top">Berkeley DB web site</a> .

<span class="term">Run the test suite:</span>  
If you see repeated failures or failures of simple test cases, run the Berkeley DB test suite to determine whether the distribution of Berkeley DB you are using was built and configured correctly.
