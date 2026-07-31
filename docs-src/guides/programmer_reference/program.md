---
title: "Chapter 15.  Programmer Notes"
api-name: "Chapter 15.  Programmer Notes"
source: docs/programmer_reference/program.html
---
## Chapter 15.  Programmer Notes

**Table of Contents**

<span class="sect1"> [Signal handling](program.md#program_appsignals) </span>

<span class="sect1"> [Error returns to applications](program_errorret.md) </span>

<span class="sect1"> [Environment variables](program_environ.md) </span>

<span class="sect1"> [Multithreaded applications](program_mt.md) </span>

<span class="sect1"> [Berkeley DB handles](program_scope.md) </span>

<span class="sect1"> [Name spaces](program_namespace.md) </span>

<span class="sect2"> [C Language Name Space](program_namespace.md#idp52962960) </span>

<span class="sect2"> [Filesystem Name Space](program_namespace.md#idp53001824) </span>

<span class="sect1"> [Memory-only or Flash configurations](program_ram.md) </span>

<span class="sect1"> [Disk drive caches](program_cache.md) </span>

<span class="sect1"> [Copying or moving databases](program_copy.md) </span>

<span class="sect1"> [Compatibility with historic UNIX interfaces](program_compatible.md) </span>

<span class="sect1"> [Run-time configuration](program_runtime.md) </span>

<span class="sect1"> [Performance Event Monitoring](program_perfmon.md) </span>

<span class="sect2"> [Using the DTrace Provider](program_perfmon.md#program_perfmon_dtrace) </span>

<span class="sect2"> [Using SystemTap](program_perfmon.md#program_perfmon_stap) </span>

<span class="sect2"> [Example Scripts](program_perfmon.md#program_perfmon_examples) </span>

<span class="sect2"> [Performance Events Reference](program_perfmon.md#program_perfmon_probes) </span>

<span class="sect1"> [Programmer notes FAQ](program_faq.md) </span>

## Signal handling

When applications using Berkeley DB receive signals, it is important that they exit gracefully, discarding any Berkeley DB locks that they may hold. This is normally done by setting a flag when a signal arrives and then checking for that flag periodically within the application. Because Berkeley DB is not re-entrant, the signal handler should not attempt to release locks and/or close the database handles itself. Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

If an application exits holding a lock, the situation is no different than if the application crashed, and all applications participating in the database environment must be shut down, and then recovery must be performed. If this is not done, databases may be left in an inconsistent state, or locks the application held may cause unresolvable deadlocks inside the environment, causing applications to hang.

Berkeley DB restarts all system calls interrupted by signals, that is, any underlying system calls that return failure with errno set to EINTR will be restarted rather than failing.
