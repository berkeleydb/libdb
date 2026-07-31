---
title: "Chapter 14.  Application Specific Logging and Recovery"
api-name: "Chapter 14.  Application Specific Logging and Recovery"
source: docs/programmer_reference/apprec.html
---
## Chapter 14.  Application Specific Logging and Recovery

**Table of Contents**

<span class="sect1"> [Introduction to application specific logging and recovery](apprec.md#apprec_intro) </span>

<span class="sect1"> [Defining application-specific log records](apprec_def.md) </span>

<span class="sect1"> [Automatically generated functions](apprec_auto.md) </span>

<span class="sect1"> [Application configuration](apprec_config.md) </span>

## Introduction to application specific logging and recovery

It is possible to use the Locking, Logging and Transaction subsystems of Berkeley DB to provide transaction semantics on objects other than those described by the Berkeley DB access methods. In these cases, the application will need application-specific logging and recovery functions.

For example, consider an application that provides transaction semantics on data stored in plain text files accessed using the POSIX read and write system calls. The read and write operations for which transaction protection is desired will be bracketed by calls to the standard Berkeley DB transactional interfaces, <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a> and <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a>, and the transaction's locker ID will be used to acquire relevant read and write locks.

Before data is accessed, the application must make a call to the lock manager, <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a>, for a lock of the appropriate type (for example, read) on the object being locked. The object might be a page in the file, a byte, a range of bytes, or some key. It is up to the application to ensure that appropriate locks are acquired. Before a write is performed, the application should acquire a write lock on the object by making an appropriate call to the lock manager, <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a>. Then, the application should make a call to the log manager, via the automatically-generated log-writing function described as follows. This record should contain enough information to redo the operation in case of failure after commit and to undo the operation in case of abort.

When designing applications that will use the log subsystem, it is important to remember that the application is responsible for providing any necessary structure to the log record. For example, the application must understand what part of the log record is an operation code, what part identifies the file being modified, what part is redo information, and what part is undo information.

After the log message is written, the application may issue the write system call. After all requests are issued, the application may call <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a>. When <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a> returns, the caller is guaranteed that all necessary log writes have been written to disk.

At any time before issuing a <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a>, the application may call <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a>, which will result in restoration of the database to a consistent pretransaction state. (The application may specify its own recovery function for this purpose using the <a href="../../api/c/envset_app_dispatch.md" class="olink">DB_ENV-&gt;set_app_dispatch()</a> method. The recovery function must be able to either reapply or undo the update depending on the context, for each different type of log record. The recovery functions must not use Berkeley DB methods to access data in the environment as there is no way to coordinate these accesses with either the aborting transaction or the updates done by recovery or replication.)

If the application crashes, the recovery process uses the log to restore the database to a consistent state.

Berkeley DB includes tools to assist in the development of application-specific logging and recovery. Specifically, given a description of information to be logged in a family of log records, these tools will automatically create log-writing functions (functions that marshall their arguments into a single log record), log-reading functions (functions that read a log record and unmarshall it into a structure containing fields that map into the arguments written to the log), log-printing functions (functions that print the contents of a log record for debugging), and templates for recovery functions (functions that review log records during transaction abort or recovery). The tools and generated code are C-language and POSIX-system based, but the generated code should be usable on any system, not just POSIX systems.

A sample application that does application-specific recovery is included in the Berkeley DB distribution, in the directory `examples_c/ex_apprec`.
