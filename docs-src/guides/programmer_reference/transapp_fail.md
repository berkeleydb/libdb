---
title: "Handling failure in Transactional Data Store applications"
api-name: "Handling failure in Transactional Data Store applications"
source: docs/programmer_reference/transapp_fail.html
---
## Handling failure in Transactional Data Store applications

When building Transactional Data Store applications, there are design issues to consider whenever a thread of control with open Berkeley DB handles fails for any reason (where a thread of control may be either a true thread or a process).

The first case is handling system failure: if the system fails, the database environment and the databases may be left in a corrupted state. In this case, recovery must be performed on the database environment before any further action is taken, in order to:

- recover the database environment resources,
- release any locks or mutexes that may have been held to avoid starvation as the remaining threads of control convoy behind the held locks, and
- resolve any partially completed operations that may have left a database in an inconsistent or corrupted state.

For details on performing recovery, see the <a href="transapp_recovery.md" class="xref" title="Recovery procedures">Recovery procedures</a>.

The second case is handling the failure of a thread of control. There are resources maintained in database environments that may be left locked or corrupted if a thread of control exits unexpectedly. These resources include data structure mutexes, logical database locks and unresolved transactions (that is, transactions which were never aborted or committed). While Transactional Data Store applications can treat the failure of a thread of control in the same way as they do a system failure, they have an alternative choice, the <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> method.

The <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> will return <a href="program_errorret.md#program_errorret.DB_RUNRECOVERY" class="link">DB_RUNRECOVERY</a> if the database environment is unusable as a result of the thread of control failure. (If a data structure mutex or a database write lock is left held by thread of control failure, the application should not continue to use the database environment, as subsequent use of the environment is likely to result in threads of control convoying behind the held locks.) The <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> call will release any database read locks that have been left held by the exit of a thread of control, and abort any unresolved transactions. In this case, the application can continue to use the database environment.

A Transactional Data Store application recovering from a thread of control failure should call <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a>, and, if it returns success, the application can continue. If <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> returns <a href="program_errorret.md#program_errorret.DB_RUNRECOVERY" class="link">DB_RUNRECOVERY</a>, the application should proceed as described for the case of system failure.

It greatly simplifies matters that recovery may be performed regardless of whether recovery needs to be performed; that is, it is not an error to recover a database environment for which recovery is not strictly necessary. For this reason, applications should not try to determine if the database environment was active when the application or system failed. Instead, applications should run recovery any time the <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> method returns <a href="program_errorret.md#program_errorret.DB_RUNRECOVERY" class="link">DB_RUNRECOVERY</a>, or, if the application is not calling the <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> method, any time any thread of control accessing the database environment fails, as well as any time the system reboots.
