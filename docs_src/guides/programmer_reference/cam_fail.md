---
title: "Handling failure in Data Store and Concurrent Data Store applications"
api-name: "Handling failure in Data Store and Concurrent Data Store applications"
source: docs/programmer_reference/cam_fail.html
---
## Handling failure in Data Store and Concurrent Data Store applications

When building Data Store and Concurrent Data Store applications, there are design issues to consider whenever a thread of control with open Berkeley DB handles fails for any reason (where a thread of control may be either a true thread or a process).

The simplest case is handling system failure for any Data Store or Concurrent Data Store application. In the case of system failure, it doesn't matter if the application has opened a database environment or is just using standalone databases: if the system fails, after the application has modified a database and has not subsequently flushed the database to stable storage (by calling either the <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a>, <a href="../../api/c/dbsync.md" class="olink">DB-&gt;sync()</a> or <a href="../../api/c/mempsync.md" class="olink">DB_ENV-&gt;memp_sync()</a> methods), the database may be left in a corrupted state. In this case, before accessing the database again, the database should either be:

- removed and re-created,
- removed and restored from the last known good backup, or
- verified using the <a href="../../api/c/dbverify.md" class="olink">DB-&gt;verify()</a> method or <a href="../../api/c/db_verify.md" class="olink">db_verify</a> utility. If the database does not verify cleanly, the contents may be salvaged using the **-R** and **-r** options of the <a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility.

Applications where the potential for data loss is unacceptable should consider the Berkeley DB Transactional Data Store product, which offers standard transactional durability guarantees, including recoverability after failure.

Additionally, system failure requires that any persistent database environment (that is, any database environment not created using the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag), be removed. Database environments may be removed using the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method. If the persistent database environment was backed by the filesystem (that is, the environment was not created using the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flag), the database environment may also be safely removed by deleting the environment's files with standard system utilities.

The second case is application failure for a Data Store application, with or without a database environment, or application failure for a Concurrent Data Store application without a database environment: as in the case of system failure, if any thread of control fails, after the application has modified a database and has not subsequently flushed the database to stable storage, the database may be left in a corrupted state. In this case, the database should be handled as described previously in the system failure case.

The third case is application failure for a Concurrent Data Store application with a database environment. There are resources maintained in database environments that may be left locked if a thread of control exits without first closing all open Berkeley DB handles. Concurrent Data Store applications with database environments have an additional option for handling the unexpected exit of a thread of control, the <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> method.

The <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> will return <a href="program_errorret.md#program_errorret.DB_RUNRECOVERY" class="link">DB_RUNRECOVERY</a> if the database environment is unusable as a result of the thread of control failure. (If a data structure mutex or a database write lock is left held by thread of control failure, the application should not continue to use the database environment, as subsequent use of the environment is likely to result in threads of control convoying behind the held locks.) The <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> call will release any database read locks that have been left held by the exit of a thread of control. In this case, the application can continue to use the database environment.

A Concurrent Data Store application recovering from a thread of control failure should call <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a>, and, if it returns success, the application can continue. If <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> returns <a href="program_errorret.md#program_errorret.DB_RUNRECOVERY" class="link">DB_RUNRECOVERY</a>, the application should proceed as described for the case of system failure.
