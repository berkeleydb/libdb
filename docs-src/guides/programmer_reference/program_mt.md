---
title: "Multithreaded applications"
api-name: "Multithreaded applications"
source: docs/programmer_reference/program_mt.html
---
## Multithreaded applications

Berkeley DB fully supports multithreaded applications. The Berkeley DB library is not itself multithreaded, and was deliberately architected to not use threads internally because of the portability problems that would introduce. Database environment and database object handles returned from Berkeley DB library functions are free-threaded. No other object handles returned from the Berkeley DB library are free-threaded. The following rules should be observed when using threads to access the Berkeley DB library:

1.  The <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> flag must be specified to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> and <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> methods if the Berkeley DB handles returned by those interfaces will be used in the context of more than one thread. Setting the <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> flag inconsistently may result in database corruption.

    Threading is assumed in the Java API, so no special flags are required; and Berkeley DB functions will always behave as if the <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> flag was specified.

    Only a single thread may call the <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a> or <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> methods for a returned environment or database handle.

    No other Berkeley DB handles are free-threaded.

2.  When using the non-cursor Berkeley DB calls to retrieve key/data items (for example, <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a>), the memory to which the pointer stored into the Dbt refers is valid only until the next call using the <a href="../../api/c/db.md" class="olink">DB</a> handle returned by <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a>. This includes **any** use of the returned <a href="../../api/c/db.md" class="olink">DB</a> handle, including by another thread within the process.

    For this reason, if the <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> handle was specified to the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> method, either <a href="../../api/c/dbt.md#dbt_DB_DBT_MALLOC" class="olink">DB_DBT_MALLOC</a>, <a href="../../api/c/dbt.md#dbt_DB_DBT_REALLOC" class="olink">DB_DBT_REALLOC</a> or <a href="../../api/c/dbt.md#dbt_DB_DBT_USERMEM" class="olink">DB_DBT_USERMEM</a> must be specified in the <a href="../../api/c/dbt.md" class="olink">DBT</a> when performing any non-cursor key or data retrieval.

3.  Cursors may not span transactions. Each cursor must be allocated and deallocated within the same transaction.

    Transactions and cursors may span threads, but only serially, that is, the application must serialize access to the <a href="../../api/c/txn.md" class="olink">TXN</a> and <a href="../../api/c/dbc.md" class="olink">DBC</a> handles. In the case of nested transactions, since all child transactions are part of the same parent transaction, they must observe the same constraints. That is, children may execute in different threads only if each child executes serially.

4.  User-level synchronization mutexes must have been implemented for the compiler/architecture combination. Attempting to specify the DB_THREAD flag will fail if fast mutexes are not available.

    If blocking mutexes are available (for example POSIX pthreads), they will be used. Otherwise, the Berkeley DB library will make a system call to pause for some amount of time when it is necessary to wait on a lock. This may not be optimal, especially in a thread-only environment, in which it is usually more efficient to explicitly yield the processor to another thread.

    It is possible to specify a yield function on an per-application basis. See <a href="../../api/c/db_env_set_func_yield.md" class="olink">db_env_set_func_yield</a> for more information.

    It is possible to specify the number of attempts that will be made to acquire the mutex before waiting. See <a href="../../api/c/mutexset_tas_spins.md" class="olink">DB_ENV-&gt;mutex_set_tas_spins()</a> for more information.

When creating multiple databases in a single physical file, multithreaded programs may have additional requirements. For more information, see <a href="am_opensub.md" class="xref" title="Opening multiple databases in a single file">Opening multiple databases in a single file</a>
