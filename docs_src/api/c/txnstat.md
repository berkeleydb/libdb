---
title: "DB_ENV->txn_stat()"
api-name: "DB_ENV->txn_stat()"
source: docs/api_reference/C/txnstat.html
---
## DB_ENV-\>txn_stat()

``` c
#include <db.h>

int
DB_ENV->txn_stat(DB_ENV *env, DB_TXN_STAT **statp, u_int32_t flags);  
```

The `DB_ENV->txn_stat()` method returns the transaction subsystem statistics.

The `DB_ENV->txn_stat()` method creates a statistical structure of type `DB_TXN_STAT` and copies a pointer to it into a user-specified memory location.

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

The following DB_TXN_STAT fields will be filled in:

- **u_int32_t st_inittxns;**

  The initial number of transactions configured.

- **DB_LSN st_last_ckp;**

  The LSN of the last checkpoint.

- **u_int32_t st_last_txnid;**

  The last transaction ID allocated.

- **u_int32_t st_maxnactive;**

  The maximum number of active transactions at any one time.

- **u_int32_t st_maxnsnapshot;**

  The maximum number of transactions on the snapshot list at any one time.

- **u_int32_t st_maxtxns;**

  The maximum number of active transactions configured.

- **uintmax_t st_naborts;**

  The number of transactions that have aborted.

- **u_int32_t st_nactive;**

  The number of transactions that are currently active.

- **uintmax_t st_nbegins;**

  The number of transactions that have begun.

- **uintmax_t st_ncommits;**

  The number of transactions that have committed.

- **u_int32_t st_nrestores;**

  The number of transactions that have been restored.

- **u_int32_t st_nsnapshot;**

  The number of transactions on the snapshot list. These are transactions which modified a database opened with <a href="dbopen.md#dbopen_DB_MULTIVERSION" class="link">DB_MULTIVERSION</a>, and which have committed or aborted, but the copies of pages they created are still in the cache.

- **uintmax_t st_region_nowait;**

  The number of times that a thread of control was able to obtain the transaction region mutex without waiting.

- **uintmax_t st_region_wait;**

  The number of times that a thread of control was forced to wait before obtaining the transaction region mutex.

- **roff_t st_regsize;**

  The region size, in bytes.

- **time_t st_time_ckp;**

  The time the last completed checkpoint finished (as the number of seconds since the Epoch, returned by the IEEE/ANSI Std 1003.1 (POSIX) **time** function).

- **DB_TXN_ACTIVE \*st_txnarray;**

  A pointer to an array of **st_nactive** DB_TXN_ACTIVE structures, describing the currently active transactions. The following fields of the DB_TXN_ACTIVE structure will be filled in:

  - **u_int32_t txnid;**

    The transaction ID of the transaction.

  - **u_int32_t parentid;**

    The transaction ID of the parent transaction (or 0, if no parent).

  - **pid_t pid;**

    The process ID of the originator of the transaction.

  - **db_threadid_t tid;**

    The thread of control ID of the originator of the transaction.

  - **DB_LSN lsn;**

    The current log sequence number when the transaction was begun.

  - **DB_LSN read_lsn;**

    The log sequence number of reads for snapshot transactions.

  - **u_int32_t mvcc_ref;**

    The number of buffer copies created by this transaction that remain in cache.

  - **u_int32_t priority;**

    This transaction's deadlock resolution priority.

  - **u_int32_t status;**

    Provides one of the following constants, indicating the transaction status:

    |                |
    |----------------|
    | TXN_ABORTED    |
    | TXN_COMMITTED  |
    | TXN_NEED_ABORT |
    | TXN_PREPARED   |
    | TXN_RUNNING    |

  - **u_int32_t xa_status;**

    Provides one of the following constants, which indicate the XA status:

    |                   |
    |-------------------|
    | TXN_XA_ACTIVE     |
    | TXN_XA_DEADLOCKED |
    | TXN_XA_IDLE       |
    | TXN_XA_PREPARED   |
    | TXN_XA_ROLLEDBACK |

  - **u_int8_t gid\[DB_GID_SIZE\];**

    If the transaction was prepared using <a href="txnprepare.md" class="xref" title="DB_TXN-&gt;prepare()">DB_TXN-&gt;prepare()</a>, then **gid** contains the transaction's Global ID. Otherwise, **gid's** contents are undefined.

  - **char name\[\];**

    If a name was specified for the transaction, up to the first 50 bytes of that name, followed by a nul termination byte.

The `DB_ENV->txn_stat()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->txn_stat()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### statp

The **statp** parameter references memory into which a pointer to the allocated statistics structure is copied.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_STAT_CLEAR`

  Reset statistics after returning their values.

### Errors

The `DB_ENV->txn_stat()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
