---
title: "Chapter 3. Transaction Basics"
api-name: "Chapter 3. Transaction Basics"
source: docs/gsg_txn/C/usingtxns.html
---
## Chapter 3. Transaction Basics

**Table of Contents**

<span class="sect1"> [Committing a Transaction](usingtxns.md#commitresults) </span>

<span class="sect1"> [Non-Durable Transactions](nodurabletxn.md) </span>

<span class="sect1"> [Aborting a Transaction](abortresults.md) </span>

<span class="sect1"> [Auto Commit](autocommit.md) </span>

<span class="sect1"> [Nested Transactions](nestedtxn.md) </span>

<span class="sect1"> [Transactional Cursors](txncursor.md) </span>

<span class="sect1"> [Secondary Indices with Transaction Applications](txnindices.md) </span>

<span class="sect1"> [Configuring the Transaction Subsystem](maxtxns.md) </span>

Once you have enabled transactions for your environment and your databases, you can use them to protect your database operations. You do this by acquiring a transaction handle and then using that handle for any database operation that you want to participate in that transaction.

You obtain a transaction handle using the `DB_ENV->txn_begin()` method.

Once you have completed all of the operations that you want to include in the transaction, you must commit the transaction using the `DB_TXN->commit()` method.

If, for any reason, you want to abandon the transaction, you abort it using `DB_TXN->abort()`.

Any transaction handle that has been committed or aborted can no longer be used by your application.

Finally, you must make sure that all transaction handles are either committed or aborted before closing your databases and environment.

### Note

If you only want to transaction protect a single database write operation, you can use auto commit to perform the transaction administration. When you use auto commit, you do not need an explicit transaction handle. See <a href="autocommit.md" class="xref" title="Auto Commit">Auto Commit</a> for more information.

For example, the following example opens a transactional-enabled environment and database, obtains a transaction handle, and then performs a write operation under its protection. In the event of any failure in the write operation, the transaction is aborted and the database is left in a state as if no operations had ever been attempted in the first place.

``` c
#include <stdio.h>
#include <stdlib.h>

#include "db.h"

int
main(void)
{
    int ret, ret_c;
    u_int32_t db_flags, env_flags;
    DB *dbp;
    DB_ENV *envp;
    DBT key, data;
    DB_TXN *txn;
    const char *db_home_dir = "/tmp/myEnvironment";
    const char *file_name = "mydb.db";
    const char *keystr ="thekey";
    const char *datastr = "thedata";
    
    dbp = NULL;
    envp = NULL;

    /* Open the environment */
    ret = db_env_create(&envp, 0);
    if (ret != 0) {
        fprintf(stderr, "Error creating environment handle: %s\n",
            db_strerror(ret));
        return (EXIT_FAILURE);
    }

    env_flags = DB_CREATE |    /* Create the environment if it does 
                                * not already exist. */
                DB_INIT_TXN  | /* Initialize transactions */
                DB_INIT_LOCK | /* Initialize locking. */
                DB_INIT_LOG  | /* Initialize logging */
                DB_INIT_MPOOL; /* Initialize the in-memory cache. */

    ret = envp->open(envp, db_home_dir, env_flags, 0);
    if (ret != 0) {
        fprintf(stderr, "Error opening environment: %s\n",
            db_strerror(ret));
        goto err;
    }

    /* Initialize the DB handle */
    ret = db_create(&dbp, envp, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Database creation failed");
        goto err;
    }

    db_flags = DB_CREATE | DB_AUTO_COMMIT;
    /* 
     * Open the database. Note that we are using auto commit for the open, 
     * so the database is able to support transactions.
     */
    ret = dbp->open(dbp,        /* Pointer to the database */
                    NULL,       /* Txn pointer */
                    file_name,  /* File name */
                    NULL,       /* Logical db name */
                    DB_BTREE,   /* Database type (using btree) */
                    db_flags,   /* Open flags */
                    0);         /* File mode. Using defaults */
    if (ret != 0) {
        envp->err(envp, ret, "Database '%s' open failed",
            file_name);
        goto err;
    }

    /* Prepare the DBTs */
    memset(&key, 0, sizeof(DBT));
    memset(&data, 0, sizeof(DBT));

    key.data = &keystr;
    key.size = strlen(keystr) + 1;
    data.data = &datastr;
    data.size = strlen(datastr) + 1;

    /* Get the txn handle */
    txn = NULL;
    ret = envp->txn_begin(envp, NULL, &txn, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Transaction begin failed.");
        goto err;
    }

    /*
     * Perform the database write. If this fails, abort the transaction.
     */
    ret = dbp->put(dbp, txn, &key, &data, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Database put failed.");
        txn->abort(txn);
        goto err;
    }

    /* 
     * Commit the transaction. Note that the transaction handle
     * can no longer be used.
     */ 
    ret = txn->commit(txn, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Transaction commit failed.");
        goto err;
    }

err:
    /* Close the database */
    if (dbp != NULL) {
        ret_c = dbp->close(dbp, 0);
        if (ret_c != 0) {
            envp->err(envp, ret_c, "Database close failed.");
            ret = ret_c
        }
    }

    /* Close the environment */
    if (envp != NULL) {
        ret_c = envp->close(envp, 0);
        if (ret_c != 0) {
            fprintf(stderr, "environment close failed: %s\n",
                db_strerror(ret_c));
            ret = ret_c;
        }
    }

    return (ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
} 
```

## Committing a Transaction

In order to fully understand what is happening when you commit a transaction, you must first understand a little about what DB is doing with the logging subsystem. Logging causes all database write operations to be identified in logs, and by default these logs are backed by files on disk. These logs are used to restore your databases in the event of a system or application failure, so by performing logging, DB ensures the integrity of your data.

Moreover, DB performs <span class="emphasis">*write-ahead*</span> logging. This means that information is written to the logs <span class="emphasis">*before*</span> the actual database is changed. This means that all write activity performed under the protection of the transaction is noted in the log before the transaction is committed. Be aware, however, that database maintains logs in-memory. If you are backing your logs on disk, the log information will eventually be written to the log files, but while the transaction is on-going the log data may be held only in memory.

When you commit a transaction, the following occurs:

- A commit record is written to the log. This indicates that the modifications made by the transaction are now permanent. By default, this write is performed synchronously to disk so the commit record arrives in the log files before any other actions are taken.

- Any log information held in memory is (by default) synchronously written to disk. Note that this requirement can be relaxed, depending on the type of commit you perform. See <a href="nodurabletxn.md" class="xref" title="Non-Durable Transactions">Non-Durable Transactions</a> for more information. Also, if you are maintaining your logs entirely in-memory, then this step will of course not be taken. To configure your logging system for in-memory usage, see <a href="logconfig.md#inmemorylogging" class="xref" title="Configuring In-Memory Logging">Configuring In-Memory Logging</a>.

- All locks held by the transaction are released. This means that read operations performed by other transactions or threads of control can now see the modifications without resorting to uncommitted reads (see <a href="isolation.md#dirtyreads" class="xref" title="Reading Uncommitted Data">Reading Uncommitted Data</a> for more information).

To commit a transaction, you simply call `DB_TXN->commit()`.

Notice that committing a transaction does not necessarily cause data modified in your memory cache to be written to the files backing your databases on disk. Dirtied database pages are written for a number of reasons, but a transactional commit is not one of them. The following are the things that can cause a dirtied database page to be written to the backing database file:

- Checkpoints.

  Checkpoints cause all dirtied pages currently existing in the cache to be written to disk, and a checkpoint record is then written to the logs. You can run checkpoints explicitly. For more information on checkpoints, see <a href="filemanagement.md#checkpoints" class="xref" title="Checkpoints">Checkpoints</a>.

- Cache is full.

  If the in-memory cache fills up, then dirtied pages might be written to disk in order to free up space for other pages that your application needs to use. Note that if dirtied pages are written to the database files, then any log records that describe how those pages were dirtied are written to disk before the database pages are written.

Be aware that because your transaction commit caused database modifications recorded in your logs to be forced to disk, your modifications are by default "persistent" in that they can be recovered in the event of an application or system failure. However, recovery time is gated by how much data has been modified since the last checkpoint, so for applications that perform a lot of writes, you may want to run a checkpoint with some frequency.

Note that once you have committed a transaction, the transaction handle that you used for the transaction is no longer valid. To perform database activities under the control of a new transaction, you must obtain a fresh transaction handle.
