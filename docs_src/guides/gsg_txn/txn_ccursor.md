---
title: "Transactional Cursors and Concurrent Applications"
api-name: "Transactional Cursors and Concurrent Applications"
source: docs/gsg_txn/C/txn_ccursor.html
---
## Transactional Cursors and Concurrent Applications

<span class="sect2"> [Using Cursors with Uncommitted Data](txn_ccursor.md#cursordirtyreads) </span>

When you use transactional cursors with a concurrent application, remember that in the event of a deadlock you must make sure that you close your cursor before you abort and retry your transaction.

Also, remember that when you are using the default isolation level, every time your cursor reads a record it locks that record until the encompassing transaction is resolved. This means that walking your database with a transactional cursor increases the chance of lock contention.

For this reason, if you must routinely walk your database with a transactional cursor, consider using a reduced isolation level such as read committed.

### Using Cursors with Uncommitted Data

As described in <a href="isolation.md#dirtyreads" class="xref" title="Reading Uncommitted Data">Reading Uncommitted Data</a> above, it is possible to relax your transaction's isolation level such that it can read data modified but not yet committed by another transaction. You can configure this when you create your transaction handle, and when you do so then all cursors opened inside that transaction will automatically use uncommitted reads.

You can also do this when you create a cursor handle from within a serializable transaction. When you do this, only those cursors configured for uncommitted reads uses uncommitted reads.

Either way, you must first configure your database handle to support uncommitted reads before you can configure your transactions or your cursors to use them.

The following example shows how to configure an individual cursor handle to read uncommitted data from within a serializable (full isolation) transaction. For an example of configuring a transaction to perform uncommitted reads in general, see <a href="isolation.md#dirtyreads" class="xref" title="Reading Uncommitted Data">Reading Uncommitted Data</a>.

``` c
#include <stdio.h>
#include <stdlib.h>

#include "db.h"

int
main(void)
{
    DB *dbp;
    DBC *cursorp;
    DB_ENV *envp;
    DB_TXN *txn;
    int ret, c_ret;
    char *replacementString = "new string";

    dbp = NULL;
    envp = NULL;
    txn = NULL;

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

    db_flags = DB_CREATE |             /* Create the db if it does not 
                                        * exist */
               DB_AUTO_COMMIT |        /* Enable auto commit */
               DB_READ_UNCOMMITTED;    /* Enable uncommitted reads */

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

    /* Get the txn handle */
    /* Note that this is a degree 3 transaction */
    ret = envp->txn_begin(envp, NULL, &txn, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Transaction begin failed.");
        goto err;
    }

    /* Get the cursor, supply the txn handle at that time */
    /* Cause the cursor to perform uncommitted reads */
    ret = dbp->cursor(dbp, txn, &cursorp, DB_READ_UNCOMMITTED);
    if (ret != 0) {
        envp->err(envp, ret, "Cursor open failed.");
        txn->abort(txn);
        goto err;
    }

    /*
     * From here, you perform your cursor reads and writes as normal,
     * committing and aborting the transactions as is necessary, and 
     * testing for deadlock exceptions as normal (omitted for brevity). 
     */

     ...  
```
