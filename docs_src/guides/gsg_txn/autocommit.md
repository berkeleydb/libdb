---
title: "Auto Commit"
api-name: "Auto Commit"
source: docs/gsg_txn/C/autocommit.html
---
## Auto Commit

While transactions are frequently used to provide atomicity to multiple database operations, it is sometimes necessary to perform a single database operation under the control of a transaction. Rather than force you to obtain a transaction, perform the single write operation, and then either commit or abort the transaction, you can automatically group this sequence of events using <span class="emphasis">*auto commit*</span>.

To use auto commit:

1.  Open your environment and your databases so that they support transactions. See <a href="enabletxn.md" class="xref" title="Chapter 2. Enabling Transactions">Enabling Transactions</a> for details.

    Note that frequently auto commit is used for the environment or database open. To use auto commit for either your environment or database open, specify `DB_AUTO_COMMIT` to the `DB_ENV->set_flags()` or `DB->open()` method. If you specify auto commit for the environment open, then you do not need to also specify auto commit for the database open.

2.  Do not provide a transactional handle to the method that is performing the database write operation.

Note that auto commit is not available for cursors. You must always open your cursor using a transaction if you want the cursor's operations to be transactional protected. See <a href="txncursor.md" class="xref" title="Transactional Cursors">Transactional Cursors</a> for details on using transactional cursors.

### Note

Never have more than one active transaction in your thread at a time. This is especially a problem if you mix an explicit transaction with another operation that uses auto commit. Doing so can result in undetectable deadlocks.

For example, the following uses auto commit to perform the database write operation:

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

    /*
     * Perform the database write. A txn handle is not provided, but the
     * database support auto commit, so auto commit is used for the write.
     */
    ret = dbp->put(dbp, NULL, &key, &data, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Database put failed.");
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
