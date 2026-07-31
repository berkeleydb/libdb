---
title: "Reverse BTree Splits"
api-name: "Reverse BTree Splits"
source: docs/gsg_txn/C/reversesplit.html
---
## Reverse BTree Splits

If your application is using the Btree access method, and your application is repeatedly deleting then adding records to your database, then you might be able to reduce lock contention by turning off reverse Btree splits.

As pages are emptied in a database, DB attempts to delete empty pages in order to keep the database as small as possible and minimize search time. Moreover, when a page in the database fills up, DB, of course, adds additional pages to make room for more data.

Adding and deleting pages in the database requires that the writing thread lock the parent page. Consequently, as the number of pages in your database diminishes, your application will see increasingly more lock contention; the maximum level of concurrency in a database of two pages is far smaller than that in a database of 100 pages, because there are fewer pages that can be locked.

Therefore, if you prevent the database from being reduced to a minimum number of pages, you can improve your application's concurrency throughput. Note, however, that you should do so only if your application tends to delete and then add the same data. If this is not the case, then preventing reverse Btree splits can harm your database search time.

To turn off reverse Btree splits, provide the `DB_REVSPLITOFF` flag to the `DB->set_flags()` method.

For example:

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
    const char *db_home_dir = "/tmp/myEnvironment";
    const char *file_name = "mydb.db";
    
    dbp = NULL;
    envp = NULL;

    /* Open the environment */
    ret = db_env_create(&envp, 0);
    if (ret != 0) {
        fprintf(stderr, "Error creating environment handle: %s\n",
            db_strerror(ret));
        return (EXIT_FAILURE);
    }

    env_flags = DB_CREATE      | 
                DB_INIT_LOCK   | 
                DB_INIT_LOG    | 
                DB_INIT_TXN    |
                DB_THREAD      | 
                DB_INIT_MPOOL;

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

    /* Set btree reverse split to off */
    ret = db->set_flags(&db, DB_REVSPLITOFF);
    if (ret != 0) {
        envp->err(envp, ret, "Turning off Btree reverse split failed");
        goto err;
    }

    db_flags = DB_CREATE | DB_AUTO_COMMIT;
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
