---
title: "Transactional Cursors"
api-name: "Transactional Cursors"
source: docs/gsg_txn/C/txncursor.html
---
## Transactional Cursors

You can transaction-protect your cursor operations by specifying a transaction handle at the time that you create your cursor. Beyond that, you do not ever provide a transaction handle directly to a cursor method.

Note that if you transaction-protect a cursor, then you must make sure that the cursor is closed before you either commit or abort the transaction. For example:

``` c
#include <stdio.h>
#include <stdlib.h>

#include "db.h"

int
main(void)
{
    DBT key, data;
    DBC *cursorp;
    DB_TXN *txn = NULL;
    int ret, c_ret;
    char *replacementString = "new string";

    ...
    /* environment and db handle creation omitted */
    ...

    /* Get the txn handle */
    txn = NULL;
    ret = envp->txn_begin(envp, NULL, &txn, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Transaction begin failed.");
        goto err;
    }

    /* Get the cursor, supply the txn handle at that time */
    ret = dbp->cursor(dbp, txn, &cursorp, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Cursor open failed.");
        txn->abort(txn);
        goto err;
    }

    /* 
     * Now use the cursor. Note that we do not supply any txn handles to 
     * these methods.
     */
    /* Prepare the DBTs */
    memset(&key, 0, sizeof(DBT));
    memset(&data, 0, sizeof(DBT));
    while (cursor->get(&key, &data, DB_NEXT) == 0) {
        data->data = (void *)replacementString;
        data->size = (strlen(replacementString) + 1) * sizeof(char);
        c_ret = cursor->put(cursor, &key, &data, DB_CURRENT);
        if (c_ret != 0) {
            /* abort the transaction and goto error */ 
            envp->err(envp, ret, "Cursor put failed.");
            cursorp->close(cursorp);
            cursorp = NULL;
            txn->abort(txn);
            goto err;
        }
    }

    /* 
     * Commit the transaction. Note that the transaction handle
     * can no longer be used.
     */ 
    ret = cursorp->close(cursorp);
    if (ret != 0) {
        envp->err(envp, ret, "Cursor close failed.");
        txn->abort(txn);
        goto err;
    }
    ret = txn->commit(txn, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Transaction commit failed.");
        goto err;
    }

err:
    /* Close the cursor (if the handle is not NULL)
     * and perform whatever other cleanup is required */

    /* Close the database */

    /* Close the environment */

    ...

    if (c_ret != 0)
        ret = c_ret;
    
    return (ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
} 
```
