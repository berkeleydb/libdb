---
title: "Transactional Cursors"
api-name: "Transactional Cursors"
source: docs/gsg_txn/CXX/txncursor.html
---
## Transactional Cursors

You can transaction-protect your cursor operations by specifying a transaction handle at the time that you create your cursor. Beyond that, you do not ever provide a transaction handle directly to a cursor method.

Note that if you transaction-protect a cursor, then you must make sure that the cursor is closed before you either commit or abort the transaction. For example:

``` c
#include "db_cxx.h"

...

int main(void)
{
    // Environment and database opens omitted
    ...

    DbTxn *txn = NULL;
    Dbc *cursorp = NULL;

    try {

        Dbt key, data;
        key.set_data(keystr);
        key.set_size((strlen(keystr) + 1) * sizeof(char));
        key.set_data(datastr);
        key.set_size((strlen(datastr) + 1) * sizeof(char));

        DbTxn *txn = NULL;
        myEnv.txn_begin(NULL, &txn, 0);
        try {
            // Get our cursor. Note that we pass the transaction handle 
            // here.
            db.cursor(txn, &cursorp, 0); 

            // Perform our operations. Note that we do not pass a 
            // transaction handle here.
            char *replacementString = "new string";
            while (cursor->get(&key, &data, DB_NEXT) == 0) {
                data.set_data(void *)replacementString);
                data.set_size((strlen(replacementString) + 1) * 
                    sizeof(char));
                cursor->put(&key, &data, DB_CURRENT);
            }

            // We're done. Commit the transaction.
            cursor->close();
            txn->commit(0);
        } catch (DbException &e) {
            std::cerr << "Error in transaction: "
                       << e.what() << std::endl;
            cursor->close();
            txn->abort();
        }

    } catch(DbException &e) {
        std::cerr << "Error opening database and environment: "
                  << file_name << ", "
                  << envHome << std::endl;
        std::cerr << e.what() << std::endl;
    }

    return (EXIT_SUCCESS);
} 
```
