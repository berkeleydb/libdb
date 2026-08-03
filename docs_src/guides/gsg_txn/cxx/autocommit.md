---
title: "Auto Commit"
api-name: "Auto Commit"
source: docs/gsg_txn/CXX/autocommit.html
---
## Auto Commit

While transactions are frequently used to provide atomicity to multiple database operations, it is sometimes necessary to perform a single database operation under the control of a transaction. Rather than force you to obtain a transaction, perform the single write operation, and then either commit or abort the transaction, you can automatically group this sequence of events using <span class="emphasis">*auto commit*</span>.

To use auto commit:

1.  Open your environment and your databases so that they support transactions. See <a href="enabletxn.md" class="xref" title="Chapter 2. Enabling Transactions">Enabling Transactions</a> for details.

    Note that frequently auto commit is used for the environment or database open. To use auto commit for either your environment or database open, specify `DB_AUTO_COMMIT` to the `DbEnv::set_flags()` or `Db::open()` method. If you specify auto commit for the environment open, then you do not need to also specify auto commit for the database open.

2.  Do not provide a transactional handle to the method that is performing the database write operation.

Note that auto commit is not available for cursors. You must always open your cursor using a transaction if you want the cursor's operations to be transactional protected. See <a href="txncursor.md" class="xref" title="Transactional Cursors">Transactional Cursors</a> for details on using transactional cursors.

### Note

Never have more than one active transaction in your thread at a time. This is especially a problem if you mix an explicit transaction with another operation that uses auto commit. Doing so can result in undetectable deadlocks.

For example, the following uses auto commit to perform the database write operation:

``` c
#include "db_cxx.h"

...

int main(void)
{
    u_int32_t env_flags = DB_CREATE     |  // If the environment does not
                                           // exist, create it.
                          DB_INIT_LOCK  |  // Initialize locking
                          DB_INIT_LOG   |  // Initialize logging
                          DB_INIT_MPOOL |  // Initialize the cache
                          DB_INIT_TXN;     // Initialize transactions

    u_int32_t db_flags = DB_CREATE | DB_AUTO_COMMIT;
    Db *dbp = NULL;
    const char *file_name = "mydb.db";
    const char *keystr ="thekey";
    const char *datastr = "thedata";

    std::string envHome("/export1/testEnv");
    DbEnv myEnv(0);

    try {

        myEnv.open(envHome.c_str(), env_flags, 0);
        dbp = new Db(&myEnv, 0);

        // Open the database. Note that we are using auto commit for 
        // the open, so the database is able to support transactions.
        dbp->open(NULL,       // Txn pointer
                  file_name,  // File name
                  NULL,       // Logical db name */
                  DB_BTREE,   // Database type (using btree)
                  db_flags,   // Open flags
                  0);         // File mode. Using defaults

        Dbt key, data;
        key.set_data(keystr);
        key.set_size((strlen(keystr) + 1) * sizeof(char));
        key.set_data(datastr);
        key.set_size((strlen(datastr) + 1) * sizeof(char));

        // Perform the write. Because the database was opened to support
        // auto commit, this write is performed using auto commit.
        db->put(NULL, &key, &data, 0);

    } catch(DbException &e) {
        std::cerr << "Error opening database and environment: "
                  << file_name << ", "
                  << envHome << std::endl;
        std::cerr << e.what() << std::endl;
    }

    try {
        if (dbp != NULL) 
            dbp->close(0);
        myEnv.close(0);
    } catch(DbException &e) {
        std::cerr << "Error closing database and environment: "
                  << file_name << ", "
                  << envHome << std::endl;
        std::cerr << e.what() << std::endl;
        return (EXIT_FAILURE);
    }

    return (EXIT_SUCCESS);
} 
```
