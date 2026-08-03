---
title: "Reverse BTree Splits"
api-name: "Reverse BTree Splits"
source: docs/gsg_txn/CXX/reversesplit.html
---
## Reverse BTree Splits

If your application is using the Btree access method, and your application is repeatedly deleting then adding records to your database, then you might be able to reduce lock contention by turning off reverse Btree splits.

As pages are emptied in a database, DB attempts to delete empty pages in order to keep the database as small as possible and minimize search time. Moreover, when a page in the database fills up, DB, of course, adds additional pages to make room for more data.

Adding and deleting pages in the database requires that the writing thread lock the parent page. Consequently, as the number of pages in your database diminishes, your application will see increasingly more lock contention; the maximum level of concurrency in a database of two pages is far smaller than that in a database of 100 pages, because there are fewer pages that can be locked.

Therefore, if you prevent the database from being reduced to a minimum number of pages, you can improve your application's concurrency throughput. Note, however, that you should do so only if your application tends to delete and then add the same data. If this is not the case, then preventing reverse Btree splits can harm your database search time.

To turn off reverse Btree splits, provide the `DB_REVSPLITOFF` flag to the `Db::set_flags()` method.

For example:

``` c
#include "db_cxx.h"

...

int main(void)
{
    u_int32_t env_flags = DB_CREATE     |  // If the environment does not
                                           // exist, create it.
                          DB_INIT_LOCK  |  // Initialize locking
                          DB_INIT_LOG   |  // Initialize locking
                          DB_INIT_MPOOL |  // Initialize the cache
                          DB_THREAD     |  // Free-thread the env handle
                          DB_INIT_TXN;     // Initialize transactions

    u_int32_t db_flags = DB_CREATE | DB_AUTO_COMMIT;
    Db *dbp = NULL;
    const char *file_name = "mydb.db";

    std::string envHome("/export1/testEnv");
    DbEnv myEnv(0);

    try {

        myEnv.open(envHome.c_str(), env_flags, 0);
        dbp = new Db(&myEnv, 0);

        // Turn off BTree reverse split.
        dbp=>set_flags(DB_REVSPLITOFF);

        dbp->open(dbp,        // Pointer to the database 
                  NULL,       // Txn pointer 
                  file_name,  // File name 
                  NULL,       // Logical db name 
                  DB_BTREE,   // Database type (using btree)
                  db_flags,   // Open flags 
                  0);         // File mode. Using defaults

    } catch(DbException &e) {
        std::cerr << "Error opening database and environment: "
                  << file_name << ", " << envHome << std::endl;
        std::cerr << e.what() << std::endl;
    }

    try {
        dbp->close(dbp, 0);
        myEnv.close(0);
    } catch(DbException &e) {
        std::cerr << "Error closing database and environment: "
                  << file_name << ", " << envHome << std::endl;
        std::cerr << e.what() << std::endl;
        return (EXIT_FAILURE);
    }

    return (EXIT_SUCCESS);
} 
```
