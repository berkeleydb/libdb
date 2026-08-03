---
title: "Opening a Transactional Environment and Database"
api-name: "Opening a Transactional Environment and Database"
source: docs/gsg_txn/CXX/envopen.html
---
## Opening a Transactional Environment and Database

To enable transactions for your environment, you must initialize the transactional subsystem. Note that doing this also initializes the logging subsystem. In addition, you must initialize the memory pool (in-memory cache). You must also initialize the locking subsystem. For example:

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

    std::string envHome("/export1/testEnv");
    DbEnv myEnv(0);

    try {

        myEnv.open(envHome.c_str(), env_flags, 0);

    } catch(DbException &e) {
        std::cerr << "Error opening database environment: "
                  << envHome << std::endl;
        std::cerr << e.what() << std::endl;
        return (EXIT_FAILURE);
    }

    try {
        myEnv.close(0);
    } catch(DbException &e) {
        std::cerr << "Error closing database environment: "
                << envHome << std::endl;
        std::cerr << e.what() << std::endl;
        return (EXIT_FAILURE);
    }

    return (EXIT_SUCCESS);
} 
```

You then create and open your database(s) as you would for a non-transactional system. The only difference is that you must pass the environment handle to the `DbEnv::open()` method, and you must open the database within a transaction. Typically auto commit is used for this purpose. To do so, pass `DB_AUTO_COMMIT` to the database open command. It is recommended that you close all your databases before you close your environment. For example:

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

    std::string envHome("/export1/testEnv");
    DbEnv myEnv(0);

    try {

        myEnv.open(envHome.c_str(), env_flags, 0);
        dbp = new Db(&myEnv, 0);
        dbp->open(NULL,       // Txn pointer 
                  file_name,  // File name 
                  NULL,       // Logical db name 
                  DB_BTREE,   // Database type (using btree) 
                  db_flags,   // Open flags 
                  0);         // File mode. Using defaults 

    } catch(DbException &e) {
        std::cerr << "Error opening database and environment: "
                  << file_name << ", "
                  << envHome << std::endl;
        std::cerr << e.what() << std::endl;
    }

    try {
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

### Note

Never close a database that has active transactions. Make sure all transactions are resolved (either committed or aborted) before closing the database.
