---
title: "Managing Databases in Environments"
api-name: "Managing Databases in Environments"
source: docs/gsg/CXX/CoreEnvUsage.html
---
## Managing Databases in Environments

In <a href="environments.md" class="xref" title="Environments">Environments</a>, we introduced environments. While environments are not used in the example built in this book, they are so commonly used for a wide class of DB applications that it is necessary to show their basic usage, if only from a completeness perspective.

To use an environment, you must first open it. At open time, you must identify the directory in which it resides. This directory must exist prior to the open attempt. You can also identify open properties, such as whether the environment can be created if it does not already exist.

You will also need to initialize the in-memory cache when you open your environment.

For example, to create an environment handle and open an environment:

``` c
#include <db_cxx.h>
...
u_int32_t env_flags = DB_CREATE |     // If the environment does not
                                      // exist, create it.
                      DB_INIT_MPOOL; // Initialize the in-memory cache.

std::string envHome("/export1/testEnv");
DbEnv myEnv(0);

try {
    myEnv.open(envHome.c_str(), env_flags, 0);
} catch(DbException &e) {
    std::cerr << "Error opening database environment: "
              << envHome << std::endl;
    std::cerr << e.what() << std::endl;
    exit( -1 );
} catch(std::exception &e) {
    std::cerr << "Error opening database environment: "
              << envHome << std::endl;
    std::cerr << e.what() << std::endl;
    exit( -1 );
} 
```

Once an environment is opened, you can open databases in it. Note that by default databases are stored in the environment's home directory, or relative to that directory if you provide any sort of a path in the database's file name:

``` c
#include <db_cxx.h>
...
u_int32_t env_flags = DB_CREATE |  // If the environment does not
                                  // exist, create it.
                      DB_INIT_MPOOL; // Initialize the in-memory cache.
std::string envHome("/export1/testEnv");
                    
u_int32_t db_flags = DB_CREATE;   // If the database does not
                                  // exist, create it.
std::string dbName("mydb.db");
DbEnv myEnv(0);
Db *myDb;

try {
    myEnv.open(envHome.c_str(), env_flags, 0);
    myDb = new Db(&myEnv, 0);
    myDb->open(NULL,
               dbName.c_str(),
               NULL,
               DB_BTREE,
               db_flags,
               0);
} catch(DbException &e) {
    std::cerr << "Error opening database environment: "
              << envHome 
              << " and database "
              << dbName << std::endl;
    std::cerr << e.what() << std::endl;
    exit( -1 );
} catch(std::exception &e) {
    std::cerr << "Error opening database environment: "
              << envHome 
              << " and database "
              << dbName << std::endl;
    std::cerr << e.what() << std::endl;
    exit( -1 );
} 
```

When you are done with an environment, you must close it. It is recommended that before closing an environment, you close any open databases.

``` c
try {
    if (myDb != NULL) {
        myDb->close(0);
    }
    myEnv.close(0);
    
} catch(DbException &e) {
    std::cerr << "Error closing database environment: "
              << envHome 
              << " or database "
              << dbName << std::endl;
    std::cerr << e.what() << std::endl;
    exit( -1 );
} catch(std::exception &e) {
    std::cerr << "Error closing database environment: "
              << envHome 
              << " or database "
              << dbName << std::endl;
    std::cerr << e.what() << std::endl;
    exit( -1 );
} 
```
