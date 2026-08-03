---
title: "Transactional Cursors and Concurrent Applications"
api-name: "Transactional Cursors and Concurrent Applications"
source: docs/gsg_txn/CXX/txn_ccursor.html
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

    u_int32_t db_flags = DB_CREATE |           // Create the db if it does
                                               // not exist
                         DB_AUTO_COMMIT |      // Enable auto commit
                         DB_READ_UNCOMMITTED;  // Enable uncommitted reads

    Db *dbp = NULL;
    const char *file_name = "mydb.db";

    std::string envHome("/export1/testEnv");
    DbEnv myEnv(0);

    Dbc *cursorp = NULL;

    try {

        myEnv.open(envHome.c_str(), env_flags, 0);
        dbp = new Db(&myEnv, 0);
        dbp->open(NULL,       // Txn pointer
                  file_name,  // File name
                  NULL,       // Logical db name
                  DB_BTREE,   // Database type (using btree)
                  db_flags,   // Open flags
                  0);         // File mode. Using defaults

        DbTxn *txn = NULL;
        myEnv.txn_begin(NULL, &txn, 0);
        try {
            // Get our cursor. Note that we pass the transaction 
            // handle here. Note also that we pass the 
            // DB_READ_UNCOMMITTED flag here so as to cause the
            // cursor to perform uncommitted reads.
            db.cursor(txn, &cursorp, DB_READ_UNCOMMITTED); 

            // From here, you perform your cursor reads and writes 
            // as normal, committing and aborting the transactions as 
            // is necessary, and testing for deadlock exceptions as 
            // normal (omitted for brevity). 
        
            ... 
```
