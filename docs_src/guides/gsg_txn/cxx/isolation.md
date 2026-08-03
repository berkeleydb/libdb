---
title: "Isolation"
api-name: "Isolation"
source: docs/gsg_txn/CXX/isolation.html
---
## Isolation

<span class="sect2"> [Supported Degrees of Isolation](isolation.md#degreesofisolation) </span>

<span class="sect2"> [Reading Uncommitted Data](isolation.md#dirtyreads) </span>

<span class="sect2"> [Committed Reads](isolation.md#readcommitted) </span>

<span class="sect2"> [Using Snapshot Isolation](isolation.md#snapshot_isolation) </span>

Isolation guarantees are an important aspect of transactional protection. Transactions ensure the data your transaction is working with will not be changed by some other transaction. Moreover, the modifications made by a transaction will never be viewable outside of that transaction until the changes have been committed.

That said, there are different degrees of isolation, and you can choose to relax your isolation guarantees to one degree or another depending on your application's requirements. The primary reason why you might want to do this is because of performance; the more isolation you ask your transactions to provide, the more locking that your application must do. With more locking comes a greater chance of blocking, which in turn causes your threads to pause while waiting for a lock. Therefore, by relaxing your isolation guarantees, you can <span class="emphasis">*potentially*</span> improve your application's throughput. Whether you actually see any improvement depends, of course, on the nature of your application's data and transactions.

### Supported Degrees of Isolation

DB supports the following levels of isolation:

<table data-border="1" width="80%">
<thead>
<tr>
<th>Degree</th>
<th>ANSI Term</th>
<th>Definition</th>
</tr>
</thead>
<tbody>
<tr>
<td>1</td>
<td>READ UNCOMMITTED</td>
<td>Uncommitted reads means that one transaction will never overwrite another transaction's dirty data. Dirty data is data that a transaction has modified but not yet committed to the underlying data store. However, uncommitted reads allows a transaction to see data dirtied by another transaction. In addition, a transaction may read data dirtied by another transaction, but which subsequently is aborted by that other transaction. In this latter case, the reading transaction may be reading data that never really existed in the database.</td>
</tr>
<tr>
<td>2</td>
<td>READ COMMITTED</td>
<td><p>Committed read isolation means that degree 1 is observed, except that dirty data is never read.</p>
<p>In addition, this isolation level guarantees that data will never change so long as it is addressed by the cursor, but the data may change before the reading cursor is closed. In the case of a transaction, data at the current cursor position will not change, but once the cursor moves, the previous referenced data can change. This means that readers release read locks before the cursor is closed, and therefore, before the transaction completes. Note that this level of isolation causes the cursor to operate in exactly the same way as it does in the absence of a transaction.</p></td>
</tr>
<tr>
<td>3</td>
<td>SERIALIZABLE</td>
<td><p>Committed read is observed, plus the data read by a transaction, T, will never be dirtied by another transaction before T completes. This means that both read and write locks are not released until the transaction completes.</p>
<p><span> In addition, </span> no transactions will see phantoms. Phantoms are records returned as a result of a search, but which were not seen by the same transaction when the identical search criteria was previously used.</p>
<p>This is DB's default isolation guarantee.</p></td>
</tr>
</tbody>
</table>

By default, DB transactions and transactional cursors offer serializable isolation. You can optionally reduce your isolation level by configuring DB to use uncommitted read isolation. See <a href="isolation.md#dirtyreads" class="xref" title="Reading Uncommitted Data">Reading Uncommitted Data</a> for more information. You can also configure DB to use committed read isolation. See <a href="isolation.md#readcommitted" class="xref" title="Committed Reads">Committed Reads</a> for more information.

Finally, in addition to DB's normal degrees of isolation, you can also use <span class="emphasis">*snapshot isolation*</span>. This allows you to avoid the read locks that serializable isolation requires. See <a href="isolation.md#snapshot_isolation" class="xref" title="Using Snapshot Isolation">Using Snapshot Isolation</a> for details.

### Reading Uncommitted Data

Berkeley DB allows you to configure your application to read data that has been modified but not yet committed by another transaction; that is, dirty data. When you do this, you may see a performance benefit by allowing your application to not have to block waiting for write locks. On the other hand, the data that your application is reading may change before the transaction has completed.

When used with transactions, uncommitted reads means that one transaction can see data modified but not yet committed by another transaction. When used with transactional cursors, uncommitted reads means that any database reader can see data modified by the cursor before the cursor's transaction has committed.

Because of this, uncommitted reads allow a transaction to read data that may subsequently be aborted by another transaction. In this case, the reading transaction will have read data that never really existed in the database.

To configure your application to read uncommitted data:

1.  Open your database such that it will allow uncommitted reads. You do this by specifying `DB_READ_UNCOMMITTED` when you open your database.

2.   Specify `DB_READ_UNCOMMITTED` when you create the transaction, open the cursor, or read a record from the database.

For example, the following opens the database such that it supports uncommitted reads, and then creates a transaction that causes all reads performed within it to use uncommitted reads. Remember that simply opening the database to support uncommitted reads is not enough; you must also declare your read operations to be performed using uncommitted reads.

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
                          DB_THREAD     |  // Free-thread the env handle
                          DB_INIT_TXN;     // Initialize transactions

    u_int32_t db_flags = DB_CREATE |           // Create the db if it does
                                               // not exist
                         DB_AUTO_COMMIT |      // Enable auto commit
                         DB_READ_UNCOMMITTED;  // Enable uncommitted reads

    Db *dbp = NULL;
    const char *file_name = "mydb.db";
    const char *keystr ="thekey";
    const char *datastr = "thedata";

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

        DbTxn *txn = NULL;
        myEnv.txn_begin(NULL, &txn, DB_READ_UNCOMMITTED);

        // From here, you perform your database reads and writes as
        // normal, committing and aborting the transactions as is 
        // necessary, and testing for deadlock exceptions as normal 
        // (omitted for brevity). 

        ...
```

### Committed Reads

You can configure your transaction so that the data being read by a transactional cursor is consistent so long as it is being addressed by the cursor. However, once the cursor is done reading the record (that is, reading records from the page that it currently has locked), the cursor releases its lock on that record or page. This means that the data the cursor has read and released may change before the cursor's transaction has completed.

For example, suppose you have two transactions, `Ta` and `Tb`. Suppose further that `Ta` has a cursor that reads `record R`, but does not modify it. Normally, `Tb` would then be unable to write `record R` because `Ta` would be holding a read lock on it. But when you configure your transaction for committed reads, `Tb` <span class="emphasis">*can*</span> modify `record R` before `Ta` completes, so long as the reading cursor is no longer addressing the record or page.

When you configure your application for this level of isolation, you may see better performance throughput because there are fewer read locks being held by your transactions. Read committed isolation is most useful when you have a cursor that is reading and/or writing records in a single direction, and that does not ever have to go back to re-read those same records. In this case, you can allow DB to release read locks as it goes, rather than hold them for the life of the transaction.

To configure your application to use committed reads, do one of the following:

- Create your transaction such that it allows committed reads. You do this by specifying `DB_READ_COMMITTED` when you open the transaction.

-  Specify `DB_READ_COMMITTED` when you open the cursor.

For example, the following creates a transaction that allows committed reads:

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
                          DB_THREAD     |  // Free-thread the env handle
                          DB_INIT_TXN;     // Initialize transactions

    // Notice that we do not have to specify any flags to the database to
    // allow committed reads (this is as opposed to uncommitted reads 
    // where we DO have to specify a flag on the database open.
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

        DbTxn *txn = NULL;

        // Open the transaction and enable committed reads. All cursors 
        // open with this transaction handle will use read committed 
        // isolation.
        myEnv.txn_begin(NULL, &txn, DB_READ_COMMITTED);

        // From here, you perform your database reads and writes as
        // normal, committing and aborting the transactions as is
        // necessary, testing for deadlock exceptions as normal 
        // (omitted for brevity). 

        // Using transactional cursors with concurrent applications is 
        // described in more detail in the following section.
        
        ...
```

### Using Snapshot Isolation

By default DB uses serializable isolation. An important side effect of this isolation level is that read operations obtain read locks on database pages, and then hold those locks until the read operation is completed. When you are using transactional cursors, this means that read locks are held until the transaction commits or aborts. In that case, over time a transactional cursor can gradually block all other transactions from writing to the database.

You can avoid this by using snapshot isolation. Snapshot isolation uses <span class="emphasis">*multiversion concurrency control*</span> to guarantee repeatable reads. What this means is that every time a writer would take a read lock on a page, instead a copy of the page is made and the writer operates on that page copy. This frees other writers from blocking due to a read lock held on the page.

### Note

Snapshot isolation is strongly recommended for read-only threads when writer threads are also running, as this will eliminate read-write contention and greatly improve transaction throughput for your writer threads. However, in order for snapshot isolation to work for your reader-only threads, you must of course use transactions for your DB reads.

#### Snapshot Isolation Cost

Snapshot isolation does not come without a cost. Because pages are being duplicated before being operated upon, the cache will fill up faster. This means that you might need a larger cache in order to hold the entire working set in memory.

If the cache becomes full of page copies before old copies can be discarded, additional I/O will occur as pages are written to temporary "freezer" files on disk. This can substantially reduce throughput, and should be avoided if possible by configuring a large cache and keeping snapshot isolation transactions short.

You can estimate how large your cache should be by taking a checkpoint, followed by a call to the `DbEnv::log_archive()` method. The amount of cache required is approximately double the size of the remaining log files (that is, the log files that cannot be archived).

#### Snapshot Isolation Transactional Requirements

In addition to an increased cache size, you may also need to increase the number of transactions that your application supports. (See <a href="maxtxns.md" class="xref" title="Configuring the Transaction Subsystem">Configuring the Transaction Subsystem</a> for details on how to set this.) In the worst case scenario, you might need to configure your application for one more transaction for every page in the cache. This is because transactions are retained until the last page they created is evicted from the cache.

#### When to Use Snapshot Isolation

Snapshot isolation is best used when all or most of the following conditions are true:

- You can have a large cache relative to your working data set size.

- You require repeatable reads.

- You will be using transactions that routinely work on the entire database, or more commonly, there is data in your database that will be very frequently written by more than one transaction.

- Read/write contention is limiting your application's throughput, or the application is all or mostly read-only and contention for the lock manager mutex is limiting throughput.

#### How to use Snapshot Isolation

You use snapshot isolation by:

- Opening the database with multiversion support. You can configure this either when you open your environment or when you open your database. Use the `DB_MULTIVERSION` flag to configure this support.

- Configure your cursor or transaction to use snapshot isolation.

  To do this, pass the `DB_TXN_SNAPSHOT` flag when you open the cursor or create the transaction. If configured for the transaction, then this flag is not required when the cursor is opened.

The simplest way to take advantage of snapshot isolation is for queries: keep update transactions using full read/write locking and use snapshot isolation on read-only transactions or cursors. This should minimize blocking of snapshot isolation transactions and will avoid deadlock errors.

If the application has update transactions which read many items and only update a small set (for example, scanning until a desired record is found, then modifying it), throughput may be improved by running some updates at snapshot isolation as well. But doing this means that you must manage deadlock errors. See <a href="lockingsubsystem.md#deadlockresolve" class="xref" title="Resolving Deadlocks">Resolving Deadlocks</a> for details.

The following code fragment turns on snapshot isolation for a transaction:

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
                          
    // Note that no special flags are required here for snapshot isolation.
    // This is because it will be enabled at the environment level.
    u_int32_t db_flags = DB_CREATE | DB_AUTO_COMMIT;
    Db *dbp = NULL;
    const char *file_name = "mydb.db";

    std::string envHome("/export1/testEnv");
    DbEnv myEnv(0);

    try {

        myEnv.open(envHome.c_str(), env_flags, 0);
        // Support snapshot isolation
        myEnv.set_flags(DB_MULTIVERSION, 1);
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

    ....

    envp->txn_begin(NULL, txn, DB_TXN_SNAPSHOT);

    // Remainder of program omitted for brevity.

 
```
