---
title: "Configuring the Transaction Subsystem"
api-name: "Configuring the Transaction Subsystem"
source: docs/gsg_txn/CXX/maxtxns.html
---
## Configuring the Transaction Subsystem

Most of the configuration activities that you need to perform for your transactional DB application will involve the locking and logging subsystems. See <a href="txnconcurrency.md" class="xref" title="Chapter 4. Concurrency">Concurrency</a> and <a href="filemanagement.md" class="xref" title="Chapter 5. Managing DB Files">Managing DB Files</a> for details.

However, there are a couple of things that you can do to configure your transaction subsystem directly. These things are:

- 

  Configure the maximum number of simultaneous transactions needed by your application. In general, you should not need to do this unless you use deeply nested transactions or you have many threads all of which have active transactions. In addition, you may need to configure a higher maximum number of transactions if you are using snapshot isolation. See <a href="isolation.md#sisolation_maxtxn" class="xref" title="Snapshot Isolation Transactional Requirements">Snapshot Isolation Transactional Requirements</a> for details.

  By default, your application can support 20 active transactions.

  You can set the maximum number of simultaneous transactions supported by your application using the `DbEnv::set_tx_max()` method. Note that this method must be called before the environment has been opened.

  If your application has exceeded this maximum value, then any attempt to begin a new transaction will fail.

  This value can also be set using the `DB_CONFIG` file's `set_tx_max` parameter. Remember that the `DB_CONFIG` must reside in your environment home directory.

- 

  Configure the timeout value for your transactions. This value represents the longest period of time a transaction can be active. Note, however, that transaction timeouts are checked only when DB examines its lock tables for blocked locks (see <a href="blocking_deadlocks.md" class="xref" title="Locks, Blocks, and Deadlocks">Locks, Blocks, and Deadlocks</a> for more information). Therefore, a transaction's timeout can have expired, but the application will not be notified until DB has a reason to examine its lock tables.

  Be aware that some transactions may be inappropriately timed out before the transaction has a chance to complete. You should therefore use this mechanism only if you know your application might have unacceptably long transactions and you want to make sure your application will not stall during their execution. (This might happen if, for example, your transaction blocks or requests too much data.)

  Note that by default transaction timeouts are set to 0 seconds, which means that they never time out.

  To set the maximum timeout value for your transactions, use the `DbEnv::set_timeout()` method. This method configures the entire environment; not just the handle used to set the configuration. Further, this value may be set at any time during the application's lifetime.

  This value can also be set using the `DB_CONFIG` file's `set_txn_timeout` parameter.

For example:

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

    std::string envHome("/export1/testEnv");
    DbEnv myEnv(0);

    try {

        // Configure a maximum transaction timeout of 1 second.
        myEnv.set_timeout(1000000, DB_SET_TXN_TIMEOUT);
        // Configure 40 maximum transactions.
        myEnv.set_tx_max(40);
        myEnv.open(envHome.c_str(), env_flags, 0);

        // From here, you open your databases, proceed with your 
        // database operations, and respond to deadlocks as 
        // is normal (omitted for brevity).

        
        ...
```
