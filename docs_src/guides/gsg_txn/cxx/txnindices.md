---
title: "Secondary Indices with Transaction Applications"
api-name: "Secondary Indices with Transaction Applications"
source: docs/gsg_txn/CXX/txnindices.html
---
## Secondary Indices with Transaction Applications

You can use transactions with your secondary indices so long as you open the secondary index so that it supports transactions (that is, you wrap the database open in a transaction, or use auto commit, in the same way as when you open a primary transactional database). In addition, you must make sure that when you associate the secondary index with the primary database, the association is performed using a transaction. The easiest thing to do here is to simply specify `DB_AUTO_COMMIT` when you perform the association.

All other aspects of using secondary indices with transactions are identical to using secondary indices without transactions. In addition, transaction-protecting cursors opened against secondary indices is performed in exactly the same way as when you use transactional cursors against a primary database. See <a href="txncursor.md" class="xref" title="Transactional Cursors">Transactional Cursors</a> for details.

Note that when you use transactions to protect your database writes, your secondary indices are protected from corruption because updates to the primary and the secondaries are performed in a single atomic transaction.

For example:

``` c
#include <db_cxx.h>

...

// Environment and primary database open omitted
...

Db my_index(&envp, 0);    // Secondary

// Open the secondary
my_index.open(NULL,              // Transaction pointer
              "my_secondary.db", // On-disk file that holds the database.
              NULL,              // Optional logical database name
              DB_BTREE,          // Database access method
              DB_AUTO_COMMIT,    // Open flags.
              0);                // File mode (using defaults)

// Now associate the primary and the secondary
my_database.associate(NULL,             // Txn id
                      &my_index,        // Associated secondary database
                      get_sales_rep,    // Callback used for key
                                        // extraction. This is described 
                                        // in the Getting Started guide.
                      DB_AUTO_COMMIT);  // Flags 
```
