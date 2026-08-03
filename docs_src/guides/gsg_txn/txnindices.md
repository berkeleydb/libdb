---
title: "Secondary Indices with Transaction Applications"
api-name: "Secondary Indices with Transaction Applications"
source: docs/gsg_txn/C/txnindices.html
---
## Secondary Indices with Transaction Applications

You can use transactions with your secondary indices so long as you open the secondary index so that it supports transactions (that is, you wrap the database open in a transaction, or use auto commit, in the same way as when you open a primary transactional database). In addition, you must make sure that when you associate the secondary index with the primary database, the association is performed using a transaction. The easiest thing to do here is to simply specify `DB_AUTO_COMMIT` when you perform the association.

All other aspects of using secondary indices with transactions are identical to using secondary indices without transactions. In addition, transaction-protecting cursors opened against secondary indices is performed in exactly the same way as when you use transactional cursors against a primary database. See <a href="txncursor.md" class="xref" title="Transactional Cursors">Transactional Cursors</a> for details.

Note that when you use transactions to protect your database writes, your secondary indices are protected from corruption because updates to the primary and the secondaries are performed in a single atomic transaction.

For example:

``` c
#include <db.h>

...

DB_ENV *envp;      /* Environment pointer */
DB *dbp, *sdbp;    /* Primary and secondary DB handles */
u_int32_t flags;   /* Primary database open flags */
int ret;           /* Function return value */

/* Environment and primary database opens omitted */

/* Secondary */
ret = db_create(&sdbp, envp, 0);
if (ret != 0) {
  /* Error handling goes here */
}
/* open the secondary database */
ret = sdbp->open(sdbp,          /* DB structure pointer */
                 NULL,            /* Transaction pointer */
                 "my_secdb.db",   /* On-disk file that holds the 
                                   * database. */
                 NULL,            /* Optional logical database name */
                 DB_BTREE,        /* Database access method */
                 DB_AUTO_COMMIT,  /* Open flags */
                 0);              /* File mode (using defaults) */
if (ret != 0) {
  /* Error handling goes here */
}

/* Now associate the secondary to the primary */
dbp->associate(dbp,            /* Primary database */
               NULL,           /* TXN id */
               sdbp,           /* Secondary database */
               get_sales_rep,  /* Callback used for key creation. This
                                * is described in the Getting Started
                                * guide. */
               DB_AUTO_COMMIT);/* Flags */
```
