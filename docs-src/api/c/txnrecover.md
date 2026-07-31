---
title: "DB_ENV->txn_recover()"
api-name: "DB_ENV->txn_recover()"
source: docs/api_reference/C/txnrecover.html
---
## DB_ENV-\>txn_recover()

``` c
#include <db.h>

int
DB_ENV->txn_recover(DB_ENV *dbenv, DB_PREPLIST preplist[],
    long count, long *retp, u_int32_t flags);  
```

Database environment recovery restores transactions that were prepared, but not yet resolved at the time of the system shut down or crash, to their state prior to the shut down or crash, including any locks previously held. The `DB_ENV->txn_recover()` method returns a list of those prepared transactions.

The `DB_ENV->txn_recover()` method should only be called after the environment has been recovered.

Multiple threads of control may call `DB_ENV->txn_recover()`, but only one thread of control may resolve each returned transaction, that is, only one thread of control may call <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a> or <a href="txnabort.md" class="xref" title="DB_TXN-&gt;abort()">DB_TXN-&gt;abort()</a> on each returned transaction. Callers of `DB_ENV->txn_recover()` must call <a href="txndiscard.md" class="xref" title="DB_TXN-&gt;discard()">DB_TXN-&gt;discard()</a> to discard each transaction they do not resolve.

On return from `DB_ENV->txn_recover()`, the **preplist** parameter will be filled in with a list of transactions that must be resolved by the application (committed, aborted or discarded). The **preplist** parameter is a structure of type `DB_PREPLIST`; the following `DB_PREPLIST` fields will be filled in:

- **DB_TXN \* txn;**

  The transaction handle for the transaction.

- **u_int8_t gid\[DB_GID_SIZE\];**

  The global transaction ID for the transaction. The global transaction ID is the one specified when the transaction was prepared. The application is responsible for ensuring uniqueness among global transaction IDs.

The `DB_ENV->txn_recover()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### preplist

The **preplist** parameter references memory into which the list of transactions to be resolved by the application is copied.

#### count

The **count** parameter specifies the number of available entries in the passed-in **preplist** array. The **retp** parameter returns the number of entries `DB_ENV->txn_recover()` has filled in, in the array.

#### flags

The **flags** parameter must be set to one of the following values:

- `DB_FIRST`

  Begin returning a list of prepared, but not yet resolved transactions. Specifying this flag begins a new pass over all prepared, but not yet completed transactions, regardless of whether they have already been returned in previous calls to `DB_ENV->txn_recover.()` Calls to `DB_ENV->txn_recover()` from different threads of control should not be intermixed in the same environment.

- `DB_NEXT`

  Continue returning a list of prepared, but not yet resolved transactions, starting where the last call to `DB_ENV->txn_recover()` left off.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
