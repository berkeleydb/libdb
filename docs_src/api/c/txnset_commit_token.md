---
title: "DB_TXN->set_commit_token()"
api-name: "DB_TXN->set_commit_token()"
source: docs/api_reference/C/txnset_commit_token.html
---
## DB_TXN-\>set_commit_token()

``` c
#include <db.h>

int
DB_TXN->set_commit_token(DB_TXN *txn, DB_TXN_TOKEN *buffer);  
```

The `DB_TXN->set_commit_token()` method configures the transaction for commit token generation, and accepts the address of an application-supplied buffer to receive the token. The actual generation of the token contents does not occur until commit time.

Commit tokens are used to enable some consistency guarantees for replicated applications. Please see the <a href="../../guides/programmer_reference/rep_ryw.md" class="olink">Read your writes consistency</a> section in the *Berkeley DB Programmer's Reference Guide* for more information.

The `DB_TXN->set_commit_token()` method may be called at any time after the <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a> method has been called, and before <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a> has been called.

The `DB_TXN->set_commit_token()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### buffer

The address of an application-supplied buffer. The buffer memory must remain available, and will be filled in later by Berkeley DB, at the time of the commit() call.

### Errors

The `DB_TXN->set_commit_token()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the transaction is a nested transaction; if this method is called on a replication client; if the database environment is not configured for logging.

### Class

<a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>, <a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
