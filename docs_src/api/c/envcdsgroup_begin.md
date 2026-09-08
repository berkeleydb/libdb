---
title: "DB_ENV->cdsgroup_begin()"
api-name: "DB_ENV->cdsgroup_begin()"
source: docs/api_reference/C/envcdsgroup_begin.html
---
## DB_ENV-\>cdsgroup_begin()

``` c
#include <db.h>

int
DB_ENV->cdsgroup_begin(DB_ENV *dbenv, DB_TXN **tid);  
```

The `DB_ENV->cdsgroup_begin()` method allocates a locker ID in an environment configured for Berkeley DB Concurrent Data Store applications. It copies a pointer to a <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> that uniquely identifies the locker ID into the memory to which **tid** refers. Calling the <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a> method will discard the allocated locker ID.

See <a href="../../guides/programmer_reference/cam.md#cam_intro" class="olink">Berkeley DB Concurrent Data Store applications</a> for more information about when this is required.

The <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> handle returned by `DB_ENV->cdsgroup_begin()` is not a transaction handle: it names a CDS locker group, and a CDS environment has no transaction subsystem. Only <a href="txnabort.md" class="xref" title="DB_TXN-&gt;abort()">DB_TXN-&gt;abort()</a>, <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a>, <a href="txndiscard.md" class="xref" title="DB_TXN-&gt;discard()">DB_TXN-&gt;discard()</a> and <a href="txnid.md" class="xref" title="DB_TXN-&gt;id()">DB_TXN-&gt;id()</a> are meaningful on it. Every other `DB_TXN` method — <a href="txnprepare.md" class="xref" title="DB_TXN-&gt;prepare()">DB_TXN-&gt;prepare()</a>, <a href="txnget_name.md" class="xref" title="DB_TXN-&gt;get_name()">DB_TXN-&gt;get_name()</a>, <a href="txnset_name.md" class="xref" title="DB_TXN-&gt;set_name()">DB_TXN-&gt;set_name()</a>, <a href="txnset_timeout.md" class="xref" title="DB_TXN-&gt;set_timeout()">DB_TXN-&gt;set_timeout()</a>, <a href="txnget_priority.md" class="xref" title="DB_TXN-&gt;get_priority()">DB_TXN-&gt;get_priority()</a>, <a href="txnset_priority.md" class="xref" title="DB_TXN-&gt;set_priority()">DB_TXN-&gt;set_priority()</a> and <a href="txnset_commit_token.md" class="xref" title="DB_TXN-&gt;set_commit_token()">DB_TXN-&gt;set_commit_token()</a> — returns `DB_OPNOTSUP` when called on a CDS group handle. (In releases before 5.3.35 the last three of these were unimplemented slots and calling one crashed the application rather than returning an error.)

The `DB_ENV->cdsgroup_begin()` method may be called at any time during the life of the application.

The `DB_ENV->cdsgroup_begin()` method returns a non-zero error value on failure and 0 on success.

### Errors

The `DB_ENV->cdsgroup_begin()` method may fail and return one of the following non-zero errors:

#### EINVAL

The environment was not configured for Berkeley DB Concurrent Data Store, that is, it was opened without the <a href="envopen.md#envopen_DB_INIT_CDB" class="link">DB_INIT_CDB</a> flag.

#### ENOMEM

The maximum number of lockers has been reached.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
