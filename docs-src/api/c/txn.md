---
title: "Chapter 12.  The DB_TXN Handle"
api-name: "Chapter 12.  The DB_TXN Handle"
source: docs/api_reference/C/txn.html
---
## Chapter 12.  The DB_TXN Handle

``` c
#include <db.h>

typedef struct __db_txn DB_TXN;  
```

The `DB_TXN` object is the handle for a transaction. Methods of the `DB_TXN` handle are used to configure, abort and commit the transaction. `DB_TXN` handles are provided to <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> methods in order to transactionally protect those database operations.

`DB_TXN` handles are not free-threaded; transactions handles may be used by multiple threads, but only serially, that is, the application must serialize access to the `DB_TXN` handle. Once the <a href="txnabort.md" class="xref" title="DB_TXN-&gt;abort()">DB_TXN-&gt;abort()</a> or <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a> methods are called, the handle may not be accessed again, regardless of the method's return. In addition, parent transactions may not issue any Berkeley DB operations while they have active child transactions (child transactions that have not yet been committed or aborted) except for <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>, <a href="txnabort.md" class="xref" title="DB_TXN-&gt;abort()">DB_TXN-&gt;abort()</a> and <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a>.

## Transaction Subsystem and Related Methods

<table data-border="1" width="80%">
<thead>
<tr>
<th>Transaction Subsystem and Related Methods</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><a href="txnrecover.md" class="xref" title="DB_ENV-&gt;txn_recover()">DB_ENV-&gt;txn_recover()</a></td>
<td>Distributed transaction recovery</td>
</tr>
<tr>
<td><a href="txncheckpoint.md" class="xref" title="DB_ENV-&gt;txn_checkpoint()">DB_ENV-&gt;txn_checkpoint()</a></td>
<td>Checkpoint the transaction subsystem</td>
</tr>
<tr>
<td><a href="txnstat.md" class="xref" title="DB_ENV-&gt;txn_stat()">DB_ENV-&gt;txn_stat()</a></td>
<td>Return transaction subsystem statistics</td>
</tr>
<tr>
<td><a href="txnstat_print.md" class="xref" title="DB_ENV-&gt;txn_stat_print()">DB_ENV-&gt;txn_stat_print()</a></td>
<td>Print transaction subsystem statistics</td>
</tr>
<tr>
<td><a href="txnset_timeout.md" class="xref" title="DB_TXN-&gt;set_timeout()">DB_TXN-&gt;set_timeout()</a></td>
<td>Set transaction timeout</td>
</tr>
<tr>
<td colspan="2"><strong>Transaction Subsystem Configuration</strong></td>
</tr>
<tr>
<td><a href="envset_timeout.md" class="xref" title="DB_ENV-&gt;set_timeout()">DB_ENV-&gt;set_timeout()</a>, <a href="envget_timeout.md" class="xref" title="DB_ENV-&gt;get_timeout()">DB_ENV-&gt;get_timeout()</a></td>
<td>Set/get lock and transaction timeout</td>
</tr>
<tr>
<td><a href="dbget_transactional.md" class="xref" title="DB-&gt;get_transactional()">DB-&gt;get_transactional()</a></td>
<td>Does the DB have transaction support</td>
</tr>
<tr>
<td><a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a></td>
<td>Get a locker ID in Berkeley DB Concurrent Data Store</td>
</tr>
<tr>
<td><a href="envset_tx_max.md" class="xref" title="DB_ENV-&gt;set_tx_max()">DB_ENV-&gt;set_tx_max()</a>, <a href="envget_tx_max.md" class="xref" title="DB_ENV-&gt;get_tx_max()">DB_ENV-&gt;get_tx_max()</a></td>
<td>Set/get maximum number of transactions</td>
</tr>
<tr>
<td><a href="envset_tx_timestamp.md" class="xref" title="DB_ENV-&gt;set_tx_timestamp()">DB_ENV-&gt;set_tx_timestamp()</a>, <a href="envget_tx_timestamp.md" class="xref" title="DB_ENV-&gt;get_tx_timestamp()">DB_ENV-&gt;get_tx_timestamp()</a></td>
<td>Set/get recovery timestamp</td>
</tr>
<tr>
<td colspan="2"><strong>Transaction Operations</strong></td>
</tr>
<tr>
<td><a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a></td>
<td>Begin a transaction</td>
</tr>
<tr>
<td><a href="txnabort.md" class="xref" title="DB_TXN-&gt;abort()">DB_TXN-&gt;abort()</a></td>
<td>Abort a transaction</td>
</tr>
<tr>
<td><a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a></td>
<td>Commit a transaction</td>
</tr>
<tr>
<td><a href="txndiscard.md" class="xref" title="DB_TXN-&gt;discard()">DB_TXN-&gt;discard()</a></td>
<td>Discard a prepared but not resolved transaction handle</td>
</tr>
<tr>
<td><a href="txnid.md" class="xref" title="DB_TXN-&gt;id()">DB_TXN-&gt;id()</a></td>
<td>Return a transaction's ID</td>
</tr>
<tr>
<td><a href="txnprepare.md" class="xref" title="DB_TXN-&gt;prepare()">DB_TXN-&gt;prepare()</a></td>
<td>Prepare a transaction for commit</td>
</tr>
<tr>
<td><a href="txnset_name.md" class="xref" title="DB_TXN-&gt;set_name()">DB_TXN-&gt;set_name()</a>, <a href="txnget_name.md" class="xref" title="DB_TXN-&gt;get_name()">DB_TXN-&gt;get_name()</a></td>
<td>Associate a string with a transaction</td>
</tr>
<tr>
<td><a href="txnset_priority.md" class="xref" title="DB_TXN-&gt;set_priority()">DB_TXN-&gt;set_priority()</a>, <a href="txnget_priority.md" class="xref" title="DB_TXN-&gt;get_priority()">DB_TXN-&gt;get_priority()</a></td>
<td>Set/get transaction's priority</td>
</tr>
</tbody>
</table>
