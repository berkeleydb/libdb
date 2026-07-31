---
title: "txn_XXX"
api-name: "txn_XXX"
source: docs/upgrading/upgrade_4_0_txn.html
---
## txn_XXX

The C API for the Berkeley DB Transaction subsystem was reworked in the 4.0 release as follows:

| Historic functional interface | Berkeley DB 4.X method |
|----|----|
| txn_abort | <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a> |
| txn_begin | <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a> |
| txn_checkpoint | <a href="../../api/c/txncheckpoint.md" class="olink">DB_ENV-&gt;txn_checkpoint()</a> |
| txn_commit | <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a> |
| txn_discard | <a href="../../api/c/txndiscard.md" class="olink">DB_TXN-&gt;discard()</a> |
| txn_id | <a href="../../api/c/txnid.md" class="olink">DB_TXN-&gt;id()</a> |
| txn_prepare | <a href="../../api/c/txnprepare.md" class="olink">DB_TXN-&gt;prepare()</a> |
| txn_recover | <a href="../../api/c/txnrecover.md" class="olink">DB_TXN-&gt;recover()</a> |
| txn_stat | <a href="../../api/c/txnstat.md" class="olink">DB_TXN-&gt;stat()</a> |

Applications calling any of these functions should update their calls to use the enclosing <a href="../../api/c/env.md" class="olink">DB_ENV class</a> handle's method (easily done as the first argument to the existing call is the correct handle to use).

As a special case, since applications might potentially have many calls to the txn_abort, txn_begin and txn_commit functions, those functions continue to work unchanged in the Berkeley DB 4.0 release.

In addition, the <a href="../../api/c/txnstat.md" class="olink">DB_TXN-&gt;stat()</a> call has been changed in the 4.0 release to take a flags argument. To leave their historic behavior unchanged, applications should add a final argument of 0 to any calls made to <a href="../../api/c/txnstat.md" class="olink">DB_TXN-&gt;stat()</a>.
