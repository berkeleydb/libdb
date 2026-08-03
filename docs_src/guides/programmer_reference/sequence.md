---
title: "Chapter 20.  Sequences"
api-name: "Chapter 20.  Sequences"
source: docs/programmer_reference/sequence.html
---
## Chapter 20.  Sequences

Sequences provide an arbitrary number of persistent objects that return an increasing or decreasing sequence of integers. Opening a sequence handle associates it with a record in a database. The handle can maintain a cache of values from the database so that a database update is not needed as the application allocates a value.

A sequence is stored as a record pair in a database. The database may be of any type, but may not have been configured to support duplicate data items. The sequence is referenced by the key used when the sequence is created, therefore the key must be compatible with the underlying access method. If the database stores fixed-length records, the record size must be at least 64 bytes long.

Since a sequence handle is opened using a database handle, the use of transactions with the sequence must follow how the database handle was opened. In other words, if the database handle was opened within a transaction, operations on the sequence handle must use transactions. Of course, if sequences are cached, not all operations will actually trigger a transaction.

For the highest concurrency, caching should be used and the <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> and <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a> flags should be specified to the <a href="../../api/c/seqget.md" class="olink">DB_SEQUENCE-&gt;get()</a> method call. If the allocation of the sequence value must be part of a transaction, and rolled back if the transaction aborts, then no caching should be specified and the transaction handle must be passed to the <a href="../../api/c/seqget.md" class="olink">DB_SEQUENCE-&gt;get()</a> method.

For more information on the operations supported by the sequence handle, see the <a href="../../api/c/seq.md#seqlist" class="olink">Sequences and Related Methods</a> section in the *Berkeley DB C API Reference Guide.*
