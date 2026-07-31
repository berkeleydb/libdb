---
title: "Chapter 3. Berkeley DB Features"
api-name: "Chapter 3. Berkeley DB Features"
source: docs/bdb-sql/dbfeatures.html
---
## Chapter 3. Berkeley DB Features

**Table of Contents**

<span class="sect1"> [Using Bulk Loading](dbfeatures.md#bulkloading) </span>

<span class="sect1"> [Using Multiversion Concurrency Control](mvcc.md) </span>

<span class="sect1"> [Selecting the Page Size](selectpage_size.md) </span>

When using the Berkeley DB SQL API, there are features that you can manage using PRAGMAs that are unique to Berkeley DB. This chapter discusses these features.

## Using Bulk Loading

Bulk loading is an I/O optimization feature that is useful when loading large amounts of data to the database inside of a single transaction. The bulk load optimization avoids writing some log records to the journal file. This can provide significant performance benefits when loading a large number of new rows into a database.

When you use bulk load, nested transactions are disabled. This means that you cannot undo any single operation. Instead, if you want to undo a given operation, you must undo everything performed within the transaction.

It is not possible to use the bulk load functionality when replication is enabled.

To enable bulk loading, use:

**PRAGMA TXN_BULK** = 0 \| 1;

Default value is `0`, which means the PRAGMA is turned off and so bulk loading is not in use. `1` means bulk loading is in use.
