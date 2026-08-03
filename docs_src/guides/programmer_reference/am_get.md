---
title: "Retrieving records"
api-name: "Retrieving records"
source: docs/programmer_reference/am_get.html
---
## Retrieving records

The <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> method retrieves records from the database. In general, <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> takes a key and returns the associated data from the database.

There are a few flags that you can set to customize retrieval:

<span class="term"> <a href="../../api/c/dbget.md#get_DB_GET_BOTH" class="olink">DB_GET_BOTH</a> </span>  
Search for a matching key and data item, that is, only return success if both the key and the data items match those stored in the database.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_RMW" class="olink">DB_RMW</a> </span>  
Read-modify-write: acquire write locks instead of read locks during retrieval. This can enhance performance in threaded applications by reducing the chance of deadlock.

<span class="term"> <a href="../../api/c/dbget.md#dbget_DB_SET_RECNO" class="olink">DB_SET_RECNO</a> </span>  
If the underlying database is a Btree, and was configured so that it is possible to search it by logical record number, retrieve a specific record.

If the database has been configured to support duplicate records, <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> will always return the first data item in the duplicate set.
