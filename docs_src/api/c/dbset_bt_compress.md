---
title: "DB->set_bt_compress()"
api-name: "DB->set_bt_compress()"
source: docs/api_reference/C/dbset_bt_compress.html
---
## DB-\>set_bt_compress()

``` c
#include <db.h>

int
DB->set_bt_compress(DB *db,
    int (*bt_compress_fcn)(DB *db, const DBT *prevKey, 
        const DBT *prevData, const DBT *key, const DBT *data, DBT *dest),
    int (*bt_decompress_fcn)(DB *db, const DBT *prevKey, 
        const DBT *prevData, DBT *compressed, DBT *destKey, 
        DBT *destData));  
```

Set the Btree compression and decompression functions. The compression function is called whenever a key/data pair is added to the tree and the decompression function is called whenever data is requested from the tree.

This method is only compatible with prefix-based compression routines. This callback is mostly intended for compressing keys. From a performance perspective, it is better to perform compression of the data portion of your records outside of the Berkeley DB library.

If NULL function pointers are specified, then default compression and decompression functions are used. Berkeley DB's default compression function performs prefix compression on all keys and prefix compression on data values for duplicate keys. If using default compression, both the default compression and decompression functions must be used.

The `DB->set_bt_compress()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_bt_compress()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_bt_compress()` must be the same as that historically used to create the database or corruption can occur.

The `DB->set_bt_compress()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### bt_compress_fcn

The **bt_compress_fcn** function is the application-specified Btree compression function. The compression function takes six parameters:

- `db`

  The **db** parameter is the enclosing database handle.

- `prevKey`

  The **prevKey** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the key immediately preceding the application supplied key.

- `prevData`

  The **prevData** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the data associated with **prevKey**.

- `key`

  The **key** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the application supplied key.

- `data`

  The **data** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the application supplied data.

- `dest`

  The **dest** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the data stored in the tree, where the function should write the compressed data.

The **bt_compress_fcn** function must return 0 on success and a non-zero value on failure. If the compressed data cannot fit in **dest-\>data** (the size of which is stored in **dest-\>ulen**), the function should identify the required buffer size in **dest-\>size** and return `DB_BUFFER_SMALL`.

#### bt_decompress_fcn

The **bt_decompress_fcn** function is the application-specified Btree decompression function. The decompression function takes six parameters:

- `db`

  The **db** parameter is the enclosing database handle.

- `prevKey`

  The **prevKey** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the key immediately preceding the key being decompressed.

- `prevData`

  The **prevData** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the data associated with **prevKey**.

- `compressed`

  The **compressed** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the data stored in the tree, that is, the compressed data.

- `destKey`

  The **key** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> where the decompression function should store the decompressed key.

- `destData`

  The **data** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> where the decompression function should store the decompressed key.

The **bt_decompress_fcn** function must return 0 on success and a non-zero value on failure. If the decompressed data cannot fit in **key-\>data** or **data-\>data** (the size of which is available in the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>'s **ulen** field), the function should identify the required buffer size using the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>'s **size** field and return `DB_BUFFER_SMALL`.

### Errors

The `DB->set_bt_compress()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
