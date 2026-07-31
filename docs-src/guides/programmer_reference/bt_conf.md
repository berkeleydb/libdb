---
title: "Btree access method specific configuration"
api-name: "Btree access method specific configuration"
source: docs/programmer_reference/bt_conf.html
---
## Btree access method specific configuration

<span class="sect2"> [Btree comparison](bt_conf.md#am_conf_bt_compare) </span>

<span class="sect2"> [Btree prefix comparison](bt_conf.md#am_conf_bt_prefix) </span>

<span class="sect2"> [Minimum keys per page](bt_conf.md#am_conf_bt_minkey) </span>

<span class="sect2"> [Retrieving Btree records by logical record number](bt_conf.md#am_conf_bt_recnum) </span>

<span class="sect2"> [Compression](bt_conf.md#am_conf_bt_compress) </span>

There are a series of configuration tasks which you can perform when using the Btree access method. They are described in the following sections.

### Btree comparison

The Btree data structure is a sorted, balanced tree structure storing associated key/data pairs. By default, the sort order is lexicographical, with shorter keys collating before longer keys. The user can specify the sort order for the Btree by using the <a href="../../api/c/dbset_bt_compare.md" class="olink">DB-&gt;set_bt_compare()</a> method.

Sort routines are passed pointers to keys as arguments. The keys are represented as <a href="../../api/c/dbt.md" class="olink">DBT</a> structures. The routine must return an integer less than, equal to, or greater than zero if the first argument is considered to be respectively less than, equal to, or greater than the second argument. The only fields that the routines may examine in the <a href="../../api/c/dbt.md" class="olink">DBT</a> structures are **data** and **size** fields.

An example routine that might be used to sort integer keys in the database is as follows:

``` c
int
compare_int(DB *dbp, const DBT *a, const DBT *b)
    
{
    int ai, bi;
    /*
     * Returns:
     *    < 0 if a < b
     *    = 0 if a = b
     *    > 0 if a > b
     */
    memcpy(&ai, a->data, sizeof(int));
    memcpy(&bi, b->data, sizeof(int));
    return (ai - bi);
}
```

Note that the data must first be copied into memory that is appropriately aligned, as Berkeley DB does not guarantee any kind of alignment of the underlying data, including for comparison routines. When writing comparison routines, remember that databases created on machines of different architectures may have different integer byte orders, for which your code may need to compensate.

An example routine that might be used to sort keys based on the first five bytes of the key (ignoring any subsequent bytes) is as follows:

``` c
int
compare_dbt(DB *dbp, const DBT *a, const DBT *b)
    
{
    int len;
    u_char *p1, *p2;

    /*
     * Returns:
     *    < 0 if a < b
     *    = 0 if a = b
     *    > 0 if a > b
     */
    for (p1 = a->data, p2 = b->data, len = 5; len--; ++p1, ++p2)
        if (*p1 != *p2)
            return ((long)*p1 - (long)*p2);
    return (0);
}
```

All comparison functions must cause the keys in the database to be well-ordered. The most important implication of being well-ordered is that the key relations must be transitive, that is, if key A is less than key B, and key B is less than key C, then the comparison routine must also return that key A is less than key C.

It is reasonable for a comparison function to not examine an entire key in some applications, which implies partial keys may be specified to the Berkeley DB interfaces. When partial keys are specified to Berkeley DB, interfaces which retrieve data items based on a user-specified key (for example, <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> and <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> with the <a href="../../api/c/dbcget.md#dbcget_DB_SET" class="olink">DB_SET</a> flag), will modify the user-specified key by returning the actual key stored in the database.

### Btree prefix comparison

The Berkeley DB Btree implementation maximizes the number of keys that can be stored on an internal page by storing only as many bytes of each key as are necessary to distinguish it from adjacent keys. The prefix comparison routine is what determines this minimum number of bytes (that is, the length of the unique prefix), that must be stored. A prefix comparison function for the Btree can be specified by calling <a href="../../api/c/dbset_bt_prefix.md" class="olink">DB-&gt;set_bt_prefix()</a>.

The prefix comparison routine must be compatible with the overall comparison function of the Btree, since what distinguishes any two keys depends entirely on the function used to compare them. This means that if a prefix comparison routine is specified by the application, a compatible overall comparison routine must also have been specified.

Prefix comparison routines are passed pointers to keys as arguments. The keys are represented as <a href="../../api/c/dbt.md" class="olink">DBT</a> structures. The only fields the routines may examine in the <a href="../../api/c/dbt.md" class="olink">DBT</a> structures are **data** and **size** fields.

The prefix comparison function must return the number of bytes necessary to distinguish the two keys. If the keys are identical (equal and equal in length), the length should be returned. If the keys are equal up to the smaller of the two lengths, then the length of the smaller key plus 1 should be returned.

An example prefix comparison routine follows:

``` c
size_t
compare_prefix(DB *dbp, const DBT *a, const DBT *b)
   
{
    size_t cnt, len;
    u_int8_t *p1, *p2;

    cnt = 1;
    len = a->size > b->size ? b->size : a->size;
    for (p1 =
        a->data, p2 = b->data; len--; ++p1, ++p2, ++cnt)
            if (*p1 != *p2)
                return (cnt);
    /*
     * They match up to the smaller of the two sizes.
     * Collate the longer after the shorter.
     */
    if (a->size < b->size)
        return (a->size + 1);
    if (b->size < a->size)
        return (b->size + 1);
    return (b->size);
}
```

The usefulness of this functionality is data-dependent, but in some data sets can produce significantly reduced tree sizes and faster search times.

### Minimum keys per page

The number of keys stored on each page affects the size of a Btree and how it is maintained. Therefore, it also affects the retrieval and search performance of the tree. For each Btree, Berkeley DB computes a maximum key and data size. This size is a function of the page size and the fact that at least two key/data pairs must fit on any Btree page. Whenever key or data items exceed the calculated size, they are stored on overflow pages instead of in the standard Btree leaf pages.

Applications may use the <a href="../../api/c/dbset_bt_minkey.md" class="olink">DB-&gt;set_bt_minkey()</a> method to change the minimum number of keys that must fit on a Btree page from two to another value. Altering this value in turn alters the on-page maximum size, and can be used to force key and data items which would normally be stored in the Btree leaf pages onto overflow pages.

Some data sets can benefit from this tuning. For example, consider an application using large page sizes, with a data set almost entirely consisting of small key and data items, but with a few large items. By setting the minimum number of keys that must fit on a page, the application can force the outsized items to be stored on overflow pages. That in turn can potentially keep the tree more compact, that is, with fewer internal levels to traverse during searches.

The following calculation is similar to the one performed by the Btree implementation. (The **minimum_keys** value is multiplied by 2 because each key/data pair requires 2 slots on a Btree page.)

``` c
maximum_size = page_size / (minimum_keys * 2)
```

Using this calculation, if the page size is 8KB and the default **minimum_keys** value of 2 is used, then any key or data items larger than 2KB will be forced to an overflow page. If an application were to specify a **minimum_key** value of 100, then any key or data items larger than roughly 40 bytes would be forced to overflow pages.

It is important to remember that accesses to overflow pages do not perform as well as accesses to the standard Btree leaf pages, and so setting the value incorrectly can result in overusing overflow pages and decreasing the application's overall performance.

### Retrieving Btree records by logical record number

The Btree access method optionally supports retrieval by logical record numbers. To configure a Btree to support record numbers, call the <a href="../../api/c/dbset_flags.md" class="olink">DB-&gt;set_flags()</a> method with the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RECNUM" class="olink">DB_RECNUM</a> flag.

Configuring a Btree for record numbers should not be done lightly. While often useful, it may significantly slow down the speed at which items can be stored into the database, and can severely impact application throughput. Generally it should be avoided in trees with a need for high write concurrency.

To retrieve by record number, use the <a href="../../api/c/dbget.md#dbget_DB_SET_RECNO" class="olink">DB_SET_RECNO</a> flag to the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> and <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> methods. The following is an example of a routine that displays the data item for a Btree database created with the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RECNUM" class="olink">DB_RECNUM</a> option.

``` c
int
rec_display(DB *dbp, db_recno_t recno)
    
{
    DBT key, data;
    int ret;

    memset(&key, 0, sizeof(key));
    key.data = &recno;
    key.size = sizeof(recno);
    memset(&data, 0, sizeof(data));

    if ((ret = dbp->get(dbp, NULL, &key, &data, DB_SET_RECNO)) != 0)
        return (ret);
    printf("data for %lu: %.*s\n",
        (u_long)recno, (int)data.size, (char *)data.data);
    return (0);
}
```

To determine a key's record number, use the <a href="../../api/c/dbcget.md#dbcget_DB_GET_RECNO" class="olink">DB_GET_RECNO</a> flag to the <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> method. The following is an example of a routine that displays the record number associated with a specific key.

``` c
int
recno_display(DB *dbp, char *keyvalue)
    
{
    DBC *dbcp;
    DBT key, data;
    db_recno_t recno;
    int ret, t_ret;

    /* Acquire a cursor for the database. */
    if ((ret = dbp->cursor(dbp, NULL, &dbcp, 0)) != 0) {
        dbp->err(dbp, ret, "DB->cursor");
        goto err;
    }

    /* Position the cursor. */
    memset(&key, 0, sizeof(key));
    key.data = keyvalue;
    key.size = strlen(keyvalue);
    memset(&data, 0, sizeof(data));
    if ((ret = dbcp->get(dbcp, &key, &data, DB_SET)) != 0) {
        dbp->err(dbp, ret, "DBC->get(DB_SET): %s", keyvalue);
        goto err;
    }

    /*
     * Request the record number, and store it into appropriately
     * sized and aligned local memory.
     */
    memset(&data, 0, sizeof(data));
    data.data = &recno;
    data.ulen = sizeof(recno);
    data.flags = DB_DBT_USERMEM;
    if ((ret = dbcp->get(dbcp, &key, &data, DB_GET_RECNO)) != 0) {
        dbp->err(dbp, ret, "DBC->get(DB_GET_RECNO)");
        goto err;
    }

    printf("key for requested key was %lu\n", (u_long)recno);

err:    /* Close the cursor. */
    if ((t_ret = dbcp->close(dbcp)) != 0) {
        if (ret == 0)
            ret = t_ret;
        dbp->err(dbp, ret, "DBC->close");
    }
    return (ret);
}
```

### Compression

The Btree access method supports the automatic compression of key/data pairs upon their insertion into the database. The key/data pairs are decompressed before they are returned to the application, making an application's interaction with a compressed database identical to that for a non-compressed database. To configure Berkeley DB for compression, call the <a href="../../api/c/dbset_bt_compress.md" class="olink">DB-&gt;set_bt_compress()</a> method and specify custom compression and decompression functions. If <a href="../../api/c/dbset_bt_compress.md" class="olink">DB-&gt;set_bt_compress()</a> is called with NULL compression and decompression functions, Berkeley DB will use its default compression functions.

### Note

Compression only works with the Btree access method, and then only so long as your database is not configured for unsorted duplicates.

### Note

The default compression function is not guaranteed to reduce the size of the on-disk database in every case. It has been tested and shown to work well with English-language text. Of course, in order to determine if the default compression algorithm is beneficial for your application, it is important to test both the final size and the performance using a representative set of data and access patterns.

The default compression function performs prefix compression on each key added to the database. This means that, for a key <span class="emphasis">*n*</span> bytes in length, the first <span class="emphasis">*i*</span> bytes that match the first <span class="emphasis">*i*</span> bytes of the previous key exactly are omitted and only the final <span class="emphasis">*n-i*</span> bytes are stored in the database. If the bytes of key being stored match the bytes of the previous key exactly, then the same prefix compression algorithm is applied to the data value being stored. To use Berkeley DB's default compression behavior, both the default compression and decompression functions must be used.

For example, to configure your database for default compression:

``` c
    DB *dbp = NULL;
    DB_ENV *envp = NULL;
     u_int32_t db_flags; 
    const char *file_name = "mydb.db"; 
    int ret;

...
    
    /* Skipping environment open to shorten this example */
    /* Initialize the DB handle */
    ret = db_create(&dbp, envp, 0);
    if (ret != 0) {
        fprintf(stderr, "%s\n", db_strerror(ret));
        return (EXIT_FAILURE);
    }

    /* Turn on default data compression */
    dbp->set_bt_compress(dbp, NULL, NULL);

    /* Now open the database */
    db_flags = DB_CREATE;       /* Allow database creation */

    ret = dbp->open(dbp,        /* Pointer to the database */
                    NULL,       /* Txn pointer */
                    file_name,  /* File name */
                    NULL,       /* Logical db name */
                    DB_BTREE,   /* Database type (using btree) */
                    db_flags,   /* Open flags */
                    0);         /* File mode. Using defaults */
    if (ret != 0) {
        dbp->err(dbp, ret, "Database '%s' open failed",
            file_name);
        return (EXIT_FAILURE);
    }
```

#### Custom compression

An application wishing to perform its own compression may supply a compression and decompression function which will be called instead of Berkeley DB's default functions. The compression function is passed five <a href="../../api/c/dbt.md" class="olink">DBT</a> structures:

- The key and data immediately preceeding the key/data pair that is being stored.

- The key and data being stored in the tree.

- The buffer where the compressed data should be written.

The total size of the buffer used to store the compressed data is identified in the <a href="../../api/c/dbt.md" class="olink">DBT</a>'s `ulen` field. If the compressed data cannot fit in the buffer, the compression function should store the amount of space needed in <a href="../../api/c/dbt.md" class="olink">DBT</a>'s `size` field and then return `DB_BUFFER_SMALL`. Berkeley DB will subsequently re-call the compression function with the required amount of space allocated in the compression data buffer.

Multiple compressed key/data pairs will likely be written to the same buffer and the compression function should take steps to ensure it does not overwrite data.

For example, the following code fragments illustrate the use of a custom compression routine. This code is actually a much simplified example of the default compression provided by Berkeley DB. It does simple prefix compression on the key part of the data.

``` c
    int compress(DB *dbp, const DBT *prevKey, const DBT *prevData, 
        const DBT *key, const DBT *data, DBT *dest)
{
    u_int8_t *dest_data_ptr;
    const u_int8_t *key_data, *prevKey_data;
    size_t len, prefix, suffix;

    key_data = (const u_int8_t*)key->data;
    prevKey_data = (const u_int8_t*)prevKey->data;
    len = key->size > prevKey->size ? prevKey->size : key->size;
    for (; len-- && *key_data == *prevKey_data; ++key_data, 
                                                ++prevKey_data)
        continue;

    prefix = (size_t)(key_data - (u_int8_t*)key->data);
    suffix = key->size - prefix;

    /* Check that we have enough space in dest */
    dest->size = (u_int32_t)(__db_compress_count_int(prefix) +
        __db_compress_count_int(suffix) +
        __db_compress_count_int(data->size) + suffix + data->size);
    if (dest->size > dest->ulen)
        return (DB_BUFFER_SMALL);

    /* prefix length */
    dest_data_ptr = (u_int8_t*)dest->data;
    dest_data_ptr += __db_compress_int(dest_data_ptr, prefix);

    /* suffix length */
    dest_data_ptr += __db_compress_int(dest_data_ptr, suffix);

    /* data length */
    dest_data_ptr += __db_compress_int(dest_data_ptr, data->size);

    /* suffix */
    memcpy(dest_data_ptr, key_data, suffix);
    dest_data_ptr += suffix;

    /* data */
    memcpy(dest_data_ptr, data->data, data->size);

    return (0);
} 
```

The corresponding decompression function is likewise passed five <a href="../../api/c/dbt.md" class="olink">DBT</a> structures:

- The key and data <a href="../../api/c/dbt.md" class="olink">DBT</a>s immediately preceding the decompressed key and data.

- The compressed data from the database.

- One to store the decompressed key and another one for the decompressed data.

Because the compression of `record X` relies upon `record X-1`, the decompression function can be called repeatedly to linearally decompress a set of records stored in the compressed buffer.

The total size of the buffer available to store the decompressed data is identified in the destination <a href="../../api/c/dbt.md" class="olink">DBT</a>'s `ulen` field. If the decompressed data cannot fit in the buffer, the decompression function should store the amount of space needed in the destination <a href="../../api/c/dbt.md" class="olink">DBT</a>'s `size` field and then return `DB_BUFFER_SMALL`. Berkeley DB will subsequently re-call the decompression function with the required amount of space allocated in the decompression data buffer.

For example, the decompression routine that corresponds to the example compression routine provided above is:

``` c
int decompress(DB *dbp, const DBT *prevKey, const DBT *prevData, 
    DBT *compressed, DBT *destKey, DBT *destData)
{
    u_int8_t *comp_data, *dest_data;
    u_int32_t prefix, suffix, size;

    /* Unmarshal prefix, suffix and data length */
    comp_data = (u_int8_t*)compressed->data;
    size = __db_decompress_count_int(comp_data);
    if (size > compressed->size)
        return (EINVAL);
    comp_data += __db_decompress_int32(comp_data, &prefix);

    size += __db_decompress_count_int(comp_data);
    if (size > compressed->size)
        return (EINVAL);
    comp_data += __db_decompress_int32(comp_data, &suffix);

    size += __db_decompress_count_int(comp_data);
    if (size > compressed->size)
        return (EINVAL);
    comp_data += __db_decompress_int32(comp_data, &destData->size);

    /* Check destination lengths */
    destKey->size = prefix + suffix;
    if (destKey->size > destKey->ulen || 
            destData->size > destData->ulen)
        return (DB_BUFFER_SMALL);

    /* Write the prefix */
    if (prefix > prevKey->size)
        return (EINVAL);
    dest_data = (u_int8_t*)destKey->data;
    memcpy(dest_data, prevKey->data, prefix);
    dest_data += prefix;

    /* Write the suffix */
    size += suffix;
    if (size > compressed->size)
        return (EINVAL);
    memcpy(dest_data, comp_data, suffix);
    comp_data += suffix;

    /* Write the data */
    size += destData->size;
    if (size > compressed->size)
        return (EINVAL);
    memcpy(destData->data, comp_data, destData->size);
    comp_data += destData->size;

    /* Return bytes read */
    compressed->size = 
        (u_int32_t)(comp_data - (u_int8_t*)compressed->data);
    return (0);
} 
```

#### Programmer Notes

As you use compression with your databases, be aware of the following:

- Compression works by placing key/data pairs from a single database page into a single block of compressed data. This is true whether you use DB's default compression, or you write your own compression. Because all of key/data data is placed in a single block of memory, you cannot decompress data unless you have decompressed everything that came before it in the block. That is, you cannot decompress item <span class="emphasis">*n*</span> in the data block, unless you also decompress items <span class="emphasis">*0*</span> through <span class="emphasis">*n-1*</span>.

- If you increase the minimum number of key/data pairs placed on a Btree leaf page (using <a href="../../api/c/dbset_bt_minkey.md" class="olink">DB-&gt;set_bt_minkey()</a>), you will decrease your seek times on a compressed database. However, this will also decrease the effectiveness of the compression.

- Compressed databases are fastest if bulk load is used to add data to them. See <a href="am_misc_bulk.md" class="xref" title="Retrieving and updating records in bulk">Retrieving and updating records in bulk</a> for information on using bulk load.
