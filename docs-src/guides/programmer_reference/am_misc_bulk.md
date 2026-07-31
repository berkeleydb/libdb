---
title: "Retrieving and updating records in bulk"
api-name: "Retrieving and updating records in bulk"
source: docs/programmer_reference/am_misc_bulk.html
---
## Retrieving and updating records in bulk

<span class="sect2"> [Bulk retrieval](am_misc_bulk.md#am_misc_bulk_get) </span>

<span class="sect2"> [Bulk updates](am_misc_bulk.md#am_misc_bulk_put) </span>

<span class="sect2"> [Bulk deletes](am_misc_bulk.md#am_misc_bulk_del) </span>

When retrieving or modifying large numbers of records, the number of method calls can often dominate performance. Berkeley DB offers bulk get, put and delete interfaces which can significantly increase performance for some applications.

### Bulk retrieval

To retrieve records in bulk, an application buffer must be specified to the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> methods. This is done in the C API by setting the **data** and **ulen** fields of the **data** <a href="../../api/c/dbt.md" class="olink">DBT</a> to reference an application buffer, and the **flags** field of that structure to <a href="../../api/c/dbt.md#dbt_DB_DBT_USERMEM" class="olink">DB_DBT_USERMEM</a>. In the Berkeley DB C++ and Java APIs, the actions are similar, although there are API-specific methods to set the <a href="../../api/c/dbt.md" class="olink">DBT</a> values. Then, the <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE" class="olink">DB_MULTIPLE</a> or <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE_KEY" class="olink">DB_MULTIPLE_KEY</a> flags are specified to the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> methods, which cause multiple records to be returned in the specified buffer.

The difference between <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE" class="olink">DB_MULTIPLE</a> and <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE_KEY" class="olink">DB_MULTIPLE_KEY</a> is as follows: <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE" class="olink">DB_MULTIPLE</a> returns multiple data items for a single key. For example, the <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE" class="olink">DB_MULTIPLE</a> flag would be used to retrieve all of the duplicate data items for a single key in a single call. The <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE_KEY" class="olink">DB_MULTIPLE_KEY</a> flag is used to retrieve multiple key/data pairs, where each returned key may or may not have duplicate data items.

Once the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> method has returned, the application will walk through the buffer handling the returned records. This is implemented for the C and C++ APIs using four macros: <a href="../../api/c/DB_MULTIPLE_INIT.md" class="olink">DB_MULTIPLE_INIT</a>, <a href="../../api/c/DB_MULTIPLE_NEXT.md" class="olink">DB_MULTIPLE_NEXT</a>, <a href="../../api/c/DB_MULTIPLE_KEY_NEXT.md" class="olink">DB_MULTIPLE_KEY_NEXT</a>, and <a href="../../api/c/DB_MULTIPLE_RECNO_NEXT.md" class="olink">DB_MULTIPLE_RECNO_NEXT</a>. For the Java API, this is implemented as three iterator classes: <a href="../java/com/sleepycat/db/MultipleDataEntry.html" class="ulink" target="_top">MultipleDataEntry</a>, <a href="../java/com/sleepycat/db/MultipleKeyDataEntry.html" class="ulink" target="_top">MultipleKeyDataEntry</a>, and <a href="../java/com/sleepycat/db/MultipleRecnoDataEntry.html" class="ulink" target="_top">MultipleRecnoDataEntry</a>.

The <a href="../../api/c/DB_MULTIPLE_INIT.md" class="olink">DB_MULTIPLE_INIT</a> macro is always called first. It initializes a local application variable and the **data** <a href="../../api/c/dbt.md" class="olink">DBT</a> for stepping through the set of returned records. Then, the application calls one of the remaining three macros: <a href="../../api/c/DB_MULTIPLE_NEXT.md" class="olink">DB_MULTIPLE_NEXT</a>, <a href="../../api/c/DB_MULTIPLE_KEY_NEXT.md" class="olink">DB_MULTIPLE_KEY_NEXT</a>, and <a href="../../api/c/DB_MULTIPLE_RECNO_NEXT.md" class="olink">DB_MULTIPLE_RECNO_NEXT</a>.

If the <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE" class="olink">DB_MULTIPLE</a> flag was specified to the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> method, the application will always call the <a href="../../api/c/DB_MULTIPLE_NEXT.md" class="olink">DB_MULTIPLE_NEXT</a> macro. If the <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE_KEY" class="olink">DB_MULTIPLE_KEY</a> flag was specified to the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> method, and the underlying database is a Btree or Hash database, the application will always call the <a href="../../api/c/DB_MULTIPLE_KEY_NEXT.md" class="olink">DB_MULTIPLE_KEY_NEXT</a> macro. If the <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE_KEY" class="olink">DB_MULTIPLE_KEY</a> flag was specified to the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> method, and the underlying database is a Queue or Recno database, the application will always call the <a href="../../api/c/DB_MULTIPLE_RECNO_NEXT.md" class="olink">DB_MULTIPLE_RECNO_NEXT</a> macro. The <a href="../../api/c/DB_MULTIPLE_NEXT.md" class="olink">DB_MULTIPLE_NEXT</a>, <a href="../../api/c/DB_MULTIPLE_KEY_NEXT.md" class="olink">DB_MULTIPLE_KEY_NEXT</a>, and <a href="../../api/c/DB_MULTIPLE_RECNO_NEXT.md" class="olink">DB_MULTIPLE_RECNO_NEXT</a> macros are called repeatedly, until the end of the returned records is reached. The end of the returned records is detected by the application's local pointer variable being set to NULL.

Note that if you want to use a cursor for bulk retrieval of records in a Btree database, you should open the cursor using the `DB_CURSOR_BULK` flag. This optimizes the cursor for bulk retrieval.

The following is an example of a routine that displays the contents of a Btree database using the bulk return interfaces.

``` c
int
rec_display(DB *dbp)
{
    DBC *dbcp;
    DBT key, data;
    size_t retklen, retdlen;
    void *retkey, *retdata;
    int ret, t_ret;
    void *p;

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    /* Review the database in 5MB chunks. */
#define    BUFFER_LENGTH    (5 * 1024 * 1024)
    if ((data.data = malloc(BUFFER_LENGTH)) == NULL)
        return (errno);
    data.ulen = BUFFER_LENGTH;
    data.flags = DB_DBT_USERMEM;

    /* Acquire a cursor for the database. */
    if ((ret = dbp->cursor(dbp, NULL, &dbcp, DB_CURSOR_BULK)) 
        != 0) {
            dbp->err(dbp, ret, "DB->cursor");
            free(data.data);
            return (ret);
    }

    for (;;) {
        /*
         * Acquire the next set of key/data pairs.  This code 
         * does not handle single key/data pairs that won't fit 
         * in a BUFFER_LENGTH size buffer, instead returning 
         * DB_BUFFER_SMALL to our caller.
         */
        if ((ret = dbcp->get(dbcp,
            &key, &data, DB_MULTIPLE_KEY | DB_NEXT)) != 0) {
            if (ret != DB_NOTFOUND)
                dbp->err(dbp, ret, "DBcursor->get");
            break;
        }

        for (DB_MULTIPLE_INIT(p, &data);;) {
            DB_MULTIPLE_KEY_NEXT(p,
                &data, retkey, retklen, retdata, retdlen);
            if (p == NULL)
                break;
            printf("key: %.*s, data: %.*s\n",
                (int)retklen, (char *)retkey, (int)retdlen, 
                (char *)retdata);
        }
    }

    if ((t_ret = dbcp->close(dbcp)) != 0) {
        dbp->err(dbp, ret, "DBcursor->close");
        if (ret == 0)
            ret = t_ret;
    }

    free(data.data);

    return (ret);
}
```

### Bulk updates

To put records in bulk with the btree or hash access methods, construct bulk buffers in the **key** and **data** <a href="../../api/c/dbt.md" class="olink">DBT</a> using <a href="../../api/c/DB_MULTIPLE_WRITE_INIT.md" class="olink">DB_MULTIPLE_WRITE_INIT</a> and <a href="../../api/c/DB_MULTIPLE_WRITE_NEXT.md" class="olink">DB_MULTIPLE_WRITE_NEXT</a>. To put records in bulk with the recno or queue access methods, construct bulk buffers in the **data** <a href="../../api/c/dbt.md" class="olink">DBT</a> as before, but construct the **key** <a href="../../api/c/dbt.md" class="olink">DBT</a> using <a href="../../api/c/DB_MULTIPLE_RECNO_WRITE_INIT.md" class="olink">DB_MULTIPLE_RECNO_WRITE_INIT</a> and <a href="../../api/c/DB_MULTIPLE_RECNO_WRITE_NEXT.md" class="olink">DB_MULTIPLE_RECNO_WRITE_NEXT</a> with a data size of zero;. In both cases, set the <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE" class="olink">DB_MULTIPLE</a> flag to <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a>.

Alternatively, for btree and hash access methods, construct a single bulk buffer in the **key** <a href="../../api/c/dbt.md" class="olink">DBT</a> using <a href="../../api/c/DB_MULTIPLE_WRITE_INIT.md" class="olink">DB_MULTIPLE_WRITE_INIT</a> and <a href="../../api/c/DB_MULTIPLE_KEY_WRITE_NEXT.md" class="olink">DB_MULTIPLE_KEY_WRITE_NEXT</a>. For recno and queue access methods, construct a bulk buffer in the **key** <a href="../../api/c/dbt.md" class="olink">DBT</a> using <a href="../../api/c/DB_MULTIPLE_RECNO_WRITE_INIT.md" class="olink">DB_MULTIPLE_RECNO_WRITE_INIT</a> and <a href="../../api/c/DB_MULTIPLE_RECNO_WRITE_NEXT.md" class="olink">DB_MULTIPLE_RECNO_WRITE_NEXT</a>. In both cases, set the <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE_KEY" class="olink">DB_MULTIPLE_KEY</a> flag to <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a>.

A successful bulk operation is logically equivalent to a loop through each key/data pair, performing a <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> for each one.

### Bulk deletes

To delete all records with a specified set of keys with the btree or hash access methods, construct a bulk buffer in the **key** <a href="../../api/c/dbt.md" class="olink">DBT</a> using <a href="../../api/c/DB_MULTIPLE_WRITE_INIT.md" class="olink">DB_MULTIPLE_WRITE_INIT</a> and <a href="../../api/c/DB_MULTIPLE_WRITE_NEXT.md" class="olink">DB_MULTIPLE_WRITE_NEXT</a>. To delete a set of records with the recno or queue access methods, construct the **key** <a href="../../api/c/dbt.md" class="olink">DBT</a> using <a href="../../api/c/DB_MULTIPLE_RECNO_WRITE_INIT.md" class="olink">DB_MULTIPLE_RECNO_WRITE_INIT</a> and <a href="../../api/c/DB_MULTIPLE_RECNO_WRITE_NEXT.md" class="olink">DB_MULTIPLE_RECNO_WRITE_NEXT</a> with a data size of zero. In both cases, set the <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE" class="olink">DB_MULTIPLE</a> flag to <a href="../../api/c/dbdel.md" class="olink">DB-&gt;del()</a>. This is equivalent to calling <a href="../../api/c/dbdel.md" class="olink">DB-&gt;del()</a> for each key in the bulk buffer. In particular, if the database supports duplicates, all records with the matching key are deleted.

Alternatively, to delete a specific set of key/data pairs, which may be items within a set of duplicates, there are also two cases depending on whether the access method uses record numbers for keys. For btree and hash access methods, construct a single bulk buffer in the **key** <a href="../../api/c/dbt.md" class="olink">DBT</a> using <a href="../../api/c/DB_MULTIPLE_WRITE_INIT.md" class="olink">DB_MULTIPLE_WRITE_INIT</a> and <a href="../../api/c/DB_MULTIPLE_KEY_WRITE_NEXT.md" class="olink">DB_MULTIPLE_KEY_WRITE_NEXT</a>. For recno and queue access methods, construct a bulk buffer in the **key** <a href="../../api/c/dbt.md" class="olink">DBT</a> using <a href="../../api/c/DB_MULTIPLE_RECNO_WRITE_INIT.md" class="olink">DB_MULTIPLE_RECNO_WRITE_INIT</a> and <a href="../../api/c/DB_MULTIPLE_RECNO_WRITE_NEXT.md" class="olink">DB_MULTIPLE_RECNO_WRITE_NEXT</a>. In both cases, set the <a href="../../api/c/dbcget.md#dbcget_DB_MULTIPLE_KEY" class="olink">DB_MULTIPLE_KEY</a> flag to <a href="../../api/c/dbdel.md" class="olink">DB-&gt;del()</a>.

A successful bulk operation is logically equivalent to a loop through each key/data pair, performing a <a href="../../api/c/dbdel.md" class="olink">DB-&gt;del()</a> for each one.
