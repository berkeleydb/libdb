---
title: "Cursor operations"
api-name: "Cursor operations"
source: docs/programmer_reference/am_cursor.html
---
## Cursor operations

<span class="sect2"> [Retrieving records with a cursor](am_cursor.md#am_curget) </span>

<span class="sect2"> [Storing records with a cursor](am_cursor.md#am_curput) </span>

<span class="sect2"> [Deleting records with a cursor](am_cursor.md#am_curdel) </span>

<span class="sect2"> [Duplicating a cursor](am_cursor.md#am_curdup) </span>

<span class="sect2"> [Equality Join](am_cursor.md#am_join) </span>

<span class="sect2"> [Data item count](am_cursor.md#am_count) </span>

<span class="sect2"> [Cursor close](am_cursor.md#am_curclose) </span>

A database cursor refers to a single key/data pair in the database. It supports traversal of the database and is the only way to access individual duplicate data items. Cursors are used for operating on collections of records, for iterating over a database, and for saving handles to individual records, so that they can be modified after they have been read.

The <a href="../../api/c/dbcursor.md" class="olink">DB-&gt;cursor()</a> method opens a cursor into a database. Upon return the cursor is uninitialized, cursor positioning occurs as part of the first cursor operation.

Once a database cursor has been opened, records may be retrieved (<a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a>), stored (<a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a>), and deleted (<a href="../../api/c/dbcdel.md" class="olink">DBC-&gt;del()</a>).

Additional operations supported by the cursor handle include duplication (<a href="../../api/c/dbcdup.md" class="olink">DBC-&gt;dup()</a>), equality join (<a href="../../api/c/dbjoin.md" class="olink">DB-&gt;join()</a>), and a count of duplicate data items (<a href="../../api/c/dbccount.md" class="olink">DBC-&gt;count()</a>). Cursors are eventually closed using <a href="../../api/c/dbcclose.md" class="olink">DBC-&gt;close()</a>.

For more information on the operations supported by the cursor handle, see the <a href="../../api/c/dbc.md#dbclist" class="olink">Database Cursors and Related Methods</a> section in the *Berkeley DB C API Reference Guide.*

### Retrieving records with a cursor

The <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> method retrieves records from the database using a cursor. The <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> method takes a flag which controls how the cursor is positioned within the database and returns the key/data item associated with that positioning. Similar to <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a>, <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> may also take a supplied key and retrieve the data associated with that key from the database. There are several flags that you can set to customize retrieval.

#### Cursor position flags

<span class="term"><a href="../../api/c/dbcget.md#dbcget_DB_FIRST" class="olink">DB_FIRST</a>, <a href="../../api/c/dbcget.md#dbcget_DB_LAST" class="olink">DB_LAST</a></span>  
Return the first (last) record in the database.

<span class="term"><a href="../../api/c/dbcget.md#dbcget_DB_NEXT" class="olink">DB_NEXT</a>, <a href="../../api/c/dbcget.md#dbcget_DB_PREV" class="olink">DB_PREV</a></span>  
Return the next (previous) record in the database.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_NEXT_DUP" class="olink">DB_NEXT_DUP</a> </span>  
Return the next record in the database, if it is a duplicate data item for the current key. For Heap databases, this flag always results in the cursor returning the `DB_NOTFOUND` error.

<span class="term"><a href="../../api/c/dbcget.md#dbcget_DB_NEXT_NODUP" class="olink">DB_NEXT_NODUP</a>, <a href="../../api/c/dbcget.md#dbcget_DB_PREV_NODUP" class="olink">DB_PREV_NODUP</a></span>  
Return the next (previous) record in the database that is not a duplicate data item for the current key.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_CURRENT" class="olink">DB_CURRENT</a> </span>  
Return the record from the database to which the cursor currently refers.

#### Retrieving specific key/data pairs

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_SET" class="olink">DB_SET</a> </span>  
Return the record from the database that matches the supplied key. In the case of duplicates the first duplicate is returned and the cursor is positioned at the beginning of the duplicate list. The user can then traverse the duplicate entries for the key.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_SET_RANGE" class="olink">DB_SET_RANGE</a> </span>  
Return the smallest record in the database greater than or equal to the supplied key. This functionality permits partial key matches and range searches in the Btree access method.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_GET_BOTH" class="olink">DB_GET_BOTH</a> </span>  
Return the record from the database that matches both the supplied key and data items. This is particularly useful when there are large numbers of duplicate records for a key, as it allows the cursor to easily be positioned at the correct place for traversal of some part of a large set of duplicate records.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_GET_BOTH_RANGE" class="olink">DB_GET_BOTH_RANGE</a> </span>  
If used on a database configured for sorted duplicates, this returns the smallest record in the database greater than or equal to the supplied key and data items. If used on a database that is <span class="emphasis">*not*</span> configured for sorted duplicates, this flag behaves identically to `DB_GET_BOTH`.

#### Retrieving based on record numbers

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_SET_RECNO" class="olink">DB_SET_RECNO</a> </span>  
If the underlying database is a Btree, and was configured so that it is possible to search it by logical record number, retrieve a specific record based on a record number argument.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_GET_RECNO" class="olink">DB_GET_RECNO</a> </span>  
If the underlying database is a Btree, and was configured so that it is possible to search it by logical record number, return the record number for the record to which the cursor refers.

#### Special-purpose flags

<span class="term"> <a href="../../api/c/dbget.md#dbget_DB_CONSUME" class="olink">DB_CONSUME</a> </span>  
Read-and-delete: the first record (the head) of the queue is returned and deleted. The underlying database must be a Queue.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_RMW" class="olink">DB_RMW</a> </span>  
Read-modify-write: acquire write locks instead of read locks during retrieval. This can enhance performance in threaded applications by reducing the chance of deadlock.

In all cases, the cursor is repositioned by a <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> operation to point to the newly-returned key/data pair in the database.

The following is a code example showing a cursor walking through a database and displaying the records it contains to the standard output:

``` c
int
display(char *database)
    
{
    DB *dbp;
    DBC *dbcp;
    DBT key, data;
    int close_db, close_dbc, ret;

    close_db = close_dbc = 0;

    /* Open the database. */
    if ((ret = db_create(&dbp, NULL, 0)) != 0) {
        fprintf(stderr,
            "%s: db_create: %s\n", progname, db_strerror(ret));
        return (1);
    }
    close_db = 1;

    /* Turn on additional error output. */
    dbp->set_errfile(dbp, stderr);
    dbp->set_errpfx(dbp, progname);

    /* Open the database. */
    if ((ret = dbp->open(dbp, NULL, database, NULL, 
            DB_UNKNOWN, DB_RDONLY, 0)) != 0) {
        dbp->err(dbp, ret, "%s: DB->open", database);
        goto err;
    }

    /* Acquire a cursor for the database. */
    if ((ret = dbp->cursor(dbp, NULL, &dbcp, 0)) != 0) {
        dbp->err(dbp, ret, "DB->cursor");
        goto err;
    }
    close_dbc = 1;

    /* Initialize the key/data return pair. */
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    /* Walk through the database and print out the key/data pairs. */
    while ((ret = dbcp->get(dbcp, &key, &data, DB_NEXT)) == 0)
        printf("%.*s : %.*s\n",
            (int)key.size, (char *)key.data,
            (int)data.size, (char *)data.data);
    if (ret != DB_NOTFOUND) {
        dbp->err(dbp, ret, "DBcursor->get");
        goto err;
    }

err:    if (close_dbc && (ret = dbcp->close(dbcp)) != 0)
        dbp->err(dbp, ret, "DBcursor->close");
    if (close_db && (ret = dbp->close(dbp, 0)) != 0)
        fprintf(stderr,
            "%s: DB->close: %s\n", progname, db_strerror(ret));
    return (0);
}
```

### Storing records with a cursor

The <a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a> method stores records into the database using a cursor. In general, <a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a> takes a key and inserts the associated data into the database, at a location controlled by a specified flag.

There are several flags that you can set to customize storage:

<span class="term"> <a href="../../api/c/dbcput.md#dbcput_DB_AFTER" class="olink">DB_AFTER</a> </span>  
Create a new record, immediately after the record to which the cursor refers.

<span class="term"> <a href="../../api/c/dbcput.md#dbcput_DB_BEFORE" class="olink">DB_BEFORE</a> </span>  
Create a new record, immediately before the record to which the cursor refers.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_CURRENT" class="olink">DB_CURRENT</a> </span>  
Replace the data part of the record to which the cursor refers.

<span class="term"> <a href="../../api/c/dbcput.md#dbcput_DB_KEYFIRST" class="olink">DB_KEYFIRST</a> </span>  
Create a new record as the first of the duplicate records for the supplied key.

<span class="term"> <a href="../../api/c/dbcput.md#dbcput_DB_KEYLAST" class="olink">DB_KEYLAST</a> </span>  
Create a new record, as the last of the duplicate records for the supplied key.

In all cases, the cursor is repositioned by a <a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a> operation to point to the newly inserted key/data pair in the database.

The following is a code example showing a cursor storing two data items in a database that supports duplicate data items:

``` c
int
store(DB *dbp)
    
{
    DBC *dbcp;
    DBT key, data;
    int ret;

    /*
     * The DB handle for a Btree database supporting duplicate data
     * items is the argument; acquire a cursor for the database.
     */
    if ((ret = dbp->cursor(dbp, NULL, &dbcp, 0)) != 0) {
        dbp->err(dbp, ret, "DB->cursor");
        goto err;
    }

    /* Initialize the key. */
    memset(&key, 0, sizeof(key));
    key.data = "new key";
    key.size = strlen(key.data) + 1;

    /* Initialize the data to be the first of two duplicate records. */
    memset(&data, 0, sizeof(data));
    data.data = "new key's data: entry #1";
    data.size = strlen(data.data) + 1;

    /* Store the first of the two duplicate records. */
    if ((ret = dbcp->put(dbcp, &key, &data, DB_KEYFIRST)) != 0)
        dbp->err(dbp, ret, "DB->cursor");

    /* Initialize the data to be the second of two duplicate records. */
    data.data = "new key's data: entry #2";
    data.size = strlen(data.data) + 1;

    /*
     * Store the second of the two duplicate records.  No duplicate
     * record sort function has been specified, so we explicitly
     * store the record as the last of the duplicate set.
     */
    if ((ret = dbcp->put(dbcp, &key, &data, DB_KEYLAST)) != 0)
        dbp->err(dbp, ret, "DB->cursor");

err:    if ((ret = dbcp->close(dbcp)) != 0)
        dbp->err(dbp, ret, "DBcursor->close");

    return (0);
}
```

### Note

If you are using the Heap access method and you are creating a new record in the database, then the key that you provide to the <a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a> method should be empty. The <a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a> method will return the record's ID (RID) in the key. The RID is automatically created for you when Heap database records are created.

### Deleting records with a cursor

The <a href="../../api/c/dbcdel.md" class="olink">DBC-&gt;del()</a> method deletes records from the database using a cursor. The <a href="../../api/c/dbcdel.md" class="olink">DBC-&gt;del()</a> method deletes the record to which the cursor currently refers. In all cases, the cursor position is unchanged after a delete.

### Duplicating a cursor

Once a cursor has been initialized (for example, by a call to <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a>), it can be thought of as identifying a particular location in a database. The <a href="../../api/c/dbcdup.md" class="olink">DBC-&gt;dup()</a> method permits an application to create a new cursor that has the same locking and transactional information as the cursor from which it is copied, and which optionally refers to the same position in the database.

In order to maintain a cursor position when an application is using locking, locks are maintained on behalf of the cursor until the cursor is closed. In cases when an application is using locking without transactions, cursor duplication is often required to avoid self-deadlocks. For further details, refer to <a href="lock_am_conv.md" class="xref" title="Berkeley DB Transactional Data Store locking conventions">Berkeley DB Transactional Data Store locking conventions</a>.

### Equality Join

Berkeley DB supports "equality" (also known as "natural"), joins on secondary indices. An equality join is a method of retrieving data from a primary database using criteria stored in a set of secondary indices. It requires the data be organized as a primary database which contains the primary key and primary data field, and a set of secondary indices. Each of the secondary indices is indexed by a different secondary key, and, for each key in a secondary index, there is a set of duplicate data items that match the primary keys in the primary database.

For example, let's assume the need for an application that will return the names of stores in which one can buy fruit of a given color. We would first construct a primary database that lists types of fruit as the key item, and the store where you can buy them as the data item:

| Primary key: | Primary data:     |
|:-------------|:------------------|
| apple        | Convenience Store |
| blueberry    | Farmer's Market   |
| peach        | Shopway           |
| pear         | Farmer's Market   |
| raspberry    | Shopway           |
| strawberry   | Farmer's Market   |

We would then create a secondary index with the key **color**, and, as the data items, the names of fruits of different colors.

| Secondary key: | Secondary data: |
|:---------------|:----------------|
| blue           | blueberry       |
| red            | apple           |
| red            | raspberry       |
| red            | strawberry      |
| yellow         | peach           |
| yellow         | pear            |

This secondary index would allow an application to look up a color, and then use the data items to look up the stores where the colored fruit could be purchased. For example, by first looking up **blue**, the data item **blueberry** could be used as the lookup key in the primary database, returning **Farmer's Market**.

Your data must be organized in the following manner in order to use the <a href="../../api/c/dbjoin.md" class="olink">DB-&gt;join()</a> method:

1.  The actual data should be stored in the database represented by the <a href="../../api/c/db.md" class="olink">DB</a> object used to invoke this method. Generally, this <a href="../../api/c/db.md" class="olink">DB</a> object is called the <span class="emphasis">*primary*</span>.

2.  Secondary indices should be stored in separate databases, whose keys are the values of the secondary indices and whose data items are the primary keys corresponding to the records having the designated secondary key value. It is acceptable (and expected) that there may be duplicate entries in the secondary indices.

    These duplicate entries should be sorted for performance reasons, although it is not required. For more information see the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_DUPSORT" class="olink">DB_DUPSORT</a> flag to the <a href="../../api/c/dbset_flags.md" class="olink">DB-&gt;set_flags()</a> method.

What the <a href="../../api/c/dbjoin.md" class="olink">DB-&gt;join()</a> method does is review a list of secondary keys, and, when it finds a data item that appears as a data item for all of the secondary keys, it uses that data item as a lookup into the primary database, and returns the associated data item.

If there were another secondary index that had as its key the **cost** of the fruit, a similar lookup could be done on stores where inexpensive fruit could be purchased:

| Secondary key: | Secondary data: |
|:---------------|:----------------|
| expensive      | blueberry       |
| expensive      | peach           |
| expensive      | pear            |
| expensive      | strawberry      |
| inexpensive    | apple           |
| inexpensive    | pear            |
| inexpensive    | raspberry       |

The <a href="../../api/c/dbjoin.md" class="olink">DB-&gt;join()</a> method provides equality join functionality. While not strictly cursor functionality, in that it is not a method off a cursor handle, it is more closely related to the cursor operations than to the standard <a href="../../api/c/db.md" class="olink">DB</a> operations.

It is also possible to do lookups based on multiple criteria in a single operation. For example, it is possible to look up fruits that are both red and expensive in a single operation. If the same fruit appeared as a data item in both the color and expense indices, then that fruit name would be used as the key for retrieval from the primary index, and would then return the store where expensive, red fruit could be purchased.

#### Example

Consider the following three databases:

<span class="term">personnel</span>  
- key = SSN
- data = record containing name, address, phone number, job title

<span class="term">lastname</span>  
- key = lastname
- data = SSN

<span class="term">jobs</span>  
- key = job title
- data = SSN

Consider the following query:

``` c
Return the personnel records of all people named smith with the job
title manager.
```

This query finds are all the records in the primary database (personnel) for whom the criteria **lastname=smith and job title=manager** is true.

Assume that all databases have been properly opened and have the handles: pers_db, name_db, job_db. We also assume that we have an active transaction to which the handle txn refers.

``` c
DBC *name_curs, *job_curs, *join_curs;
DBC *carray[3];
DBT key, data;
int ret, tret;

name_curs = NULL;
job_curs = NULL;
memset(&key, 0, sizeof(key));
memset(&data, 0, sizeof(data));

if ((ret =
    name_db->cursor(name_db, txn, &name_curs, 0)) != 0)
    goto err;
key.data = "smith";
key.size = sizeof("smith");
if ((ret =
    name_curs->get(name_curs, &key, &data, DB_SET)) != 0)
    goto err;

if ((ret = job_db->cursor(job_db, txn, &job_curs, 0)) != 0)
    goto err;
key.data = "manager";
key.size = sizeof("manager");
if ((ret =
    job_curs->get(job_curs, &key, &data, DB_SET)) != 0)
    goto err;

carray[0] = name_curs;
carray[1] = job_curs;
carray[2] = NULL;

if ((ret =
    pers_db->join(pers_db, carray, &join_curs, 0)) != 0)
    goto err;
while ((ret =
    join_curs->get(join_curs, &key, &data, 0)) == 0) {
    /* Process record returned in key/data. */
}

/*
 * If we exited the loop because we ran out of records,
 * then it has completed successfully.
 */
if (ret == DB_NOTFOUND)
    ret = 0;

err:
if (join_curs != NULL &&
    (tret = join_curs->close(join_curs)) != 0 && ret == 0)
    ret = tret;
if (name_curs != NULL &&
    (tret = name_curs->close(name_curs)) != 0 && ret == 0)
    ret = tret;
if (job_curs != NULL &&
    (tret = job_curs->close(job_curs)) != 0 && ret == 0)
    ret = tret;

return (ret);
```

The name cursor is positioned at the beginning of the duplicate list for **smith** and the job cursor is placed at the beginning of the duplicate list for **manager**. The join cursor is returned from the join method. This code then loops over the join cursor getting the personnel records of each one until there are no more.

### Data item count

Once a cursor has been initialized to refer to a particular key in the database, it can be used to determine the number of data items that are stored for any particular key. The <a href="../../api/c/dbccount.md" class="olink">DBC-&gt;count()</a> method returns this number of data items. The returned value is always one, unless the database supports duplicate data items, in which case it may be any number of items.

### Cursor close

The <a href="../../api/c/dbcclose.md" class="olink">DBC-&gt;close()</a> method closes the <a href="../../api/c/dbc.md" class="olink">DBC</a> cursor, after which the cursor may no longer be used. Although cursors are implicitly closed when the database they point to are closed, it is good programming practice to explicitly close cursors. In addition, in transactional systems, cursors may not exist outside of a transaction and so must be explicitly closed.
