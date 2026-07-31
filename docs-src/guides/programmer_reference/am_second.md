---
title: "Secondary indexes"
api-name: "Secondary indexes"
source: docs/programmer_reference/am_second.html
---
## Secondary indexes

<span class="sect2"> [Error Handling With Secondary Indexes](am_second.md#idp51040080) </span>

A secondary index, put simply, is a way to efficiently access records in a database (the primary) by means of some piece of information other than the usual (primary) key. In Berkeley DB, this index is simply another database whose keys are these pieces of information (the secondary keys), and whose data are the primary keys. Secondary indexes can be created manually by the application; there is no disadvantage, other than complexity, to doing so. However, when the secondary key can be mechanically derived from the primary key and datum that it points to, as is frequently the case, Berkeley DB can automatically and transparently manage secondary indexes.

As an example of how secondary indexes might be used, consider a database containing a list of students at a college, each of whom has a unique student ID number. A typical database would use the student ID number as the key; however, one might also reasonably want to be able to look up students by last name. To do this, one would construct a secondary index in which the secondary key was this last name.

In SQL, this would be done by executing something like the following:

``` c
CREATE TABLE students(student_id CHAR(4) NOT NULL,
    lastname CHAR(15), firstname CHAR(15), PRIMARY KEY(student_id));
CREATE INDEX lname ON students(lastname);
```

In Berkeley DB, this would work as follows (a <a href="second.javas" class="ulink" target="_top">Java API example is also available</a>):

``` c
struct student_record {
    char student_id[4];
    char last_name[15];
    char first_name[15];
};

....

void
second()
{
    DB *dbp, *sdbp;
    int ret;
    
    /* Open/create primary */
    if ((ret = db_create(&dbp, dbenv, 0)) != 0)
        handle_error(ret);
    if ((ret = dbp->open(dbp, NULL,
        "students.db", NULL, DB_BTREE, DB_CREATE, 0600)) != 0)
        handle_error(ret);
    
    /*
     * Open/create secondary.  Note that it supports duplicate data
     * items, since last names might not be unique.
     */
    if ((ret = db_create(&sdbp, dbenv, 0)) != 0)
        handle_error(ret);
    if ((ret = sdbp->set_flags(sdbp, DB_DUP | DB_DUPSORT)) != 0)
        handle_error(ret);
    if ((ret = sdbp->open(sdbp, NULL,
        "lastname.db", NULL, DB_BTREE, DB_CREATE, 0600)) != 0)
        handle_error(ret);
    
    /* Associate the secondary with the primary. */
    if ((ret = dbp->associate(dbp, NULL, sdbp, getname, 0)) != 0)
        handle_error(ret);
}

/*
 * getname -- extracts a secondary key (the last name) from a primary
 *     key/data pair
 */
int
getname(DB *secondary, const DBT *pkey, const DBT *pdata, DBT *skey)
    
{
    /*
     * Since the secondary key is a simple structure member of the
     * record, we don't have to do anything fancy to return it.  If
     * we have composite keys that need to be constructed from the
     * record, rather than simply pointing into it, then the user's
     * function might need to allocate space and copy data.  In
     * this case, the DB_DBT_APPMALLOC flag should be set in the
     * secondary key DBT.
     */
    memset(skey, 0, sizeof(DBT));
    skey->data = ((struct student_record *)pdata->data)->last_name;
    skey->size = sizeof(((struct student_record *)pdata->data)->last_name);
    return (0);
}
```

From the application's perspective, putting things into the database works exactly as it does without a secondary index; one can simply insert records into the primary database. In SQL one would do the following:

``` c
INSERT INTO student
    VALUES ("WC42", "Churchill      ", "Winston        ");
```

and in Berkeley DB, one does:

``` c
struct student_record s;
DBT data, key;

memset(&key, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));
memset(&s, 0, sizeof(struct student_record));
key.data = "WC42";
key.size = 4;
memcpy(&s.student_id, "WC42", sizeof(s.student_id));
memcpy(&s.last_name, "Churchill      ", sizeof(s.last_name));
memcpy(&s.first_name, "Winston        ", sizeof(s.first_name));
data.data = &s;
data.size = sizeof(s);
if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0)
    handle_error(ret);
    
```

Internally, a record with secondary key "Churchill" is inserted into the secondary database (in addition to the insertion of "WC42" into the primary, of course).

Deletes are similar. The SQL clause:

``` c
DELETE FROM student WHERE (student_id = "WC42");
```

looks like:

``` c
DBT key;

memset(&key, 0, sizeof(DBT));
key.data = "WC42";
key.size = 4;
if ((ret = dbp->del(dbp, txn, &key, 0)) != 0)
    handle_error(ret);
```

Deletes can also be performed on the secondary index directly; a delete done this way will delete the "real" record in the primary as well. If the secondary supports duplicates and there are duplicate occurrences of the secondary key, then all records with that secondary key are removed from both the secondary index and the primary database. In SQL:

``` c
DELETE FROM lname WHERE (lastname = "Churchill      ");
```

In Berkeley DB:

``` c
DBT skey;

memset(&skey, 0, sizeof(DBT));
skey.data = "Churchill      ";
skey.size = 15;
if ((ret = sdbp->del(sdbp, txn, &skey, 0)) != 0)
    handle_error(ret);
```

Gets on a secondary automatically return the primary datum. If <a href="../../api/c/dbget.md" class="olink">DB-&gt;pget()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;pget()</a> is used in lieu of <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a>, the primary key is returned as well. Thus, the equivalent of:

``` c
SELECT * from lname WHERE (lastname = "Churchill      ");
```

would be:

``` c
DBT data, pkey, skey;

memset(&skey, 0, sizeof(DBT));
memset(&pkey, 0, sizeof(DBT));
memset(&data, 0, sizeof(DBT));
skey.data = "Churchill      ";
skey.size = 15;
if ((ret = sdbp->pget(sdbp, txn, &skey, &pkey, &data, 0)) != 0)
    handle_error(ret);
/*
 * Now pkey contains "WC42" and data contains Winston's record.
 */
```

To create a secondary index to a Berkeley DB database, open the database that is to become a secondary index normally, then pass it as the "secondary" argument to the <a href="../../api/c/dbassociate.md" class="olink">DB-&gt;associate()</a> method for some primary database.

After a <a href="../../api/c/dbassociate.md" class="olink">DB-&gt;associate()</a> call is made, the secondary indexes become alternate interfaces to the primary database. All updates to the primary will be automatically reflected in each secondary index that has been associated with it. All get operations using the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> methods on the secondary index return the primary datum associated with the specified (or otherwise current, in the case of cursor operations) secondary key. The <a href="../../api/c/dbget.md" class="olink">DB-&gt;pget()</a> and <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;pget()</a> methods also become usable; these behave just like <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> and <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a>, but return the primary key in addition to the primary datum, for those applications that need it as well.

Cursor get operations on a secondary index perform as expected; although the data returned will by default be those of the primary database, a position in the secondary index is maintained normally, and records will appear in the order determined by the secondary key and the comparison function or other structure of the secondary database.

Delete operations on a secondary index delete the item from the primary database and all relevant secondaries, including the current one.

Put operations of any kind are forbidden on secondary indexes, as there is no way to specify a primary key for a newly put item. Instead, the application should use the <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> or <a href="../../api/c/dbcput.md" class="olink">DBC-&gt;put()</a> methods on the primary database.

Any number of secondary indexes may be associated with a given primary database, up to limitations on available memory and the number of open file descriptors.

Note that although Berkeley DB guarantees that updates made using any <a href="../../api/c/db.md" class="olink">DB</a> handle with an associated secondary will be reflected in the that secondary, associating each primary handle with all the appropriate secondaries is the responsibility of the application and is not enforced by Berkeley DB. It is generally unsafe, but not forbidden by Berkeley DB, to modify a database that has secondary indexes without having those indexes open and associated. Similarly, it is generally unsafe, but not forbidden, to modify a secondary index directly. Applications that violate these rules face the possibility of outdated or incorrect results if the secondary indexes are later used.

If a secondary index becomes outdated for any reason, it should be discarded using the <a href="../../api/c/dbremove.md" class="olink">DB-&gt;remove()</a> method and a new one created using the <a href="../../api/c/dbassociate.md" class="olink">DB-&gt;associate()</a> method. If a secondary index is no longer needed, all of its handles should be closed using the <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> method, and then the database should be removed using a new database handle and the <a href="../../api/c/dbremove.md" class="olink">DB-&gt;remove()</a> method.

Closing a primary database handle automatically dis-associates all secondary database handles associated with it.

### Error Handling With Secondary Indexes

An error return during a secondary update in CDS or DS (which requires an abort in TDS) may leave a secondary index inconsistent in CDS or DS. There are a few non-error returns:

- 0
- DB_BUFFER_SMALL
- DB_NOTFOUND
- DB_KEYEMPTY
- DB_KEYEXIST

In the case of any other error return during a secondary update in CDS or DS, delete the secondary indices, recreate them and set the `DB_CREATE flag` to the `DB->associate` method. Some examples of error returns that need to be handled this way are:

- ENOMEM - indicating there is insufficient memory to return the requested item
- EINVAL - indicating that an invalid flag value or parameter is specified

Note that `DB_RUNRECOVERY` and `DB_PAGE_NOTFOUND` are fatal errors which should never occur during normal use of CDS or DS. If those errors are returned by Berkeley DB when running without transactions, check the database integrity with the `DB->verify` method before rebuilding the secondary indices.
