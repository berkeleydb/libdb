---
title: "DB->associate, DB->open, DB->remove, DB->rename"
api-name: "DB->associate, DB->open, DB->remove, DB->rename"
source: docs/upgrading/upgrade_4_1_fop.html
---
## DB-\>associate, DB-\>open, DB-\>remove, DB-\>rename

Historic releases of Berkeley DB transaction-protected the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a>, <a href="../../api/c/dbremove.md" class="olink">DB-&gt;remove()</a>, and <a href="../../api/c/dbrename.md" class="olink">DB-&gt;rename()</a> methods, but did it in an implicit way, that is, applications did not specify the <a href="../../api/c/txn.md" class="olink">TXN</a> handles associated with the operations. This approach had a number of problems, the most significant of which was there was no way to group operations that included database creation, removal or rename. For example, applications wanting to maintain a list of the databases in an environment in a well-known database had no way to update the well-known database and create a database within a single transaction, and so there was no way to guarantee the list of databases was correct for the environment after system or application failure. Another example might be the creation of both a primary database and a database intended to serve as a secondary index, where again there was no way to group the creation of both databases in a single atomic operation.

In the 4.1 release of Berkeley DB, this is no longer the case. The <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> and <a href="../../api/c/dbassociate.md" class="olink">DB-&gt;associate()</a> methods now take a <a href="../../api/c/txn.md" class="olink">TXN</a> handle returned by <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a> as an optional argument. New <a href="../../api/c/envdbremove.md" class="olink">DB_ENV-&gt;dbremove()</a> and <a href="../../api/c/envdbrename.md" class="olink">DB_ENV-&gt;dbrename()</a> methods taking a <a href="../../api/c/txn.md" class="olink">TXN</a> handle as an optional argument have been added.

To upgrade, applications must add a <a href="../../api/c/txn.md" class="olink">TXN</a> parameter in the appropriate location for the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> method calls, and the <a href="../../api/c/dbassociate.md" class="olink">DB-&gt;associate()</a> method calls (in both cases, the second argument for the C API, the first for the C++ or Java APIs).

Applications wanting to transaction-protect their <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> and <a href="../../api/c/dbassociate.md" class="olink">DB-&gt;associate()</a> method calls can add a NULL <a href="../../api/c/txn.md" class="olink">TXN</a> argument and specify the <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> flag to the two calls, which wraps the operation in an internal Berkeley DB transaction. Applications wanting to transaction-protect the remove and rename operations must rewrite their calls to the <a href="../../api/c/dbremove.md" class="olink">DB-&gt;remove()</a> and <a href="../../api/c/dbrename.md" class="olink">DB-&gt;rename()</a> methods to be, instead, calls to the new <a href="../../api/c/envdbremove.md" class="olink">DB_ENV-&gt;dbremove()</a> and <a href="../../api/c/envdbrename.md" class="olink">DB_ENV-&gt;dbrename()</a> methods. Applications not wanting to transaction-protect any of the operations can add a NULL argument to their <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> and <a href="../../api/c/dbassociate.md" class="olink">DB-&gt;associate()</a> method calls and require no further changes.

For example, an application currently opening and closing a database as follows:

``` c
DB *dbp;
DB_ENV *dbenv;
int ret;

if ((ret = db_create(&dbp, dbenv, 0)) != 0)
    goto err_handler;

if ((ret = dbp->open(dbp, "file", NULL, DB_BTREE, 
     DB_CREATE, 0664)) != 0) {
    (void)dbp->close(dbp);
    goto err_handler;
}
```

could transaction-protect the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> call as follows:

``` c
DB *dbp;
DB_ENV *dbenv;
int ret;

if ((ret = db_create(&dbp, dbenv, 0)) != 0)
    goto err_handler;

if ((ret = dbp->open(dbp,
    NULL, "file", NULL, DB_BTREE, DB_CREATE | 
    DB_AUTO_COMMIT, 0664)) != 0) {
    (void)dbp->close(dbp);
    goto err_handler;
}
```

An application currently removing a database as follows:

``` c
DB *dbp;
DB_ENV *dbenv;
int ret;

if ((ret = db_create(&dbp, dbenv, 0)) != 0)
    goto err_handler;

if ((ret = dbp->remove(dbp, "file", NULL, 0)) != 0)
    goto err_handler;
```

could transaction-protect the database removal as follows:

``` c
DB *dbp;
DB_ENV *dbenv;
int ret;

if ((ret =
    dbenv->dbremove(dbenv, NULL, "file", NULL, DB_AUTO_COMMIT)) != 0)
    goto err_handler;
```

An application currently renaming a database as follows:

``` c
DB *dbp;
DB_ENV *dbenv;
int ret;

if ((ret = db_create(&dbp, dbenv, 0)) != 0)
    goto err_handler;

if ((ret = dbp->rename(dbp, "file", NULL, "newname", 0)) != 0)
    goto err_handler;
```

could transaction-protect the database renaming as follows:

``` c
DB *dbp;
DB_ENV *dbenv;
int ret;

if ((ret = dbenv->dbrename(
    dbenv, NULL, "file", NULL, "newname", DB_AUTO_COMMIT)) != 0)
    goto err_handler;
```

These examples are the simplest possible translation, and will result in behavior matching that of previous releases. For further discussion on how to transaction-protect <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> method calls, see <a href="../../guides/programmer_reference/transapp_data_open.md" class="olink">Opening the databases</a>.

<a href="../../api/c/db.md" class="olink">DB</a> handles that will later be used for transaction-protected operations must be opened within a transaction. Specifying a transaction handle to operations using handles not opened within a transaction will return an error. Similarly, not specifying a transaction handle to operations using handles that were opened within a transaction will also return an error.
