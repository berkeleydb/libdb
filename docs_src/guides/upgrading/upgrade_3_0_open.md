---
title: "database open/close"
api-name: "database open/close"
source: docs/upgrading/upgrade_3_0_open.html
---
## database open/close

Database opens were changed in the Berkeley DB 3.0 release in a similar way to environment opens.

To upgrade your application, first find each place your application opens a database, that is, calls the db_open function. Each of these calls should be replaced with calls to <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> and <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a>.

Here's an example creating a Berkeley DB database using the 2.X interface:

``` c
DB *dbp;
DB_ENV *dbenv;
int ret;

if ((ret = db_open(DATABASE,
    DB_BTREE, DB_CREATE, 0664, dbenv, NULL, &dbp)) != 0)
    return (ret);
```

In the Berkeley DB 3.0 release, this code would be written as:

``` c
DB *dbp;
DB_ENV *dbenv;
int ret;

if ((ret = db_create(&dbp, dbenv, 0)) != 0)
    return (ret);

if ((ret = dbp->open(dbp,
    DATABASE, NULL, DB_BTREE, DB_CREATE, 0664)) != 0) {
    (void)dbp->close(dbp, 0);
    return (ret);
}
```

As you can see, the arguments to db_open and to <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> are largely the same. There is some re-organization, and note that the enclosing <a href="../../api/c/env.md" class="olink">DB_ENV</a> structure is specified when the <a href="../../api/c/db.md" class="olink">DB</a> object is created using the <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> function. There is one additional argument to <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a>, argument \#3. For backward compatibility with the 2.X Berkeley DB releases, simply set that argument to NULL.

There are two additional issues with the db_open call.

First, it was possible in the 2.X releases for an application to provide an environment that did not contain a shared memory buffer pool as the database environment, and Berkeley DB would create a private one automatically. This functionality is no longer available, applications must specify the <a href="../../api/c/envopen.md#envopen_DB_INIT_MPOOL" class="olink">DB_INIT_MPOOL</a> flag if databases are going to be opened in the environment.

The final issue with upgrading the db_open call is that the DB_INFO structure is no longer used, having been replaced by individual methods on the <a href="../../api/c/db.md" class="olink">DB</a> handle. That change is discussed in detail later in this chapter.
