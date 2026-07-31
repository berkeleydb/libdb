---
title: "environment open/close/unlink"
api-name: "environment open/close/unlink"
source: docs/upgrading/upgrade_3_0_envopen.html
---
## environment open/close/unlink

The hardest part of upgrading your application from a 2.X code base to the 3.0 release is translating the Berkeley DB environment open, close and remove calls.

There were two logical changes in this part of the Berkeley DB interface. First, in Berkeley DB 3.0, there are no longer separate structures that represent each subsystem (for example, DB_LOCKTAB or DB_TXNMGR) and an overall <a href="../../api/c/env.md" class="olink">DB_ENV</a> environment structure. Instead there is only the <a href="../../api/c/env.md" class="olink">DB_ENV</a> references should be passed around by your application instead of passing around DB_LOCKTAB or DB_TXNMGR references. This is likely to be a simple change for most applications as few applications use the lock_XXX, log_XXX, memp_XXX or txn_XXX interfaces to create Berkeley DB environments.

The second change is that there are no longer separate open, close, and unlink interfaces to the Berkeley DB subsystems. For example, in previous releases, it was possible to open a lock subsystem either using db_appinit or using the lock_open call. In the 3.0 release the XXX_open interfaces to the subsystems have been removed, and subsystems must now be opened using the 3.0 replacement for the db_appinit call.

To upgrade your application, first find each place your application opens, closes and/or removes a Berkeley DB environment. This will be code of the form:

``` c
db_appinit, db_appexit
lock_open, lock_close, lock_unlink
log_open, log_close, log_unlink
memp_open, memp_close, memp_unlink
txn_open, txn_close, txn_unlink
```

Each of these groups of calls should be replaced with calls to <a href="../../api/c/envcreate.md" class="olink">db_env_create()</a>, <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>, <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a>, and <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a>.

The <a href="../../api/c/envcreate.md" class="olink">db_env_create()</a> call and the call to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method replace the db_appinit, lock_open, log_open, memp_open and txn_open calls. The <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a> method replaces the db_appexit, lock_close, log_close, memp_close and txn_close calls. The <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> call replaces the lock_unlink, log_unlink, memp_unlink and txn_unlink calls.

Here's an example creating a Berkeley DB environment using the 2.X interface:

``` c
/*
 * db_init --
 *  Initialize the environment.
 */
DB_ENV *
db_init(home)
    char *home;
{
    DB_ENV *dbenv;

    if ((dbenv = (DB_ENV *)calloc(sizeof(DB_ENV), 1)) == NULL)
        return (errno);

    if ((errno = db_appinit(home, NULL, dbenv,
        DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN |
        DB_USE_ENVIRON)) == 0)
        return (dbenv);

    free(dbenv);
    return (NULL);
}
```

In the Berkeley DB 3.0 release, this code would be written as:

``` c
/*
 * db_init --
 *  Initialize the environment.
 */
int
db_init(home, dbenvp)
    char *home;
    DB_ENV **dbenvp;
{
    int ret;
    DB_ENV *dbenv;

    if ((ret = db_env_create(&dbenv, 0)) != 0)
        return (ret);

    if ((ret = dbenv->open(dbenv, home, NULL,
        DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN |
        DB_USE_ENVIRON, 0)) == 0) {
        *dbenvp = dbenv;
        return (0);
    }

    (void)dbenv->close(dbenv, 0);
    return (ret);
}
```

As you can see, the arguments to db_appinit and to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> are largely the same. There is some minor re-organization: the mapping is that arguments \#1, 2, 3, and 4 to db_appinit become arguments \#2, 3, 1 and 4 to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>. There is one additional argument to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>, argument \#5. For backward compatibility with the 2.X Berkeley DB releases, simply set that argument to 0.

It is only slightly more complex to translate calls to XXX_open to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method. Here's an example of creating a lock region using the 2.X interface:

``` c
lock_open(dir, DB_CREATE, 0664, dbenv, &regionp);
```

In the Berkeley DB 3.0 release, this code would be written as:

``` c
if ((ret = db_env_create(&dbenv, 0)) != 0)
    return (ret);

if ((ret = dbenv->open(dbenv,
    dir, NULL, DB_CREATE | DB_INIT_LOCK, 0664)) == 0) {
    *dbenvp = dbenv;
    return (0);
}
```

Note that in this example, you no longer need the DB_LOCKTAB structure reference that was required in Berkeley DB 2.X releases.

The final issue with upgrading the db_appinit call is the DB_MPOOL_PRIVATE option previously provided for the db_appinit call. If your application is using this flag, it should almost certainly use the new <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method. Regardless, you should carefully consider this change before converting to use the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag.

Translating db_appexit or XXX_close calls to <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a> is equally simple. Instead of taking a reference to a per-subsystem structure such as DB_LOCKTAB or DB_TXNMGR, all calls take a reference to a <a href="../../api/c/env.md" class="olink">DB_ENV</a> structure. The calling sequence is otherwise unchanged. Note that as the application no longer allocates the memory for the DB_ENV structure, application code to discard it after the call to db_appexit() is no longer needed.

Translating XXX_unlink calls to <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> is slightly more complex. As with <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a>, the call takes a reference to a <a href="../../api/c/env.md" class="olink">DB_ENV</a> structure instead of a per-subsystem structure. The calling sequence is slightly different, however. Here is an example of removing a lock region using the 2.X interface:

``` c
DB_ENV *dbenv;

ret = lock_unlink(dir, 1, dbenv);
```

In the Berkeley DB 3.0 release, this code fragment would be written as:

``` c
DB_ENV *dbenv;

ret = dbenv->remove(dbenv, dir, NULL, DB_FORCE);
```

The additional argument to the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> function is a configuration argument similar to that previously taken by db_appinit and now taken by the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method. For backward compatibility this new argument should simply be set to NULL. The force argument to XXX_unlink is now a flag value that is set by bitwise inclusively **OR**'ing it the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> flag argument.
