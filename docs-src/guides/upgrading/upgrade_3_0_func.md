---
title: "function arguments"
api-name: "function arguments"
source: docs/upgrading/upgrade_3_0_func.html
---
## function arguments

In Berkeley DB 3.0, there are no longer separate structures that represent each subsystem (for example, DB_LOCKTAB or DB_TXNMGR), and an overall <a href="../../api/c/env.md" class="olink">DB_ENV</a> environment structure. Instead there is only the <a href="../../api/c/env.md" class="olink">DB_ENV</a> references should be passed around by your application instead of passing around DB_LOCKTAB or DB_TXNMGR references.

Each of the following functions:

``` c
lock_detect
lock_get
lock_id
lock_put
lock_stat
lock_vec
```

should have its first argument, a reference to the DB_LOCKTAB structure, replaced with a reference to the enclosing <a href="../../api/c/env.md" class="olink">DB_ENV</a> structure. For example, the following line of code from a Berkeley DB 2.X application:

``` c
DB_LOCKTAB *lt;
DB_LOCK lock;

ret = lock_put(lt, lock);
```

should now be written as follows:

``` c
DB_ENV *dbenv;
DB_LOCK *lock;

ret = lock_put(dbenv, lock);
```

Similarly, all of the functions:

``` c
log_archive
log_compare
log_file
log_flush
log_get
log_put
log_register
log_stat
log_unregister
```

should have their DB_LOG argument replaced with a reference to a <a href="../../api/c/env.md" class="olink">DB_ENV</a> structure, and the functions:

``` c
memp_fopen
memp_register
memp_stat
memp_sync
memp_trickle
```

should have their DB_MPOOL argument replaced with a reference to a <a href="../../api/c/env.md" class="olink">DB_ENV</a> structure.

You should remove all references to DB_LOCKTAB, DB_LOG, DB_MPOOL, and DB_TXNMGR structures from your application, they are no longer useful in any way. In fact, a simple way to identify all of the places that need to be upgraded is to remove all such structures and variables they declare, and then compile. You will see a warning message from your compiler in each case that needs to be upgraded.
