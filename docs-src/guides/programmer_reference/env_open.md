---
title: "Opening databases within the environment"
api-name: "Opening databases within the environment"
source: docs/programmer_reference/env_open.html
---
## Opening databases within the environment

Once the environment has been created, database handles may be created and then opened within the environment. This is done by calling the <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> function and specifying the appropriate environment as an argument.

File naming, database operations, and error handling will all be done as specified for the environment. For example, if the <a href="../../api/c/envopen.md#envopen_DB_INIT_LOCK" class="olink">DB_INIT_LOCK</a> or <a href="../../api/c/envopen.md#envopen_DB_INIT_CDB" class="olink">DB_INIT_CDB</a> flags were specified when the environment was created or joined, database operations will automatically perform all necessary locking operations for the application.

The following is a simple example of opening two databases within a database environment:

``` c
DB_ENV *dbenv;
    DB *dbp1, *dbp2;
    int ret;

    dbenv = NULL;
    dbp1 = dbp2 = NULL;
    /*
     * Create an environment and initialize it for additional error
     * reporting.
     */
    if ((ret = db_env_create(&dbenv, 0)) != 0) {
        fprintf(errfp, "%s: %s\n", progname, db_strerror(ret));
        return (ret);
    }

    dbenv->set_errfile(dbenv, errfp);
    dbenv->set_errpfx(dbenv, progname);

    /* Open an environment with just a memory pool. */
    if ((ret =
        dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0)) != 0) {
        dbenv->err(dbenv, ret, "environment open: %s", home);
        goto err;
    }

    /* Open database #1. */
    if ((ret = db_create(&dbp1, dbenv, 0)) != 0) {
        dbenv->err(dbenv, ret, "database create");
        goto err;
    }
    if ((ret = dbp1->open(dbp1,
        NULL, DATABASE1, NULL, DB_BTREE, DB_CREATE, 0664)) != 0) {
        dbenv->err(dbenv, ret, "DB->open: %s", DATABASE1);
        goto err;
    }

    /* Open database #2. */
    if ((ret = db_create(&dbp2, dbenv, 0)) != 0) {
        dbenv->err(dbenv, ret, "database create");
        goto err;
    }
    if ((ret = dbp2->open(dbp2,
        NULL, DATABASE2, NULL, DB_HASH, DB_CREATE, 0664)) != 0) {
        dbenv->err(dbenv, ret, "DB->open: %s", DATABASE2);
        goto err;
    }

    return (0);

err:    if (dbp2 != NULL)
        (void)dbp2->close(dbp2, 0);
    if (dbp1 != NULL)
        (void)dbp1->close(dbp1, 0);
    (void)dbenv->close(dbenv, 0);
    return (1);
}
```
