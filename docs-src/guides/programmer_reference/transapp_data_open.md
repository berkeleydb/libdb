---
title: "Opening the databases"
api-name: "Opening the databases"
source: docs/programmer_reference/transapp_data_open.html
---
## Opening the databases

Next, we open three databases ("color" and "fruit" and "cats"), in the database environment. Again, our <a href="../../api/c/db.md" class="olink">DB</a> database handles are declared to be free-threaded using the <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> flag, and so may be used by any number of threads we subsequently create.

``` c
int
main(int argc, char *argv[])
{
    extern int optind;
    DB *db_cats, *db_color, *db_fruit;
    DB_ENV *dbenv;
    int ch;

    while ((ch = getopt(argc, argv, "")) != EOF)
        switch (ch) {
        case '?':
        default:
            usage();
        }
    argc -= optind;
    argv += optind;

    env_dir_create();
    env_open(&dbenv);
    ...

   /* Open database: Key is fruit class; Data is specific type. */
    if (db_open(dbenv, &db_fruit, "fruit", 0))
        return (1);

    /* Open database: Key is a color; Data is an integer. */
    if (db_open(dbenv, &db_color, "color", 0))
        return (1);

    /*
     * Open database:
     *    Key is a name; Data is: company name, cat breeds.
     */
    if (db_open(dbenv, &db_cats, "cats", 1))
        return (1);
    
    ...    

    return (0);
}

int
db_open(DB_ENV *dbenv, DB **dbp, char *name, int dups)
{
    DB *db;
    int ret;

    /* Create the database handle. */
    if ((ret = db_create(&db, dbenv, 0)) != 0) {
        dbenv->err(dbenv, ret, "db_create");
        return (1);
    }

    /* Optionally, turn on duplicate data items. */
    if (dups && (ret = db->set_flags(db, DB_DUP)) != 0) {
        (void)db->close(db, 0);
        dbenv->err(dbenv, ret, "db->set_flags: DB_DUP");
        return (1);
    }

    /*
     * Open a database in the environment:
     *    create if it doesn't exist
     *    free-threaded handle
     *    read/write owner only
     */
    if ((ret = db->open(db, NULL, name, NULL, DB_BTREE,
        DB_AUTO_COMMIT | DB_CREATE | DB_THREAD, S_IRUSR | S_IWUSR)) != 0) {
        (void)db->close(db, 0);
        dbenv->err(dbenv, ret, "db->open: %s", name);
        return (1);
    }

    *dbp = db;
    return (0);
}
```

After opening the database, we can use the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility to display information about a database we have created:

``` c
prompt> db_stat -h TXNAPP -d color
53162   Btree magic number.
8       Btree version number.
Flags:
2       Minimum keys per-page.
8192    Underlying database page size.
1       Number of levels in the tree.
0       Number of unique keys in the tree.
0       Number of data items in the tree.
0       Number of tree internal pages.
0       Number of bytes free in tree internal pages (0% ff).
1       Number of tree leaf pages.
8166    Number of bytes free in tree leaf pages (0.% ff).
0       Number of tree duplicate pages.
0       Number of bytes free in tree duplicate pages (0% ff).
0       Number of tree overflow pages.
0       Number of bytes free in tree overflow pages (0% ff).
0       Number of pages on the free list.
```

The database open must be enclosed within a transaction in order to be recoverable. The transaction will ensure that created files are re-created in recovered environments (or do not appear at all). Additional database operations or operations on other databases can be included in the same transaction, of course. In the simple case, where the open is the only operation in the transaction, an application can set the <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> flag instead of creating and managing its own transaction handle. The <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> flag will internally wrap the operation in a transaction, simplifying application code.

The previous example is the simplest case of transaction protection for database open. Obviously, additional database operations can be done in the scope of the same transaction. For example, an application maintaining a list of the databases in a database environment in a well-known file might include an update of the list in the same transaction in which the database is created. Or, an application might create both a primary and secondary database in a single transaction.

<a href="../../api/c/db.md" class="olink">DB</a> handles that will later be used for transactionally protected database operations must be opened within a transaction. Specifying a transaction handle to database operations using <a href="../../api/c/db.md" class="olink">DB</a> handles not opened within a transaction will return an error. Similarly, not specifying a transaction handle to database operations that will modify the database, using handles that were opened within a transaction, will also return an error.
