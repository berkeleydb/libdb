---
title: "Transactional cursors"
api-name: "Transactional cursors"
source: docs/programmer_reference/transapp_cursor.html
---
## Transactional cursors

Berkeley DB cursors may be used inside a transaction, exactly as any other <a href="../../api/c/db.md" class="olink">DB</a> method. The enclosing transaction ID must be specified when the cursor is created, but it does not then need to be further specified on operations performed using the cursor. One important point to remember is that a cursor **must be closed** before the enclosing transaction is committed or aborted.

The following code fragment uses a cursor to store a new key in the cats database with four associated data items. The key is a name. The data items are a company name and a list of the breeds of cat owned. Each of the data entries is stored as a duplicate data item. In this example, transactions are necessary to ensure that either all or none of the data items appear in case of system or application failure.

``` c
int
main(int argc, char *argv)
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

    /* Open database: Key is fruit class; Data is specific type. */
    db_open(dbenv, &db_fruit, "fruit", 0);

    /* Open database: Key is a color; Data is an integer. */
    db_open(dbenv, &db_color, "color", 0);

    /*
     * Open database:
     *    Key is a name; Data is: company name, cat breeds.
     */
    db_open(dbenv, &db_cats, "cats", 1);

    add_fruit(dbenv, db_fruit, "apple", "yellow delicious");

    add_color(dbenv, db_color, "blue", 0);
    add_color(dbenv, db_color, "blue", 3);

    add_cat(dbenv, db_cats,
        "Amy Adams",
        "Oracle",
        "abyssinian",
        "bengal",
        "chartreaux",
        NULL);

    return (0);
}

int
add_cat(DB_ENV *dbenv, DB *db, char *name, ...)
{
    va_list ap;
    DBC *dbc;
    DBT key, data;
    DB_TXN *tid;
    int fail, ret, t_ret;
    char *s;

    /* Initialization. */
    fail = 0;

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));
    key.data = name;
    key.size = strlen(name);

retry:    /* Begin the transaction. */
    if ((ret = dbenv->txn_begin(dbenv, NULL, &tid, 0)) != 0) {
        dbenv->err(dbenv, ret, "DB_ENV->txn_begin");
        exit (1);
    }

    /* Delete any previously existing item. */
    switch (ret = db->del(db, tid, &key, 0)) {
    case 0:
    case DB_NOTFOUND:
        break;
    case DB_LOCK_DEADLOCK:
    default:
        /* Retry the operation. */
        if ((t_ret = tid->abort(tid)) != 0) {
            dbenv->err(dbenv, t_ret, "DB_TXN->abort");
            exit (1);
        }
        if (fail++ == MAXIMUM_RETRY)
            return (ret);
        goto retry;
    }

    /* Create a cursor. */
    if ((ret = db->cursor(db, tid, &dbc, 0)) != 0) {
        dbenv->err(dbenv, ret, "db->cursor");
        exit (1);
    }

    /* Append the items, in order. */
    va_start(ap, name);
    while ((s = va_arg(ap, char *)) != NULL) {
        data.data = s;
        data.size = strlen(s);
        switch (ret = dbc->put(dbc, &key, &data, DB_KEYLAST)) {
        case 0:
            break;
        case DB_LOCK_DEADLOCK:
        default:
            va_end(ap);

            /* Retry the operation. */
            if ((t_ret = dbc->close(dbc)) != 0) {
                dbenv->err(
                    dbenv, t_ret, "dbc->close");
                exit (1);
            }
            if ((t_ret = tid->abort(tid)) != 0) {
                dbenv->err(dbenv, t_ret, "DB_TXN->abort");
                exit (1);
            }
            if (fail++ == MAXIMUM_RETRY)
                return (ret);
            goto retry;
        }
    }
    va_end(ap);

    /* Success: commit the change. */
    if ((ret = dbc->close(dbc)) != 0) {
        dbenv->err(dbenv, ret, "dbc->close");
        exit (1);
    }
    if ((ret = tid->commit(tid, 0)) != 0) {
        dbenv->err(dbenv, ret, "DB_TXN->commit");
        exit (1);
    }
    return (0);
}
```
