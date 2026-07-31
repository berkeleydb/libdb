---
title: "Recoverability and deadlock handling"
api-name: "Recoverability and deadlock handling"
source: docs/programmer_reference/transapp_put.html
---
## Recoverability and deadlock handling

The first reason listed for using transactions was recoverability. Any logical change to a database may require multiple changes to underlying data structures. For example, modifying a record in a Btree may require leaf and internal pages to split, so a single <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> method call can potentially require that multiple physical database pages be written. If only some of those pages are written and then the system or application fails, the database is left inconsistent and cannot be used until it has been recovered; that is, until the partially completed changes have been undone.

<span class="emphasis">*Write-ahead-logging*</span> is the term that describes the underlying implementation that Berkeley DB uses to ensure recoverability. What it means is that before any change is made to a database, information about the change is written to a database log. During recovery, the log is read, and databases are checked to ensure that changes described in the log for committed transactions appear in the database. Changes that appear in the database but are related to aborted or unfinished transactions in the log are undone from the database.

For recoverability after application or system failure, operations that modify the database must be protected by transactions. More specifically, operations are not recoverable unless a transaction is begun and each operation is associated with the transaction via the Berkeley DB interfaces, and then the transaction successfully committed. This is true even if logging is turned on in the database environment.

Here is an example function that updates a record in a database in a transactionally protected manner. The function takes a key and data items as arguments and then attempts to store them into the database.

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

    return (0);
}

int
add_fruit(DB_ENV *dbenv, DB *db, char *fruit, char *name)
{
    DBT key, data;
    DB_TXN *tid;
    int fail, ret, t_ret;

    /* Initialization. */
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));
    key.data = fruit;
    key.size = strlen(fruit);
    data.data = name;
    data.size = strlen(name);

    for (fail = 0;;) {
        /* Begin the transaction. */
        if ((ret = dbenv->txn_begin(dbenv, NULL, &tid, 0)) != 0) {
            dbenv->err(dbenv, ret, "DB_ENV->txn_begin");
            exit (1);
        }

        /* Store the value. */
        switch (ret = db->put(db, tid, &key, &data, 0)) {
        case 0:
            /* Success: commit the change. */
            if ((ret = tid->commit(tid, 0)) != 0) {
                dbenv->err(dbenv, ret, "DB_TXN->commit");
                exit (1);
            }
            return (0);
        case DB_LOCK_DEADLOCK:
        default:
            /* Retry the operation. */
            if ((t_ret = tid->abort(tid)) != 0) {
                dbenv->err(dbenv, t_ret, "DB_TXN->abort");
                exit (1);
            }
            if (fail++ == MAXIMUM_RETRY)
                return (ret);
            break;
        }
    }
}
```

Berkeley DB also uses transactions to recover from deadlock. Database operations (that is, any call to a function underlying the handles returned by <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> and <a href="../../api/c/dbcursor.md" class="olink">DB-&gt;cursor()</a>) are usually performed on behalf of a unique locker. Transactions can be used to perform multiple calls on behalf of the same locker within a single thread of control. For example, consider the case in which an application uses a cursor scan to locate a record and then the application accesses another other item in the database, based on the key returned by the cursor, without first closing the cursor. If these operations are done using default locker IDs, they may conflict. If the locks are obtained on behalf of a transaction, using the transaction's locker ID instead of the database handle's locker ID, the operations will not conflict.

There is a new error return in this function that you may not have seen before. In transactional (not Concurrent Data Store) applications supporting both readers and writers, or just multiple writers, Berkeley DB functions have an additional possible error return: <a href="program_errorret.md#program_errorret.DB_LOCK_DEADLOCK" class="link">DB_LOCK_DEADLOCK</a>. This means two threads of control deadlocked, and the thread receiving the `DB_LOCK_DEADLOCK` error return has been selected to discard its locks in order to resolve the problem. When an application receives a `DB_LOCK_DEADLOCK` return, the correct action is to close any cursors involved in the operation and abort any enclosing transaction. In the sample code, any time the <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> method returns `DB_LOCK_DEADLOCK`, <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a> is called (which releases the transaction's Berkeley DB resources and undoes any partial changes to the databases), and then the transaction is retried from the beginning.

There is no requirement that the transaction be attempted again, but that is a common course of action for applications. Applications may want to set an upper bound on the number of times an operation will be retried because some operations on some data sets may simply be unable to succeed. For example, updating all of the pages on a large Web site during prime business hours may simply be impossible because of the high access rate to the database.

The <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a> method is called in error cases other than deadlock. Any time an error occurs, such that a transactionally protected set of operations cannot complete successfully, the transaction must be aborted. While deadlock is by far the most common of these errors, there are other possibilities; for example, running out of disk space for the filesystem. In Berkeley DB transactional applications, there are three classes of error returns: "expected" errors, "unexpected but recoverable" errors, and a single "unrecoverable" error. Expected errors are errors like <a href="program_errorret.md#program_errorret.DB_NOTFOUND" class="link">DB_NOTFOUND</a>, which indicates that a searched-for key item is not present in the database. Applications may want to explicitly test for and handle this error, or, in the case where the absence of a key implies the enclosing transaction should fail, simply call <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a>. Unexpected but recoverable errors are errors like <a href="program_errorret.md#program_errorret.DB_LOCK_DEADLOCK" class="link">DB_LOCK_DEADLOCK</a>, which indicates that an operation has been selected to resolve a deadlock, or a system error such as EIO, which likely indicates that the filesystem has no available disk space. Applications must immediately call <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a> when these returns occur, as it is not possible to proceed otherwise. The only unrecoverable error is <a href="program_errorret.md#program_errorret.DB_RUNRECOVERY" class="link">DB_RUNRECOVERY</a>, which indicates that the system must stop and recovery must be run.

The above code can be simplified in the case of a transaction comprised entirely of a single database put or delete operation, as operations occurring in transactional databases are implicitly transaction protected. For example, in a transactional database, the above code could be more simply written as:

``` c
    for (fail = 0; fail++ <= MAXIMUM_RETRY &&
        (ret = db->put(db, NULL, &key, &data, 0)) == DB_LOCK_DEADLOCK;)
        continue;
    return (ret == 0 ? 0 : 1);
```

and the underlying transaction would be automatically handled by Berkeley DB.

Programmers should not attempt to enumerate all possible error returns in their software. Instead, they should explicitly handle expected returns and default to aborting the transaction for the rest. It is entirely the choice of the programmer whether to check for <a href="program_errorret.md#program_errorret.DB_RUNRECOVERY" class="link">DB_RUNRECOVERY</a> explicitly or not — attempting new Berkeley DB operations after <a href="program_errorret.md#program_errorret.DB_RUNRECOVERY" class="link">DB_RUNRECOVERY</a> is returned does not worsen the situation. Alternatively, using the <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a> method to handle an unrecoverable error and simply doing some number of abort-and-retry cycles for any unexpected Berkeley DB or system error in the mainline code often results in the simplest and cleanest application code.
