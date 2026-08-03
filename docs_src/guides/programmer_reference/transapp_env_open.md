---
title: "Opening the environment"
api-name: "Opening the environment"
source: docs/programmer_reference/transapp_env_open.html
---
## Opening the environment

Creating transaction-protected applications using the Berkeley DB library is quite easy. Applications first use <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> to initialize the database environment. Transaction-protected applications normally require all four Berkeley DB subsystems, so the <a href="../../api/c/envopen.md#envopen_DB_INIT_MPOOL" class="olink">DB_INIT_MPOOL</a>, <a href="../../api/c/envopen.md#envopen_DB_INIT_LOCK" class="olink">DB_INIT_LOCK</a>, <a href="../../api/c/envopen.md#envopen_DB_INIT_LOG" class="olink">DB_INIT_LOG</a>, and <a href="../../api/c/envopen.md#envopen_DB_INIT_TXN" class="olink">DB_INIT_TXN</a> flags should be specified.

Once the application has called <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>, it opens its databases within the environment. Once the databases are opened, the application makes changes to the databases inside of transactions. Each set of changes that entails a unit of work should be surrounded by the appropriate <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a>, <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a> and <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a> calls. The Berkeley DB access methods will make the appropriate calls into the Lock, Log and Memory Pool subsystems in order to guarantee transaction semantics. When the application is ready to exit, all outstanding transactions should have been committed or aborted.

Databases accessed by a transaction must not be closed during the transaction. Once all outstanding transactions are finished, all open Berkeley DB files should be closed. When the Berkeley DB database files have been closed, the environment should be closed by calling <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a>.

The following code fragment creates the database environment directory then opens the environment, running recovery. Our <a href="../../api/c/env.md" class="olink">DB_ENV</a> database environment handle is declared to be free-threaded using the <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> flag, and so may be used by any number of threads that we may subsequently create.

``` c
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <db.h>

#define    ENV_DIRECTORY    "TXNAPP"

void  env_dir_create(void);
void  env_open(DB_ENV **);
...

int
main(int argc, char *argv[])
{
    extern int optind;
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

    return (0);
}

...

void
env_dir_create()
{
    struct stat sb;

    /*
     * If the directory exists, we're done.  We do not further check
     * the type of the file, DB will fail appropriately if it's the
     * wrong type.
     */
    if (stat(ENV_DIRECTORY, &sb) == 0)
        return;

    /* Create the directory, read/write/access owner only. */
    if (mkdir(ENV_DIRECTORY, S_IRWXU) != 0) {
        fprintf(stderr,
            "txnapp: mkdir: %s: %s\n", ENV_DIRECTORY, strerror(errno));
        exit (1);
    }
}

void
env_open(DB_ENV **dbenvp)
{
    DB_ENV *dbenv;
    int ret;

    /* Create the environment handle. */
    if ((ret = db_env_create(&dbenv, 0)) != 0) {
        fprintf(stderr,
            "txnapp: db_env_create: %s\n", db_strerror(ret));
        exit (1);
    }

    /* Set up error handling. */
    dbenv->set_errpfx(dbenv, "txnapp");
    dbenv->set_errfile(dbenv, stderr);

    /*
     * Open a transactional environment:
     *    create if it doesn't exist
     *    free-threaded handle
     *    run recovery
     *    read/write owner only
     */
    if ((ret = dbenv->open(dbenv, ENV_DIRECTORY,
        DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
        DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER | DB_THREAD,
        S_IRUSR | S_IWUSR)) != 0) {
        (void)dbenv->close(dbenv, 0);
        fprintf(stderr, "dbenv->open: %s: %s\n",
            ENV_DIRECTORY, db_strerror(ret));
        exit (1);
    }

    *dbenvp = dbenv;
}
```

After running this initial program, we can use the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility to display the contents of the environment directory:

``` c
prompt> db_stat -e -h TXNAPP
3.2.1   Environment version.
120897  Magic number.
0       Panic value.
1       References.
6       Locks granted without waiting.
0       Locks granted after waiting.
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Mpool Region: 4.
264KB   Size (270336 bytes).
-1      Segment ID.
1       Locks granted without waiting.
0       Locks granted after waiting.
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Log Region: 3.
96KB    Size (98304 bytes).
-1      Segment ID.
3       Locks granted without waiting.
0       Locks granted after waiting.
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Lock Region: 2.
240KB   Size (245760 bytes).
-1      Segment ID.
1       Locks granted without waiting.
0       Locks granted after waiting.
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Txn Region: 5.
8KB     Size (8192 bytes).
-1      Segment ID.
1       Locks granted without waiting.
0       Locks granted after waiting.
```
