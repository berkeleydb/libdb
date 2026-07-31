---
title: "Program Listing"
api-name: "Program Listing"
source: docs/gsg_db_rep/C/simpleprogramlisting.html
---
## Program Listing

<span class="sect2"> [Function: main()](simpleprogramlisting.md#main_c) </span>

<span class="sect2"> [Function: create_env()](simpleprogramlisting.md#create_env_c) </span>

<span class="sect2"> [Function: env_init()](simpleprogramlisting.md#env_init_c) </span>

<span class="sect2"> [Function: doloop()](simpleprogramlisting.md#doloop_c) </span>

<span class="sect2"> [Function: print_stocks()](simpleprogramlisting.md#printstocks_c) </span>

Our example program is a fairly simple transactional application. At this early stage of its development, the application contains no hint that it must be network-aware so the only command line argument that it takes is one that allows us to specify the environment home directory. (Eventually, we will specify things like host names and ports from the command line).

Note that the application performs all writes under the protection of a transaction; however, multiple database operations are not performed per transaction. Consequently, we simplify things a bit by using autocommit for our database writes.

Also, this application is single-threaded. It is possible to write a multi-threaded or multi-process application that performs replication. That said, the concepts described in this book are applicable to both single threaded and multi-threaded applications so nothing is gained by multi-threading this application other than distracting complexity. This manual does, however, identify where care must be taken when performing replication with a non-single threaded application.

Finally, remember that transaction processing is not described in this manual. Rather, see the *Berkeley DB Getting Started with Transaction Processing* guide for details on that topic.

### Function: main()

Our program begins with the usual assortment of include statements.

``` c
/*
 * File: ex_rep_gsg_simple.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <unistd.h>
#endif

#include <db.h> 

#ifdef _WIN32
extern int getopt(int, char * const *, const char *);
#endif  
```

We then define a few values. One is the size of our cache, which we keep deliberately small for this example, and the other is the name of our database. We also provide a global variable that is the name of our program; this is used for error reporting later on.

``` c
#define CACHESIZE   (10 * 1024 * 1024)
#define DATABASE    "quote.db"

const char *progname = "ex_rep_gsg_simple";  
```

Then we perform a couple of forward declarations. The first of these, `create_env()` and `env_init()` are used to open and initialize our environment.

Next we declare `doloop()`, which is the function that we use to add data to the database and then display its contents. This is essentially a big `do` loop, hence the function's name.

Finally, we have `print_stocks`, which is used to display a database record once it has been retrieved from the database.

``` c
int create_env(const char *, DB_ENV **);
int env_init(DB_ENV *, const char *);
int doloop (DB_ENV *);
int print_stocks(DB *);  
```

Next we need our `usage()` function, which is fairly trivial at this point:

``` c
/* Usage function */
static void
usage()
{
    fprintf(stderr, "usage: %s ", progname);
    fprintf(stderr, "-h home\n");
    exit(EXIT_FAILURE);
}  
```

That completed, we can jump into our application's `main()` function. If you are familiar with DB transactional applications, you will not find any surprises here. We begin by declaring and initializing the usual set of variables:

``` c
int
main(int argc, char *argv[])
{
    extern char *optarg;
    DB_ENV *dbenv;
    const char *home;
    char ch;
    int ret;

    dbenv = NULL;

    ret = 0;
    home = NULL;  
```

Now we create and configure our environment handle. We do this with our `create_env()` function, which we will show a little later in this example.

``` c
    if ((ret = create_env(progname, &dbenv)) != 0)
            goto err; 
```

Then we parse the command line arguments:

``` c
    while ((ch = getopt(argc, argv, "h:")) != EOF)
        switch (ch) {
        case 'h':
            home = optarg;
            break;
        case '?':
        default:
            usage();
        }

    /* Error check command line. */
    if (home == NULL)
        usage();  
```

Now we can open our environment. We do this with our `env_init()` function which we will describe a little later in this chapter.

``` c
    if ((ret = env_init(dbenv, home)) != 0)
            goto err; 
```

Now that we have opened the environment, we can call our `doloop()` function. This function performs the basic database interaction. Notice that we have not yet opened any databases. In a traditional transactional application we would probably open the databases before calling our our main data processing function. However, the eventual replicated application will want to handle database open and close in the main processing loop, so in a nod to what this application will eventually become we do a slightly unusual thing here.

``` c
    if ((ret = doloop(dbenv)) != 0) {
        dbenv->err(dbenv, ret, "Application failed");
        goto err;
    }  
```

Finally, we provide our application shutdown code. Note, again, that in a traditional transactional application all databases would also be closed here. But, again, due to the way this application will eventually behave, we cause the database close to occur in the `doloop()` function.

``` c
err: if (dbenv != NULL)
        (void)dbenv->close(dbenv, 0);

    return (ret);
}  
```

### Function: create_env()

Having written our `main()` function, we now implement the first of our utility functions that we use to manage our environments. This function exists only to make our code easier to manage, and all it does is create an environment handle for us.

``` c
int
create_env(const char *progname, DB_ENV **dbenvp)
{
    DB_ENV *dbenv;
    int ret;

    if ((ret = db_env_create(&dbenv, 0)) != 0) {
        fprintf(stderr, "can't create env handle: %s\n",
            db_strerror(ret));
        return (ret);
    }

    dbenv->set_errfile(dbenv, stderr);
    dbenv->set_errpfx(dbenv, progname);

    *dbenvp = dbenv;
    return (0);
}  
```

### Function: env_init()

Having written the function that initializes an environment handle, we now implement the function that opens the handle. Again, there should be no surprises here for anyone familiar with DB applications. The open flags that we use are those normally used for a transactional application.

``` c
int
env_init(DB_ENV *dbenv, const char *home)
{
    u_int32_t flags;
    int ret;

    (void)dbenv->set_cachesize(dbenv, 0, CACHESIZE, 0);
    (void)dbenv->set_flags(dbenv, DB_TXN_NOSYNC, 1);

    flags = DB_CREATE | 
            DB_INIT_LOCK | 
            DB_INIT_LOG | 
            DB_INIT_MPOOL |
            DB_INIT_TXN | 
            DB_RECOVER;
    if ((ret = dbenv->open(dbenv, home, flags, 0)) != 0)
        dbenv->err(dbenv, ret, "can't open environment");
    return (ret);
}  
```

### Function: doloop()

Having written our `main()` function and utility functions, we now implement our application's primary data processing function. This function provides a command prompt at which the user can enter a stock ticker value and a price for that value. This information is then entered to the database.

To display the database, simply enter `return` at the prompt.

To begin, we declare a database pointer, several `DBT` variables, and the usual assortment of variables used for buffers and return codes. We also initialize all of this.

``` c
#define BUFSIZE 1024
int
doloop(DB_ENV *dbenv)
{
    DB *dbp;
    DBT key, data;
    char buf[BUFSIZE], *rbuf;
    int ret;
    u_int32_t db_flags;

    dbp = NULL;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));
    ret = 0;  
```

Next, we begin the loop and we immediately open our database if it has not already been opened. Notice that we specify autocommit when we open the database. In this case, autocommit is important because we will only ever write to our database using it. There is no need for explicit transaction handles and commit/abort code in this application, because we are not combining multiple database operations together under a single transaction.

Autocommit is described in greater detail in the *Berkeley DB Getting Started with Transaction Processing* guide.

``` c
    for (;;) {

        if (dbp == NULL) {
            if ((ret = db_create(&dbp, dbenv, 0)) != 0)
                return (ret);

            db_flags = DB_AUTO_COMMIT | DB_CREATE;

            if ((ret = dbp->open(dbp, NULL, DATABASE,
                NULL, DB_BTREE, db_flags, 0)) != 0) {
                dbenv->err(dbenv, ret, "DB->open");
                goto err;
            }
        }  
```

Now we implement our command prompt. This is a simple and not very robust implementation of a command prompt. If the user enters the keywords `exit` or `quit`, the loop is exited and the application ends. If the user enters nothing and instead simply presses `return`, the entire contents of the database is displayed. We use our `print_stocks()` function to display the database. (That implementation is shown next in this chapter.)

Notice that very little error checking is performed on the data entered at this prompt. If the user fails to enter at least one space in the value string, a simple help message is printed and the prompt is returned to the user. That is the only error checking performed here. In a real-world application, at a minimum the application would probably check to ensure that the price was in fact an integer or float value. However, in order to keep this example code as simple as possible, we refrain from implementing a thorough user interface.

``` c
        printf("QUOTESERVER > ");
        fflush(stdout);

        if (fgets(buf, sizeof(buf), stdin) == NULL)
            break;
        if (strtok(&buf[0], " \t\n") == NULL) {
            switch ((ret = print_stocks(dbp))) {
            case 0:
                continue;
            default:
                dbp->err(dbp, ret, "Error traversing data");
                goto err;
            }
        }
        rbuf = strtok(NULL, " \t\n");
        if (rbuf == NULL || rbuf[0] == '\0') {
            if (strncmp(buf, "exit", 4) == 0 ||
                strncmp(buf, "quit", 4) == 0)
                break;
            dbenv->errx(dbenv, "Format: TICKER VALUE");
            continue;
        }  
```

Now we assign data to the `DBT`s that we will use to write the new information to the database.

``` c
        key.data = buf;
        key.size = (u_int32_t)strlen(buf);

        data.data = rbuf;
        data.size = (u_int32_t)strlen(rbuf);  
```

Having done that, we can write the new information to the database. Remember that this application uses autocommit, so no explicit transaction management is required. Also, the database is not configured for duplicate records, so the data portion of a record is overwritten if the provided key already exists in the database. However, in this case DB returns `DB_KEYEXIST` — which we ignore.

``` c
        if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) != 0)
        {
            dbp->err(dbp, ret, "DB->put");
            if (ret != DB_KEYEXIST)
                goto err;
        } 
    }  
```

Finally, we close our database before returning from the function.

``` c
err:    if (dbp != NULL)
        (void)dbp->close(dbp, DB_NOSYNC);

    return (ret);
}  
```

### Function: print_stocks()

The `print_stocks()` function simply takes a database handle, opens a cursor, and uses it to display all the information it finds in a database. This is trivial cursor operation that should hold no surprises for you. We simply provide it here for the sake of completeness.

If you are unfamiliar with basic cursor operations, please see the *Getting Started with Berkeley DB* guide.

``` c
/* Displays all stock quote information in the database. */
int
print_stocks(DB *dbp)
{
    DBC *dbc;
    DBT key, data;
#define MAXKEYSIZE  10
#define MAXDATASIZE 20
    char keybuf[MAXKEYSIZE + 1], databuf[MAXDATASIZE + 1];
    int ret, t_ret;
    u_int32_t keysize, datasize;

    if ((ret = dbp->cursor(dbp, NULL, &dbc, 0)) != 0) {
        dbp->err(dbp, ret, "can't open cursor");
        return (ret);
    }

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    printf("\tSymbol\tPrice\n");
    printf("\t======\t=====\n");

    for (ret = dbc->get(dbc, &key, &data, DB_FIRST);
        ret == 0;
        ret = dbc->get(dbc, &key, &data, DB_NEXT)) {
        keysize = key.size > MAXKEYSIZE ? MAXKEYSIZE : key.size;
        memcpy(keybuf, key.data, keysize);
        keybuf[keysize] = '\0';

        datasize = data.size >= MAXDATASIZE ? MAXDATASIZE : data.size;
        memcpy(databuf, data.data, datasize);
        databuf[datasize] = '\0';

        printf("\t%s\t%s\n", keybuf, databuf);
    }
    printf("\n");
    fflush(stdout);

    if ((t_ret = dbc->close(dbc)) != 0 && ret == 0)
        ret = t_ret;

    switch (ret) {
    case 0:
    case DB_NOTFOUND:
        return (0);
    default:
        return (ret);
    }
}  
```
