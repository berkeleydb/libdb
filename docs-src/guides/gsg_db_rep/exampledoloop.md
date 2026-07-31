---
title: "Example Processing Loop"
api-name: "Example Processing Loop"
source: docs/gsg_db_rep/C/exampledoloop.html
---
## Example Processing Loop

<span class="sect2"> [Running It](exampledoloop.md#runningit) </span>

In this section we take the example processing loop that we presented in the previous section and we flesh it out to provide a more complete example. We do this by updating the `doloop()` function that our original transaction application used (see <a href="simpleprogramlisting.md#doloop_c" class="xref" title="Function: doloop()">Function: doloop()</a>) to fully support our replicated application.

In the following example code, code that we add to the original example is presented in **`bold`**.

To begin, we include a new header file into our application so that we can check for the `ENOENT` return value later in our processing loop. We also define our `APP_DATA` structure, and we define a `sleeptime` value. Finally, we add a new forward declaration for our event callback.

``` c
/*
 * File: ex_rep_gsg_repmgr.c
 */

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifndef _WIN32
#include <unistd.h>
#endif

#include <db.h>

#ifdef _WIN32
extern int getopt(int, char * const *, const char *);
#endif

#define CACHESIZE   (10 * 1024 * 1024)
#define DATABASE    "quote.db"
#define SLEEPTIME 3

const char *progname = "ex_rep_gsg_repmgr";

typedef struct {
    int is_master;
} APP_DATA;

int create_env(const char *, DB_ENV **);
int env_init(DB_ENV *, const char *);
int doloop (DB_ENV *);
static int print_stocks(DBC *);
void *event_callback(DB_ENV *, u_int32_t, void *); 
```

In our `main()` function, most of what we have to add to it is some new variable declarations and initializations:

``` c
int
main(int argc, char *argv[])
{
    DB_ENV *dbenv;
    DB_SITE *dbsite; 
    extern char *optarg;
    const char *home;
    char ch, *host, *portstr;
    int ret, local_is_set, is_group_creator;
    u_int32_t port;
    /* Used to track whether this is a replica or a master */
    APP_DATA my_app_data;

    my_app_data.is_master = 0;  /* Assume that we start as a replica */
    dbenv = NULL;

    ret = local_is_set = is_group_creator = 0;
    home = NULL;
```

The rest of our `main()` function is unchanged, except that we make our `APP_DATA` structure available through our environment handle's `app_private` field:

``` c
    if ((ret = create_env(progname, &dbenv)) != 0)
            goto err;

    /* Make APP_DATA available through the environment handle */
    dbenv->app_private = &my_app_data;

    /* Default priority is 100 */
    dbenv->rep_set_priority(dbenv, 100);
    /* Permanent messages require at least one ack */
    dbenv->repmgr_set_ack_policy(dbenv, DB_REPMGR_ACKS_ONE);
    /* Give 500 microseconds to receive the ack */
    dbenv->rep_set_timeout(dbenv, DB_REP_ACK_TIMEOUT, 500);

    while ((ch = getopt(argc, argv, "h:l:L:p:r:")) != EOF)
        switch (ch) {
        case 'h':
            home = optarg;
            break;
        /* Set the host and port used by this environment */
        case 'l':
            host = strtok(optarg, ":");
            if ((portstr = strtok(NULL, ":")) == NULL) {
                fprintf(stderr, "Bad host specification.\n");
                goto err;
            }
            port = (unsigned short)atoi(portstr);
            if ((ret = dbenv->repmgr_site(dbenv, host, port, &dbsite
                                          0)) != 0 ) {
                fprintf(stderr,
                    "Could not set local address %s.\n", host);
                goto err;
            }
            dbsite->set_config(dbsite, DB_LOCAL_SITE, 1);
            if (is_group_creator)
                dbsite->set_config(dbsite, DB_GROUP_CREATOR, 1);

            if ((ret = dbsite->close(dbsite)) != 0) {
                dbenv->(dbenv, ret, "DB_SITE->close");
                goto err;
            }
            local_is_set = 1;
            break;
        /* Set this replica's election priority */
        case 'p':
            dbenv->rep_set_priority(dbenv, atoi(optarg));
            break;
        /* Identify another site in the replication group */
        case 'r':
            host = strtok(optarg, ":");
            if ((portstr = strtok(NULL, ":")) == NULL) {
                fprintf(stderr, "Bad host specification.\n");
                goto err;
            }
            port = (unsigned short)atoi(portstr);
            if ((dbenv->repmgr_site(dbenv, host, port, &dbsite,
                                      0)) != 0) {
                fprintf(stderr,
                    "Could not add site %s.\n", host);
                goto err;
            }
            dbenv->set_config(dbsite, DB_BOOTSTRAP_HELPER, 1);
            if ((dbenv->close(dbsite)) != 0) {
                dbenv->err(dbenv, ret, "DB_SITE->close");
                goto err;
            }
            break;
        case '?':
        default:
            usage();
        }

    /* Error check command line. */
    if (home == NULL || !local_is_set)
        usage();

    if ((ret = env_init(dbenv, home)) != 0)
            goto err;

    if ((ret = dbenv->repmgr_start(dbenv, 3, DB_REP_ELECTION)) != 0)
        goto err;

    /* Sleep to give ourselves time to find a master. */
    sleep(5);

    if ((ret = doloop(dbenv)) != 0) {
        dbenv->err(dbenv, ret, "Application failed");
        goto err;
    }

err: if (dbenv != NULL)
        (void)dbenv->close(dbenv, 0);

    return (ret);
} 
```

Having updated our `main()`, we must also update our `create_env()` function to register our `event_callback` callback. Notice that our `env_init()` function, which is responsible for actually opening our environment handle, is unchanged:

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
    (void)dbenv->set_event_notify(dbenv, event_callback);

    *dbenvp = dbenv;
    return (0);
} 

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
            DB_INIT_REP |
            DB_INIT_TXN |
            DB_RECOVER |
            DB_THREAD;
    if ((ret = dbenv->open(dbenv, home, flags, 0)) != 0)
        dbenv->err(dbenv, ret, "can't open environment");
    return (ret);
}
```

That done, we need to implement our `event_callback()` callback. Note that what we use here is no different from the callback that we described in the previous section. However, for the sake of completeness we provide the implementation here again.

``` c
        
          /*
 * A callback used to determine whether the local environment is a 
 * replica or a master. This is called by the Replication Manager
 * when the local replication environment changes state.
 */
void *
event_callback(DB_ENV *dbenv, u_int32_t which, void *info)
{
    APP_DATA *app = dbenv->app_private;

    info = NULL;                /* Currently unused. */

    switch (which) {
    case DB_EVENT_REP_MASTER:
        app->is_master = 1;
        break;

    case DB_EVENT_REP_CLIENT:
        app->is_master = 0;
        break;

    case DB_EVENT_REP_STARTUPDONE: /* fallthrough */
    case DB_EVENT_REP_NEWMASTER:
        /* Ignore. */
        break;

    default:
        dbenv->errx(dbenv, "ignoring event %d", which);
    }
}
        
      
```

That done, we need to update our `doloop()` function. This is the place where we most heavily modify our application.

We begin by introducing `APP_DATA` to the function:

``` c
/*
 * Provides the main data processing function for our application.
 * This function provides a command line prompt to which the user
 * can provide a ticker string and a stock price. Once a value is
 * entered to the application, the application writes the value to
 * the database and then displays the entire database.
 */
#define BUFSIZE 1024
int
doloop(DB_ENV *dbenv)
{
    DB *dbp;
    APP_DATA *app_data;
    DBT key, data;
    char buf[BUFSIZE], *rbuf;
    int ret;
    u_int32_t flags;

    dbp = NULL;
    ret = 0;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));
    app_data = dbenv->app_private;
```

Next we begin to modify our main loop. To start, upon entering the loop we create the database handle and configure it as normal. But we also have to decide what flags we will use for the open. Again, it depends on whether we are a replica or a master.

``` c
    for (;;) {
        if (dbp == NULL) {
            if ((ret = db_create(&dbp, dbenv, 0)) != 0)
                return (ret);

            flags = DB_AUTO_COMMIT;
            if (app_data->is_master)
                flags |= DB_CREATE; 
```

When we open the database, we modify our error handling to account for the case where the database does not yet exist. This can happen if our code is running as a replica and the Replication Manager has not yet had a chance to create the databases for us. Recall that replicas never write to their own databases directly, and so they cannot create databases on their own.

If we detect that the database does not yet exist, we simply close the database handle, sleep for a short period of time and then continue processing. This gives the Replication Manager a chance to create the database so that our replica can continue operations.

``` c
            if ((ret = dbp->open(dbp,
                NULL, DATABASE, NULL, DB_BTREE, flags, 0)) != 0) {
                if (ret == ENOENT) {
                    printf(
                      "No stock database yet available.\n");
                    if ((ret = dbp->close(dbp, 0)) != 0) {
                        dbenv->err(dbenv, ret,
                            "DB->close");
                        goto err;
                    }
                    dbp = NULL;
                    sleep(SLEEPTIME);
                    continue;
                }
                dbenv->err(dbenv, ret, "DB->open");
                goto err;
            }
        } 
```

Next we modify our prompt, so that if the local process is running as a replica, we can tell from the shell that the prompt is for a read-only process.

``` c
        printf("QUOTESERVER%s> ",
            app_data->is_master ? "" : " (read-only)");
        fflush(stdout); 
```

When we collect data from the prompt, there is a case that says if no data is entered then show the entire stocks database. This display is performed by our `print_stocks()` function (which has not required a modification since we first introduced it in <a href="simpleprogramlisting.md#printstocks_c" class="xref" title="Function: print_stocks()"><span>Function: print_stocks()</span></a> ).

When we call `print_stocks()`, we check for a dead replication handle. Dead replication handles happen whenever a replication election results in a previously committed transaction becoming invalid. This is an error scenario caused by a new master having a slightly older version of the data than the original master and so all replicas must modify their database(s) to reflect that of the new master. In this situation, some number of previously committed transactions may have to be unrolled. From the replica's perspective, the database handles should all be closed and then opened again.

``` c
        if (fgets(buf, sizeof(buf), stdin) == NULL)
            break;
        if (strtok(&buf[0], " \t\n") == NULL) {
            switch ((ret = print_stocks(dbp))) {
            case 0:
                continue;
            case DB_REP_HANDLE_DEAD:
                (void)dbp->close(dbp, DB_NOSYNC);
                dbp = NULL;
                dbenv->errx(dbenv, "Got a dead replication handle");
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

That done, we need to add a little error checking to our command prompt to make sure the user is not attempting to modify the database at a replica. Remember, replicas must never modify their local databases on their own. This guards against that happening due to user input at the prompt.

``` c
        if (!app_data->is_master) {
            dbenv->errx(dbenv, "Can't update at client");
            continue;
        }
        key.data = buf;
        key.size = (u_int32_t)strlen(buf);

        data.data = rbuf;
        data.size = (u_int32_t)strlen(rbuf);

        if ((ret = dbp->put(dbp,
            NULL, &key, &data, 0)) != 0) {
            dbp->err(dbp, ret, "DB->put");
            goto err;
        }
    }

err:    if (dbp != NULL)
        (void)dbp->close(dbp, DB_NOSYNC);

    return (ret);
} 
```

With that completed, we are all done updating our application for replication. The only remaining function, `print_stocks()`, is unmodified from when we originally introduced it. For details on that function, see <a href="simpleprogramlisting.md#printstocks_c" class="xref" title="Function: print_stocks()"><span>Function: print_stocks()</span></a> .

### Running It

To run our replicated application, we need to make sure each participating environment has its own unique home directory. We can do this by running each site on a separate networked machine, but that is not strictly necessary; multiple instances of this code can run on the same machine provided the environment home restriction is observed.

To run a process, make sure the environment home exists and then start the process using the `-h` option to specify that directory. You must also use the `-l` or `-L` option to identify the local host and port that this process will use to listen for replication messages (-L means that this is a group creator), and the `-r` option to identify the other processes in the replication group. Finally, use the `-p` option to specify a priority. The process that you designate to have the highest priority will become the master.

``` c
> mkdir env1
> ./ex_rep_gsg_repmgr -h env1 -L localhost:8080 -p 10
No stock database yet available.
No stock database yet available.  
```

Now, start another process. This time, change the environment home to something else, use the `-l` flag to at least change the port number the process is listening on, and use the `-r` option to identify the host and port of the other replication process:

``` c
> mkdir env2
> ./ex_rep_gsg_repmgr -h env2 -l localhost:8081 \
-r localhost:8080 -p 20
```

After a short pause, the second process should display the master prompt:

``` c
QUOTESERVER > 
```

And the first process should display the read-only prompt:

``` c
QUOTESERVER (read-only)> 
```

Now go to the master process and give it a couple of stocks and stock prices:

``` c
QUOTESERVER> FAKECO 9.87
QUOTESERVER> NOINC .23
QUOTESERVER> 
```

Then, go to the replica and hit **`return`** at the prompt to see the new values:

``` c
QUOTESERVER (read-only)> 
        Symbol  Price
        ======  =====
        FAKECO  9.87
        NOINC    .23 
QUOTESERVER (read-only)> 
```

Doing the same at the master results in the same thing:

``` c
QUOTESERVER> 
        Symbol  Price
        ======  =====
        FAKECO  9.87
        NOINC    .23 
QUOTESERVER> 
```

You can change a stock by simply entering the stock value and new price at the master's prompt:

``` c
QUOTESERVER> FAKECO 10.01 
QUOTESERVER> 
```

Then, go to either the master or the replica to see the updated database. On the master:

``` c
QUOTESERVER> 
        Symbol  Price
        ======  =====
        FAKECO  10.01
        NOINC    .23 
QUOTESERVER> 
```

And on the replica:

``` c
QUOTESERVER (read-only)> 
        Symbol  Price
        ======  =====
        FAKECO  10.01
        NOINC    .23 
QUOTESERVER (read-only)> 
```

Finally, to quit the applications, simply type `quit` at both prompts. On the replica:

``` c
QUOTESERVER (read-only)> quit
> 
```

And on the master as well:

``` c
QUOTESERVER> quit
> 
```
