---
title: "Example Processing Loop"
api-name: "Example Processing Loop"
source: docs/gsg_db_rep/CXX/exampledoloop.html
---
## Example Processing Loop

<span class="sect2"> [Running It](exampledoloop.md#runningit) </span>

In this section we take the example processing loop that we presented in the previous section and we flesh it out to provide a more complete example. We do this by updating the `doloop()` function that our original transaction application used (see <a href="simpleprogramlisting.md#doloop_cxx" class="xref" title="Method: SimpleTxn::doloop()">Method: SimpleTxn::doloop()</a>) to fully support our replicated application.

In the following example code, code that we add to the original example is presented in **`bold`**.

To begin, we include a new header file into our application so that we can check for the `ENOENT` return value later in our processing loop. We also define our `APP_DATA` structure, and we define a `sleeptime` value. Finally, we update `RepMgrGSG` to have a new method for our event notification callback, and to add a new data member for our `APP_DATA` data member.

``` c
#include <db_cxx.h>
#include <iostream>
#include <errno.h>

...
// Skipping all the RepHostInfoObj and RepConfigInfo code, which does not
// change.
...

using std::cout;
using std::cin;
using std::cerr;
using std::endl;
using std::flush;

#define CACHESIZE   (10 * 1024 * 1024)
#define DATABASE    "quote.db"
#define SLEEPTIME   3

const char *progname = "RepMgrGSG";

// Struct used to store information in Db app_private field.
typedef struct {
    int is_master;
} APP_DATA;

class RepMgrGSG
{
public:
    // Constructor.
    RepMgrGSG();
    // Initialization method. Creates and opens our environment handle.
    int init(RepConfigInfo* config);
    // The doloop is where all the work is performed.
    int doloop();
    // terminate() provides our shutdown code.
    int terminate();

    // event notification callback
    static void 
    event_callback(DbEnv * dbenv, u_int32_t which, void *info);

private:
    // disable copy constructor.
    RepMgrGSG(const RepMgrGSG &);
    void operator = (const RepMgrGSG &);

    // internal data members.
    APP_DATA        app_data;
    RepConfigInfo   *app_config;
    DbEnv           dbenv;

    // private methods.
    // print_stocks() is used to display the contents of our database.
    static int print_stocks(Db *dbp);
}; 
```

That done, we can skip the `main()` method, because it does not change. Instead, we skip down to our `RepMgrGSG` constructor where we initialize our `APP_DATA is_master` data member:

``` c
RepMgrGSG::RepMgrGSG() : app_config(0), dbenv(0)
{
    app_data.is_master = 0; // assume I start out as client
}
```

That done, we must also update `RepMgrGSG::init()` to do a couple of things. First, we need to register our event callback with the environment handle. We also need to make our `APP_DATA` data member available through our environment handle's `app_private` field. This is a fairly trivial update, and it happens at the top of the method (we skip the rest of the method's listing since it does not change):

``` c
int RepMgrGSG::init(RepConfigInfo *config)
{
    int ret = 0;

    app_config = config;

    dbenv.set_errfile(stderr);
    dbenv.set_errpfx(progname);
    dbenv.set_app_private(&app_data);
    dbenv.set_event_notify(event_callback);

    ...  
```

That done, we need to implement our `event_callback()` callback. Note that what we use here is no different from the callback that we described in the previous section. However, for the sake of completeness we provide the implementation here again.

``` c
        
          /*
 * A callback used to determine whether the local environment is a 
 * replica or a master. This is called by the Replication Manager
 * when the local replication environment changes state.
 */
void RepMgrGSG::event_callback(DbEnv *dbenv, u_int32_t which, void *info)
{
    APP_DATA *app = dbenv->get_app_private();

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

That done, we need to update our `doloop()` method.

We begin by updating our database handle open flags to determine which flags to use, depending on whether the application is running as a master.

``` c
#define BUFSIZE 1024
int RepMgrGSG::doloop()
{
    Db *dbp;
    Dbt key, data;
    char buf[BUFSIZE], *rbuf;
    int ret;

    dbp = 0;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));
    ret = 0; 

    for (;;) {
        if (dbp == 0) {
            dbp = new Db(&dbenv, 0);

            try {
                dbp->open(NULL, DATABASE, NULL, DB_BTREE,
                    app_data.is_master ? DB_CREATE | DB_AUTO_COMMIT :
                    DB_AUTO_COMMIT, 0); 
```

When we open the database, we modify our error handling to account for the case where the database does not yet exist. This can happen if our code is running as a replica and the Replication Manager has not yet had a chance to create the databases for us. Recall that replicas never write to their own databases directly, and so they cannot create databases on their own.

If we detect that the database does not yet exist, we simply close the database handle, sleep for a short period of time and then continue processing. This gives the Replication Manager a chance to create the database so that our replica can continue operations.

``` c
            } catch(DbException dbe) {
                /* It is expected that this condition will be triggered
                 * when client sites start up.
                 * It can take a while for the master site to be found
                 * and synced, and no DB will be available until then.
                 */
                if (dbe.get_errno() == ENOENT) {
                    cout << "No stock db available yet - "
                         << "retrying." << endl;
                    try {
                        dbp->close(0);
                    } catch (DbException dbe2) {
                        cout << "Unexpected error closing after failed"
                             << " open, message: " << dbe2.what() << endl;
                        dbp = NULL;
                        goto err;
                    }
                    dbp = NULL;
                    sleep(SLEEPTIME);
                    continue;
                } else {
                    dbenv.err(ret, "DB->open");
                    throw dbe;
                }
            }
        } 
```

Next we modify our prompt, so that if the local process is running as a replica, we can tell from the shell that the prompt is for a read-only process.

``` c
        cout << "QUOTESERVER" ;
        if (!app_data.is_master)
            cout << "(read-only)";
        cout << "> " << flush; 
```

When we collect data from the prompt, there is a case that says if no data is entered then show the entire stocks database. This display is performed by our `print_stocks()` method (which has not required a modification since we first introduced it in <a href="simpleprogramlisting.md#printstocks_c" class="xref" title="Method: SimpleTxn::print_stocks()"><span>Method: SimpleTxn::print_stocks()</span></a> ).

When we call `print_stocks()`, we check for a dead replication handle. Dead replication handles happen whenever a replication election results in a previously committed transaction becoming invalid. This is an error scenario caused by a new master having a slightly older version of the data than the original master and so all replicas must modify their database(s) to reflect that of the new master. In this situation, some number of previously committed transactions may have to be unrolled. From the replica's perspective, the database handles should all be closed and then opened again.

``` c
        if (fgets(buf, sizeof(buf), stdin) == NULL)
            break;
        if (strtok(&buf[0], " \t\n") == NULL) {
            switch ((ret = print_stocks(dbp))) {
            case 0:
                continue;
            case DB_REP_HANDLE_DEAD:
                (void)dbp->close(DB_NOSYNC);
                cout << "closing db handle due to rep handle dead" << endl;
                dbp = NULL;
                continue;
            default:
                dbp->err(ret, "Error traversing data");
                goto err;
            }
        }
        rbuf = strtok(NULL, " \t\n");
        if (rbuf == NULL || rbuf[0] == '\0') {
            if (strncmp(buf, "exit", 4) == 0 ||
                strncmp(buf, "quit", 4) == 0)
                break;
            dbenv.errx("Format: TICKER VALUE");
            continue;
        } 
```

That done, we need to add a little error checking to our command prompt to make sure the user is not attempting to modify the database at a replica. Remember, replicas must never modify their local databases on their own. This guards against that happening due to user input at the prompt.

``` c
        if (!app_data.is_master) {
            dbenv->errx(dbenv, "Can't update at client");
            continue;
        }

        key.set_data(buf);
        key.set_size((u_int32_t)strlen(buf));

        data.set_data(rbuf);
        data.set_size((u_int32_t)strlen(rbuf));

        if ((ret = dbp->put(NULL, &key, &data, 0)) != 0)
        {
            dbp->err(ret, "DB->put");
            if (ret != DB_KEYEXIST)
                goto err;
        }
    }

err:    if (dbp != 0)
        (void)dbp->close(dbp, DB_NOSYNC);

    return (ret);
} 
```

With that completed, we are all done updating our application for replication. The only remaining method, `print_stocks()`, is unmodified from when we originally introduced it. For details on that function, see <a href="simpleprogramlisting.md#printstocks_c" class="xref" title="Method: SimpleTxn::print_stocks()"><span>Method: SimpleTxn::print_stocks()</span></a> .

### Running It

To run our replicated application, we need to make sure each participating environment has its own unique home directory. We can do this by running each site on a separate networked machine, but that is not strictly necessary; multiple instances of this code can run on the same machine provided the environment home restriction is observed.

To run a process, make sure the environment home exists and then start the process using the `-h` option to specify that directory. You must also use the `-l` or `-L` option to identify the local host and port that this process will use to listen for replication messages (-L means that this is a group creator), and the `-r` option to identify the other processes in the replication group. Finally, use the `-p` option to specify a priority. The process that you designate to have the highest priority will become the master.

``` c
> mkdir env1
> ./RepMgrGSG -h env1 -L localhost:8080 -p 10
No stock database yet available.
No stock database yet available.  
```

Now, start another process. This time, change the environment home to something else, use the `-l` flag to at least change the port number the process is listening on, and use the `-r` option to identify the host and port of the other replication process:

``` c
> mkdir env2
> ./RepMgrGSG -h env2 -l localhost:8081 -r localhost:8080 -p 20
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
