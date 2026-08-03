---
title: "Program Listing"
api-name: "Program Listing"
source: docs/gsg_db_rep/CXX/simpleprogramlisting.html
---
## Program Listing

<span class="sect2"> [Class: RepConfigInfo](simpleprogramlisting.md#repconfiginfo_cxx) </span>

<span class="sect2"> [Class: excxx_repquote_gsg_simple](simpleprogramlisting.md#repmgr_cxx) </span>

<span class="sect2"> [Function: usage()](simpleprogramlisting.md#usage_cxx) </span>

<span class="sect2"> [Function: main()](simpleprogramlisting.md#main_cxx) </span>

<span class="sect2"> [Method: SimpleTxn::init()](simpleprogramlisting.md#repmgr_init_cxx) </span>

<span class="sect2"> [Method: SimpleTxn::doloop()](simpleprogramlisting.md#doloop_cxx) </span>

<span class="sect2"> [Method: SimpleTxn::print_stocks()](simpleprogramlisting.md#printstocks_c) </span>

Our example program is a fairly simple transactional application. At this early stage of its development, the application contains no hint that it must be network-aware so the only command line argument that it takes is one that allows us to specify the environment home directory. (Eventually, we will specify things like host names and ports from the command line).

Note that the application performs all writes under the protection of a transaction; however, multiple database operations are not performed per transaction. Consequently, we simplify things a bit by using autocommit for our database writes.

Also, this application is single-threaded. It is possible to write a multi-threaded or multi-process application that performs replication. That said, the concepts described in this book are applicable to both single threaded and multi-threaded applications so nothing is gained by multi-threading this application other than distracting complexity. This manual does, however, identify where care must be taken when performing replication with a non-single threaded application.

Finally, remember that transaction processing is not described in this manual. Rather, see the *Berkeley DB Getting Started with Transaction Processing* guide for details on that topic.

### Class: RepConfigInfo

Before we begin, we present a class that we will use to maintain useful information for us. Under normal circumstances, this class would not be necessary for a simple transactional example such as this. However, this code will grow into a replicated example that needs to track a lot more information for the application, and so we lay the groundwork for it here.

The class that we create is called `RepConfigInfo` and its only purpose at this time is to track the location of our environment home directory.

``` c
#include <db_cxx.h>

class RepConfigInfo {
public:
    RepConfigInfo();
    virtual ~RepConfigInfo();

public:
    char* home;
};

RepConfigInfo::RepConfigInfo()
{
    home = "TESTDIR";
}

RepConfigInfo::~RepConfigInfo()
{
}  
```

### Class: excxx_repquote_gsg_simple

Our transactional example will instantiate a class, `SimpleTxn`, that performs all our work for us. Before we implement our `main()` function, we show the `SimpleTxn` class declaration.

First, we provide some declarations and definitions that are needed later in our example:

``` c
#include <iostream>
#include <db_cxx.h>
#include "RepConfig.h"

using std::cout;
using std::cin;
using std::cerr;
using std::endl;
using std::flush;

#define CACHESIZE   (10 * 1024 * 1024)
#define DATABASE    "quote.db"

const char *progname = "excxx_reqquote_gsg_simple"; 

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <direct.h>

extern "C" {
  extern int getopt(int, char * const *, const char *);
  extern char *optarg;
}
#else
#include <errno.h>
#endif  
```

And then we define our `SimpleTxn` class:

``` c
class SimpleTxn
{
public:
    // Constructor.
    SimpleTxn();
    // Initialization method. Creates and opens our environment handle.
    int init(RepConfigInfo* config);
    // The doloop is where all the work is performed.
    int doloop();
    // terminate() provides our shutdown code.
    int terminate();

private:
    // disable copy constructor.
    SimpleTxn(const SimpleTxn &);
    void operator = (const SimpleTxn &);

    // internal data members.
    RepConfigInfo   *app_config;
    DbEnv           dbenv;

    // private methods.
    // print_stocks() is used to display the contents of our database.
    static int print_stocks(Db *dbp);
};  
```

Note that we show the implementation of the various `SimpleTxn` methods later in this section.

### Function: usage()

Our `usage()` is at this stage of development trivial because we only have one command line argument to manage. Still, we show it here for the sake of completeness.

``` c
static void usage()
{
    cerr << "usage: " << progname << endl
         << "-h home" << endl;

    exit(EXIT_FAILURE);
}  
```

### Function: main()

Now we provide our `main()` function. This is a trivial function whose only job is to collect command line information, then instantiate a `SimpleTxn` object, run it, then terminate it.

We begin by declaring some useful variables. Of these, note that we instantiate our `RepConfigInfo` object here. Recall that this is used to store information useful to our code. This class becomes more interesting later in this book.

``` c
int main(int argc, char **argv)
{
    RepConfigInfo config;
    char ch;
    int ret;  
```

Then we collect our command line information. Again, this is at this point fairly trivial:

``` c
    // Extract the command line parameters
    while ((ch = getopt(argc, argv, "h:")) != EOF) {
        switch (ch) {
        case 'h':
            config.home = optarg;
            break;
        case '?':
        default:
            usage();
        }
    }

    // Error check command line.
    if (config.home == NULL)
        usage();  
```

Now we instantiate and initialize our `SimpleTxn` class, which is what is responsible for doing all our real work. The `SimpleTxn::init()` method creates and opens our environment handle.

``` c
    SimpleTxn runner;
    try {
        if((ret = runner.init(&config)) != 0)
            goto err;  
```

Then we call the `SimpleTxn::doloop()` method, which is where the actual transactional work is performed for this application.

``` c
        if((ret = runner.doloop()) != 0)
            goto err;  
```

Finally, catch exceptions and terminate the program:

``` c
    } catch (DbException dbe) {
        cerr << "Caught an exception during initialization or"
            << " processing: " << dbe.what() << endl;
    }
err:
    runner.terminate();
    return 0;
}  
```

### Method: SimpleTxn::init()

The `SimpleTxn::init()` method is used to create and open our environment handle. For readers familiar with writing transactional DB applications, there should be no surprises here. However, we will be adding to this in later chapters as we roll replication into this example.

First, we show the class constructor implementation, which is only used to initialize a few variables:

``` c
SimpleTxn::SimpleTxn() : app_config(0), dbenv(0)
{
}  
```

We now provide the `init()` method implementation. The only thing of interest here is that we specify `DB_TXN_NOSYNC` to our environment. This causes our transactional commits to become non-durable, which is something that we are doing only because of the nature of our example.

``` c
int SimpleTxn::init(RepConfigInfo *config)
{
    int ret = 0;

    app_config = config;

    dbenv.set_errfile(stderr);
    dbenv.set_errpfx(progname);

    /*
     * We can now open our environment.
     */
    dbenv.set_cachesize(0, CACHESIZE, 0);
    dbenv.set_flags(DB_TXN_NOSYNC, 1);

    try {
        dbenv.open(app_config->home, 
            DB_CREATE | 
            DB_RECOVER |
            DB_INIT_LOCK | 
            DB_INIT_LOG |
            DB_INIT_MPOOL | 
            DB_INIT_TXN, 
            0);
    } catch(DbException dbe) {
        cerr << "Caught an exception during DB environment open." << endl
            << "Ensure that the home directory is created prior to"
            << " starting the application." << endl;
        ret = ENOENT;
        goto err;
    }

err:
    return ret;
}  
```

Finally, we present the `SimpleTxn::terminate()` method here. All this does is close the environment handle. Again, there should be no surprises here, but we provide the implementation for the sake of completeness anyway.

``` c
int SimpleTxn::terminate()
{
    try {
        dbenv.close(0);
    } catch (DbException dbe) {
        cerr << "error closing environment: " << dbe.what() << endl;
    }
    return 0;
}  
```

### Method: SimpleTxn::doloop()

Having written our `main()` function and support utility methods, we now implement our application's primary data processing method. This method provides a command prompt at which the user can enter a stock ticker value and a price for that value. This information is then entered to the database.

To display the database, simply enter `return` at the prompt.

To begin, we declare a database pointer, several `Dbt` variables, and the usual assortment of variables used for buffers and return codes. We also initialize all of this.

``` c
#define BUFSIZE 1024
int SimpleTxn::doloop()
{
    Db *dbp;
    Dbt key, data;
    char buf[BUFSIZE], *rbuf;
    int ret;

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
            dbp = new Db(&dbenv, 0);

            try {
                dbp->open(NULL, DATABASE, NULL, DB_BTREE,
                    DB_CREATE | DB_AUTO_COMMIT, 0);
            } catch(DbException dbe) {
                dbenv.err(ret, "DB->open");
                throw dbe;
            }
        }  
```

Now we implement our command prompt. This is a simple and not very robust implementation of a command prompt. If the user enters the keywords `exit` or `quit`, the loop is exited and the application ends. If the user enters nothing and instead simply presses `return`, the entire contents of the database is displayed. We use our `print_stocks()` method to display the database. (That implementation is shown next in this chapter.)

Notice that very little error checking is performed on the data entered at this prompt. If the user fails to enter at least one space in the value string, a simple help message is printed and the prompt is returned to the user. That is the only error checking performed here. In a real-world application, at a minimum the application would probably check to ensure that the price was in fact an integer or float value. However, in order to keep this example code as simple as possible, we refrain from implementing a thorough user interface.

``` c
        cout << "QUOTESERVER" ;
        cout << "> " << flush;

        if (fgets(buf, sizeof(buf), stdin) == NULL)
            break;
        if (strtok(&buf[0], " \t\n") == NULL) {
            switch ((ret = print_stocks(dbp))) {
            case 0:
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

Now we assign data to the `Dbt`s that we will use to write the new information to the database.

``` c
        key.set_data(buf);
        key.set_size((u_int32_t)strlen(buf));

        data.set_data(rbuf);
        data.set_size((u_int32_t)strlen(rbuf));  
```

Having done that, we can write the new information to the database. Remember that this application uses autocommit, so no explicit transaction management is required. Also, the database is not configured for duplicate records, so the data portion of a record is overwritten if the provided key already exists in the database. However, in this case DB returns `DB_KEYEXIST` — which we ignore.

``` c
        if ((ret = dbp->put(NULL, &key, &data, 0)) != 0)
        {
            dbp->err(ret, "DB->put");
            if (ret != DB_KEYEXIST)
                goto err;
        }
    }  
```

Finally, we close our database before returning from the method.

``` c
err:    if (dbp != NULL) {
        (void)dbp->close(DB_NOSYNC);
        cout << "database closed" << endl;
        }

    return (ret);
}  
```

### Method: SimpleTxn::print_stocks()

The `print_stocks()` method simply takes a database handle, opens a cursor, and uses it to display all the information it finds in a database. This is trivial cursor operation that should hold no surprises for you. We simply provide it here for the sake of completeness.

If you are unfamiliar with basic cursor operations, please see the *Getting Started with Berkeley DB* guide.

``` c
int SimpleTxn::print_stocks(Db *dbp)
{
    Dbc *dbc;
    Dbt key, data;
#define MAXKEYSIZE  10
#define MAXDATASIZE 20
    char keybuf[MAXKEYSIZE + 1], databuf[MAXDATASIZE + 1];
    int ret, t_ret;
    u_int32_t keysize, datasize;

    if ((ret = dbp->cursor(NULL, &dbc, 0)) != 0) {
        dbp->err(ret, "can't open cursor");
        return (ret);
    }

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    cout << "\tSymbol\tPrice" << endl
        << "\t======\t=====" << endl;

    for (ret = dbc->get(&key, &data, DB_FIRST);
        ret == 0;
        ret = dbc->get(&key, &data, DB_NEXT)) {
        keysize = key.get_size() > MAXKEYSIZE ? MAXKEYSIZE : 
                                   key.get_size();
        memcpy(keybuf, key.get_data(), keysize);
        keybuf[keysize] = '\0';

        datasize = data.get_size() >=
            MAXDATASIZE ? MAXDATASIZE : data.get_size();
        memcpy(databuf, data.get_data(), datasize);
        databuf[datasize] = '\0';

        cout << "\t" << keybuf << "\t" << databuf << endl;
    }
    cout << endl << flush;

    if ((t_ret = dbc->close()) != 0 && ret == 0) {
        cout << "closed cursor" << endl;
        ret = t_ret;
    }

    switch (ret) {
    case 0:
    case DB_NOTFOUND:
        return (0);
    default:
        return (ret);
    }
}  
```
