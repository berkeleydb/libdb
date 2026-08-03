---
title: "Adding the Replication Manager to RepMgr"
api-name: "Adding the Replication Manager to RepMgr"
source: docs/gsg_db_rep/CXX/repmgr_init_example_c.html
---
## Adding the Replication Manager to RepMgr

We now use the methods described above to add partial support to the RepMgr example that we presented in <a href="txnapp.md" class="xref" title="Chapter 2. Transactional Application">Transactional Application</a>. That is, in this section we will:

- Enhance our command line options to accept information of interest to a replicated application.

- Configure our environment handle to use replication and the Replication Manager.

- Minimally configure the Replication Manager.

- Start replication.

Note that when we are done with this section, we will be only partially ready to run the application. Some critical pieces will be missing; specifically, we will not yet be handling the differences between a master and a replica. (We do that in the next chapter).

Also, note that in the following code fragments, additions and changes to the code are marked in **`bold`**.

To begin, we copy the SimpleTxn code to a new file called `RepMgrGSG.cpp`. Having done that, we must make some significant changes to our `RepConfigInfo` class because now we will be using it to maintain a lot more information.

First, we create a new structure, `RepHostInfoObj`, which we use to store host and port information for all "other" servers identified to the application via the `-o` command line option. This structure is chain-able, which makes cleaning up at program shutdown time easier.

``` c
#include <db_cxx.h>
#include <iostream>

// Chain-able struct used to store host information.
typedef struct RepHostInfoObj{
    char* host;
    u_int16_t port;
    RepHostInfoObj* next; // used for chaining multiple "other" hosts.
    bool creator;         // whether this site is the group creator.
} REP_HOST_INFO; 
```

Next, we update our `RepConfigInfo` class definition to manage a lot more information and a new method.

``` c
class RepConfigInfo {
public:
    RepConfigInfo();
    virtual ~RepConfigInfo();

    void addOtherHost(char* host, int port);
public:
    u_int32_t start_policy;
    char* home;
    bool got_listen_address;
    REP_HOST_INFO this_host;
    int nrsites;    // number of remote sites
    int priority;
    // used to store a set of optional other hosts.
    REP_HOST_INFO *other_hosts;
}; 
```

Then, we update our constructor to initialize our new variables.

``` c
RepConfigInfo::RepConfigInfo()
{
    start_policy = DB_REP_ELECTION;
    home = "TESTDIR";
    got_listen_address = false;
    nrsites = 0;
    priority = 100;
    other_hosts = NULL;
} 
```

Next, we implement our new method, `RepConfigInfo::addOtherHost`, which is used to create `RepHostInfoObj` instances and add them to the chain of "other" hosts.

``` c
RepConfigInfo::addOtherHost(char* host, int port)
{
    REP_HOST_INFO *newinfo;
    newinfo = (REP_HOST_INFO*)malloc(sizeof(REP_HOST_INFO));
    newinfo->host = host;
    newinfo->port = port;
    if (other_hosts == NULL) {
        other_hosts = newinfo;
        newinfo->next = NULL;
    } else {
        newinfo->next = other_hosts;
        other_hosts = newinfo;
    }
    nrsites++;
}
```

Having done that, we update our class destructor to release the `RepHostInfoObj` chain of objects at class destruction time.

``` c
RepConfigInfo::~RepConfigInfo()
{
    // release any other_hosts structs.
    if (other_hosts != NULL) {
        REP_HOST_INFO *CurItem = other_hosts;
        while (CurItem->next != NULL)
        {
            REP_HOST_INFO *TmpItem = CurItem;
            free(CurItem);
            CurItem = TmpItem;
        }
        free(CurItem);
    }
    other_hosts = NULL;
} 
```

Having completed our update to the `RepConfigInfo` class, we can now start making changes to the main portion of our program. We begin by changing the program's name.

``` c
using std::cout;
using std::cin;
using std::cerr;
using std::endl;
using std::flush;
                
#define CACHESIZE   (10 * 1024 * 1024)
#define DATABASE    "quote.db"
                
const char *progname = "RepMgrGSG"; 
```

Next we update our usage function. The application will continue to accept the `-h` parameter so that we can identify the environment home directory used by this application. However, we also add the:

- `-l` parameter which allows us to identify the host and port used by this application to listen for replication messages. This parameter is required unless the -L parameter is specified.

- `-L` parameter, which allows us to identify the local site as the group creator.

- `-r` parameter which allows us to specify other replicas.

- `-p` option, which is used to identify this replica's priority (recall that the priority is used as a tie breaker for elections)

``` c
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

private:
    // disable copy constructor.
    RepMgrGSG(const RepMgrGSG &);
    void operator = (const RepMgrGSG &);

    // internal data members.
    RepConfigInfo   *app_config;
    DbEnv           dbenv;

    // private methods.
    // print_stocks() is used to display the contents of our database.
    static int print_stocks(Db *dbp);
};

static void usage()
{
    cerr << "usage: " << progname << endl
         << "-h home-l|L host:port [-r host:port]"
         << "[-p priority]" << endl;

    cerr << "\t -h home directory" << endl
         << "\t -l host:port (required unless -L is specified;"
         << "\t   l stands for local)" << endl
         << "\t -L host:port (optional, L means group "
         << "   creator)" << endl
         << "\t -r host:port (optional; r stands for replica; any "
         << "number of these may be specified)" << endl
         << "\t -p priority (optional: defaults to 100)" << endl;

    exit(EXIT_FAILURE);
} 
```

Now we can begin working on our `main()` function. We begin by adding a couple of variables that we will use to collect TCP/IP host and port information.

``` c
int main(int argc, char **argv)
{
    RepConfigInfo config;
    char ch, *portstr, *tmphost;
    int tmpport;
    int ret; 
```

Now we collect our command line arguments. As we do so, we will configure host and port information as required, and we will configure the application's election priority if necessary.

``` c
    // Extract the command line parameters
    while ((ch = getopt(argc, argv, "h:l:n:p:r:")) != EOF) {
        switch (ch) {
        case 'h':
            config.home = optarg;
            break;
        case 'L':
            config.this_host.creator = true; // FALLTHROUGH
        case 'l':
            config.this_host.host = strtok(optarg, ":");
            if ((portstr = strtok(NULL, ":")) == NULL) {
                cerr << "Bad host specification." << endl;
                usage();
            }
            config.this_host.port = (unsigned short)atoi(portstr);
            config.got_listen_address = true;
            break;
        case 'p':
            config.priority = atoi(optarg);
            break;
        case 'r':
            tmphost = strtok(optarg, ":");
            if ((portstr = strtok(NULL, ":")) == NULL) {
                cerr << "Bad host specification." << endl;
                usage();
            }
            tmpport = (unsigned short)atoi(portstr);
            config.addOtherHost(tmphost, tmpport);
            break;
        case '?':
        default:
            usage();
        }
    }

    // Error check command line.
    if ((!config.got_listen_address) || config.home == NULL)
        usage(); 
```

Having done that, the remainder of our `main()` function is left unchanged:

``` c
    RepMgrGSG runner;
    try {
        if((ret = runner.init(&config)) != 0)
            goto err;
        if((ret = runner.doloop()) != 0)
            goto err;
    } catch (DbException dbe) {
        cerr << "Caught an exception during initialization or"
            << " processing: " << dbe.what() << endl;
    }
err:
    runner.terminate();
    return 0;
}  
```

Now we need to update our `RepMgrGSG::init()` method. Our updates are at first related to configuring replication. First, we need to update the method so that we can identify the local site to the environment handle (that is, the site identified by the `-l` command line option):

``` c
RepMgrGSG::RepMgrGSG() : app_config(0), dbenv(0)
{
}

int RepMgrGSG::init(RepConfigInfo *config)
{
    int ret = 0;

    app_config = config;

    dbenv.set_errfile(stderr);
    dbenv.set_errpfx(progname);

    DbSite *dbsite;
    dbenv.repmgr_site(app_config->this_host.host,
        app_config->this_host.port, &dbsite, 0);
    dbsite->set_config(DB_LOCAL_SITE, 1);
    if (app_config->this_host.creator)
        dbsite->set_config(DB_GROUP_CREATOR, 1);

    dbsite->close(); 
```

And we also add code to allow us to identify "other" sites to the environment handle (that is, the sites that we identify using the `-o` command line option). To do this, we iterate over each of the "other" sites provided to us using the `-o` command line option, and we add each one individually in turn:

We also add code here to set the environment's priority.

``` c
    int i = 1;
    for ( REP_HOST_INFO *cur = app_config->other_hosts; 
        cur != NULL && i <= app_config->nrsites;
        cur = cur->next, i++) {
            dbenv.repmgr_site(cur->host, cur->port, &dbsite, 0);
            dbsite->set_config(DB_BOOTSTRAP_HELPER, 1);

            dbsite->close();
    } 
    dbenv.rep_set_priority(app_config->priority);  
```

We can now open our environment. Note that the flags we use to open the environment are slightly different for a replicated application than they are for a non-replicated application. Namely, replication requires the `DB_INIT_REP` flag.

Also, because we are using the Replication Manager, we must prepare our environment for threaded usage. For this reason, we also need the `DB_THREAD` flag.

``` c
    dbenv.set_cachesize(0, CACHESIZE, 0);
    dbenv.set_flags(DB_TXN_NOSYNC, 1);

    try {
        dbenv.open(app_config->home, 
            DB_CREATE | 
            DB_INIT_LOCK | 
            DB_INIT_LOG | 
            DB_INIT_MPOOL |
            DB_INIT_REP |
            DB_INIT_TXN | 
            DB_RECOVER |
            DB_THREAD;
            0);
    } catch(DbException dbe) {
        cerr  << "Caught an exception during DB environment open." << endl
              << "Ensure that the home directory is created prior to "
              << "starting the application." << endl;
        ret = ENOENT;
        goto err;
    }
```

Finally, we start replication before we exit this method. Immediately after exiting this method, our application will go into the `RepMgrGSG::doloop()` method, which is where the bulk of our application's work is performed. We update that method in the next chapter.

``` c
    if ((ret = dbenv.repmgr_start(3, app_config->start_policy)) != 0)
        goto err;

err:
    return ret;
} 
```

This completes our replication updates for the moment. We are not as yet ready to actually run this program; there remains a few critical pieces left to add to it. However, the work that we performed in this section represents a solid foundation for the remainder of our replication work.
