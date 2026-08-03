---
title: "Adding the Replication Manager to ex_rep_gsg_simple"
api-name: "Adding the Replication Manager to ex_rep_gsg_simple"
source: docs/gsg_db_rep/C/repmgr_init_example_c.html
---
## Adding the Replication Manager to ex_rep_gsg_simple

We now use the methods described above to add partial support to the ex_rep_gsg_simple example that we presented in <a href="txnapp.md" class="xref" title="Chapter 2. Transactional Application">Transactional Application</a>. That is, in this section we will:

- Enhance our command line options to accept information of interest to a replicated application.

- Configure our environment handle to use replication and the Replication Manager.

- Minimally configure the Replication Manager.

- Start replication.

Note that when we are done with this section, we will be only partially ready to run the application. Some critical pieces will be missing; specifically, we will not yet be handling the differences between a master and a replica. (We do that in the next chapter).

Also, note that in the following code fragments, additions and changes to the code are marked in **`bold`**.

To begin, we copy the ex_rep_gsg_simple code to a new file called `ex_rep_gsg_repmgr.c`. We then make the corresponding change to the program name.

``` c
/*
 * File: ex_rep_gsg_repmgr.c
 */

#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <unistd.h>
#endif

#include <db.h>

#ifdef _WIN32
extern int getopt(int, char * const *, const char *);
#endif

#define CACHESIZE   (10 * 1024 * 1024)
#define DATABASE    "quote.db"

const char *progname = "ex_rep_gsg_repmgr";

int create_env(const char *, DB_ENV **);
int env_init(DB_ENV *, const char *);
int doloop (DB_ENV *);
int print_stocks(DBC *); 
```

Next we update our usage function. The application will continue to accept the `-h` parameter so that we can identify the environment home directory used by this application. However, we also add the:

- `-l` parameter which allows us to identify the host and port used by this application to listen for replication messages. This parameter is required unless the -L parameter is specified.

- `-L` parameter, which allows us to identify the local site as the group creator.

- `-r` parameter which allows us to specify other replicas.

- `-p` option, which is used to identify this replica's priority (recall that the priority is used as a tie breaker for elections)

``` c
/* Usage function */
static void
usage()
{
  fprintf(stderr, "usage: %s ", progname);
  fprintf(stderr, "-h home -l|-L host:port\n");
  fprintf(stderr, "\t\t[-r host:port][-p priority]\n");
  fprintf(stderr, "where:\n");
  fprintf(stderr, "\t-h identifies the environment home directory ");
  fprintf(stderr, "(required).\n");
  fprintf(stderr, "\t-l identifies the host and port used by this ");
  fprintf(stderr, "site (required, unless -L is specified).\n");
  fprintf(stderr, "\t-L identifies the host and port used by this ");
  fprintf(stderr, "site, which is the group creator.\n");
  fprintf(stderr, "\t-r identifies another site participating in ");
  fprintf(stderr, "this replication group\n");
  fprintf(stderr, "\t-p identifies the election priority used by ");
  fprintf(stderr, "this replica.\n");
  exit(EXIT_FAILURE);
} 
```

Now we can begin working on our `main()` function. We begin by adding a couple of variables that we will use to collect TCP/IP host and port information. We also declare a couple of flags that we use to make sure some required information is provided to this application.

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

    dbenv = NULL;

    ret = local_is_set = is_group_creator = 0;
    home = NULL; 
```

At this time we can create our environment handle and configure it exactly as we did for `simple_txn`. The only thing that we will do differently here is that we will set a priority, arbitrarily picked to be 100, so that we can be sure the environment has a priority other than 0 (the default value). This ensures that the environment can become a master via an election.

``` c
    if ((ret = create_env(progname, &dbenv)) != 0)
            goto err;

    /* Default priority is 100 */
    dbenv->rep_set_priority(dbenv, 100);   
```

Now we collect our command line arguments. As we do so, we will configure host and port information as required, and we will configure the application's election priority if necessary.

``` c
    /* Collect the command line options */
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
```

Having done that, we can call `env_init()`, which we use to open our environment handle. Note that this function changes slightly for this update (see below).

``` c
    if ((ret = env_init(dbenv, home)) != 0)
        goto err; 
```

Finally, we start replication before we go into the `doloop()` function (where we perform all our database access).

``` c
    if ((ret = dbenv->repmgr_start(dbenv, 3, DB_REP_ELECTION)) != 0)
        goto err; 

    if ((ret = doloop(dbenv)) != 0) {
        dbenv->err(dbenv, ret, "Application failed");
        goto err;
    }

err: if (dbenv != NULL)
        (void)dbenv->close(dbenv, 0);

    return (ret);
} 
```

Beyond that, the rest of our application remains the same for now, with the exception of the `env_init()` function, which we use to actually open our environment handle. The flags we use to open the environment are slightly different for a replicated application than they are for a non-replicated application. Namely, replication requires the `DB_INIT_REP` flag.

Also, because we are using the Replication Manager, we must prepare our environment for threaded usage. For this reason, we also need the `DB_THREAD` flag.

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
            DB_INIT_REP |
            DB_INIT_TXN | 
            DB_RECOVER |
            DB_THREAD;
    if ((ret = dbenv->open(dbenv, home, flags, 0)) != 0)
        dbenv->err(dbenv, ret, "can't open environment");
    return (ret);
}
```

This completes our replication updates for the moment. We are not as yet ready to actually run this program; there remains a few critical pieces left to add to it. However, the work that we performed in this section represents a solid foundation for the remainder of our replication work.
