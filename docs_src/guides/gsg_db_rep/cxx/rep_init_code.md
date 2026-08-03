---
title: "Starting and Stopping Replication"
api-name: "Starting and Stopping Replication"
source: docs/gsg_db_rep/CXX/rep_init_code.html
---
## Starting and Stopping Replication

<span class="sect2"> [Managing Election Policies](rep_init_code.md#election_flags) </span>

<span class="sect2"> [Selecting the Number of Threads](rep_init_code.md#thread_count) </span>

As described above, you introduce replication to an application by starting with a transactional application, performing some basic replication configuration, and then starting replication using `DbEnv::repmgr_start()`.

You stop replication by closing your environment cleanly in the same way you would for any DB application.

For example, the following code fragment initializes, then stops and starts replication. Note that other replication activities are omitted for brevity.

``` c
#include <db_cxx.h>

/* Use a 10mb cache */
#define CACHESIZE   (10 * 1024 * 1024)

...

    DbEnv *dbenv;            /* Environment handle. */
    DbSite *dbsite;          /* Replication manager site handle */
    const char *progname;     /* Program name. */
    const char *envHome;      /* Environment home directory. */
    const char *listen_host;  /* A TCP/IP hostname. */
    const char *other_host;   /* A TCP/IP hostname. */
    int is_group_creator;     /* A flag */
    u_int16 listen_port;      /* A TCP/IP port. */
    u_int16 other_port;       /* A TCP/IP port. */

    /* Initialize variables */
    dbenv = NULL;
    progname = "example_replication";
    envHome = "ENVIRONMENT_HOME";
    listen_host = "mymachine.sleepycat.com";
    listen_port = 5001;
    other_host = "anothermachine.sleepycat.com";
    other_port = 4555;
    is_group_creator = 1; /* This is usually set via a command line 
                             argument or some other external 
                             configuration mechanism. */

    try {
       /* Create the environment handle */
       dbenv = new DbEnv(0);

       /*
        * Configure the environment handle. Here we configure 
        * asynchronous transactional commits for performance reasons. 
        */
       dbenv->set_errfile(stderr);
       dbenv->set_errpfx(progname);
       (void)dbenv->set_cachesize(0, CACHESIZE, 0);
       (void)dbenv->set_flags(DB_TXN_NOSYNC, 1);

       /*
        * Configure the local address. This is the local hostname 
        * and port that this replication environment will use to 
        * receive incoming replication messages. Note that this can
        * be performed only once for the replication environment. 
        * It is required.

        * First: Create a DB_SITE handle to identify the site's 
        * host/port network address.
        */
        dbenv->repmgr_site(listen_host, listen_port, &dbsite;, 0);

        /*
         * Second: Configure this site as the local site within the
         * replication group.
         */
         dbsite->set_config(DB_LOCAL_SITE, 1);

        /*
         * Third: Set DB_GROUP_CREATOR if applicable. This can be done
         * only for the local site. It should also only be peformed
         * for one and only one site in a replication group, so 
         * typically this is driven by an externally-supplied
         * configuration option. 
         *
         * DB_GROUP_CREATOR only has meaning if you are starting the
         * very first site for the very first time in a replication
         * group. It is otherwise ignored.
         */
         if (is_group_creator)
            dbsite->set_config(DB_GROUP_CREATOR, 1);

        /*
         * Having configured the local site, we can immediately
         * deallocate the DB_SITE handle.
         */
         dbsite->close(dbsite);

        /*
         * Set this replication environment's priority. This is used 
         * for elections.
         *
         * Set this number to a positive integer, or 0 if you do not want
         * this site to be able to become a master.
         */
         dbenv->rep_set_priority(100);

        /*
         * Configure a bootstrap helper. This information is used only 
         * if the site currently exists, and the local site has never 
         * been started before. Otherwise, this configuration 
         * information is ignored.
         * 
         */
         if (!is_group_creator) { 
            dbenv->repmgr_site(other_host, other_port, &dbsite, 0);
            dbsite->set_config(dbsite, DB_BOOTSTRAP_HELPER, 1);

            /*
             * Having configured the bootstrap helper site, we can 
             * immediately deallocate the DB_SITE handle.
             */
             dbsite->close();
         }

         /* Open the environment handle. Note that we add DB_THREAD and
          * DB_INIT_REP to the list of flags. These are required.
          */
          dbenv->open(home, DB_CREATE | DB_RECOVER |
                            DB_INIT_LOCK | DB_INIT_LOG |
                            DB_INIT_MPOOL | DB_INIT_TXN  |
                            DB_THREAD | DB_INIT_REP,
                            0);

       /* 
        * Start the replication manager such that it uses 3 
        * threads. 
        */
         dbenv->repmgr_start(3, DB_REP_ELECTION);

       /* Sleep to give ourselves time to find a master */
        sleep(5);

       /*
        **********************************************************
        *** All other application code goes here, including  *****
        *** database opens                                   *****
        **********************************************************
        */

    } catch (DbException &de) {
        /* Error handling goes here */
    }

    /* Close out the application here.
    try {
        /* 
         * Make sure all your database handles are closed 
         *  (omitted from this example). 
         */

        /* Close the environment */
        if (dbenv != NULL)
            (void)dbenv->close(dbenv, 0);

    } catch (DbException &de) {
        /* Error handling goes here */
    }

    /* All done */ 
```

### Managing Election Policies

Before continuing, it is worth taking a look at the startup election flags accepted by `DbEnv::repgmr_start()`. These flags control how your replication application will behave when it first starts up.

In the previous example, we specified `DB_REP_ELECTION` when we started replication. This causes the application to try to find a master upon startup. If it cannot, it calls for an election. In the event an election is held, the environment receiving the most number of votes will become the master.

There's some important points to make here:

- This flag only requires that other environments in the replication group participate in the vote. There is no requirement that <span class="emphasis">*all*</span> such environments participate. In other words, if an environment starts up, it can call for an election, and select a master, even if all other environment have not yet joined the replication group.

- It only requires a simple majority of participating environments to elect a master. This is always true of elections held using the Replication Manager.

- As always, the environment participating in the election with the most up-to-date log files is selected as master. If an environment with more recent log files has not yet joined the replication group, it may not become the master.

Any one of these points may be enough to cause a less-than-optimum environment to be selected as master. Therefore, to give you a better degree of control over which environment becomes a master at application startup, the Replication Manager offers the following start-up flags:

<table data-border="1" width="80%">
<thead>
<tr>
<th>Flag</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><code class="literal">DB_REP_MASTER</code></td>
<td><p>The application starts up and declares the environment to be a master without calling for an election. It is an error for more than one environment to start up using this flag, or for an environment to use this flag when a master already exists.</p>
<p>Note that no replication group should <span class="emphasis"><em>ever</em></span> operate with more than one master.</p>
<p>In the event that a environment attempts to become a master when a master already exists, the replication code will resolve the problem by holding an election. Note, however, that there is always a possibility of data loss in the face of duplicate masters, because once a master is selected, the environment that loses the election will have to roll back any transactions committed until it is in sync with the "real" master.</p></td>
</tr>
<tr>
<td><code class="literal">DB_REP_CLIENT</code></td>
<td><p>The application starts up and declares the environment to be a replica without calling for an election. Note that the environment can still become a master if a subsequent application starts up, calls for an election, and this environment is elected master.</p></td>
</tr>
<tr>
<td><code class="literal">DB_REP_ELECTION</code></td>
<td><p>As described above, the application starts up, looks for a master, and if one is not found calls for an election.</p></td>
</tr>
</tbody>
</table>

### Selecting the Number of Threads

Under the hood, the Replication Manager is threaded and you can control the number of threads used to process messages received from other replicas. The threads that the Replication Manager uses are:

- Incoming message thread. This thread receives messages from the site's socket and passes those messages to message processing threads (see below) for handling.

- Outgoing message thread. Outgoing messages are sent from whatever thread performed a write to the database(s). That is, the thread that called, for example, `Db::put()` is the thread that writes replication messages about that fact to the socket.

  Note that if this write activity would cause the thread to be blocked due to some condition on the socket, the Replication Manager will hand the outgoing message to the incoming message thread, and it will then write the message to the socket. This prevents your database write threads from blocking due to abnormal network I/O conditions.

- Message processing threads are responsible for parsing and then responding to incoming replication messages. Typically, a response will include write activity to your database(s), so these threads can be busy performing disk I/O.

Of these threads, the only ones that you have any configuration control over are the message processing threads. In this case, you can determine how many of these threads you want to run.

It is always a bit of an art to decide on a thread count, but the short answer is you probably do not need more than three threads here, and it is likely that one will suffice. That said, the best thing to do is set your thread count to a fairly low number and then increase it if it appears that your application will benefit from the additional threads.
