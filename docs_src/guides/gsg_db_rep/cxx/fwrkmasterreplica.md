---
title: "Chapter 4. Replica versus Master Processes"
api-name: "Chapter 4. Replica versus Master Processes"
source: docs/gsg_db_rep/CXX/fwrkmasterreplica.html
---
## Chapter 4. Replica versus Master Processes

**Table of Contents**

<span class="sect1"> [Determining State](fwrkmasterreplica.md#determinestate) </span>

<span class="sect1"> [Processing Loop](processingloop.md) </span>

<span class="sect1"> [Example Processing Loop](exampledoloop.md) </span>

<span class="sect2"> [Running It](exampledoloop.md#runningit) </span>

Every environment participating in a replicated application must know whether it is a <span class="emphasis">*master*</span> or <span class="emphasis">*replica*</span>. The reason for this is because, simply, the master can modify the database while replicas cannot. As a result, not only will you open databases differently depended on whether the environment is running as a master, but the environment will frequently behave quite a bit differently depending on whether it thinks it is operating as the read/write interface for your database.

Moreover, an environment must also be capable of gracefully switching between master and replica states. This means that the environment must be able to detect when it has switched states.

Not surprisingly, a large part of your application's code will be tied up in knowing which state a given environment is in and then in the logic of how to behave depending on its state.

This chapter shows you how to determine your environment's state, and it then shows you some sample code on how an application might behave depending on whether it is a master or a replica in a replicated application.

## Determining State

In order to determine whether your code is running as a master or a replica, you implement a callback whose function it is to respond to events that happen within the DB library. Note that these events are raised whenever the state is established. For example, when the current environment becomes a client — including at application startup — the `DB_EVENT_REP_CLIENT` event is raised. Also, when an election is held and a replica is elected to be a master, the `DB_EVENT_REP_MASTER` event is raised on the newly elected master and the `DB_EVENT_REP_NEWMASTER` is raised on the other replicas.

Note that this callback is usable for events beyond those required for replication purposes. In this section, however, we only discuss the replication-specific events.

The callback is required to determine which event has been passed to it, and then take action depending on the event. For replication, the events that we care about are:

Some of the more commonly handled events are described below. For a complete list of events, see the `DbEnv::set_event_notify()` method in the *Berkeley DB C++ API Reference Guide*.

- `DB_EVENT_REP_CLIENT`

  The local environment is now a replica.

- `DB_EVENT_REP_CONNECT_BROKEN`

  A previously established connection between two sites in the replication group has been broken.

- `DB_EVENT_REP_CONNECT_ESTD`

  A connection has been established between two sites in the replication group.

- `DB_EVENT_REP_CONNECT_RETRY_ESTABLISHED`

  An attempt was made to establish a connection to a known remote site, but the connection attempt failed.

- `DB_EVENT_REP_DUPMASTER`

  A duplicate master has been discovered in the replication group.

- `DB_EVENT_REP_ELECTED`

  The local site has just won an election and is now the master. Your code should now reconfigure itself to operation as a master site.

- `DB_EVENT_REP_ELECTION_FAILED`

  The local site's attempt to initiate or participate in a replication master election failed, due to the lack of timely message response from a sufficient number of remote sites.

- `DB_EVENT_REP_ELECTION_STARTED`

  Replication Manager has started an election to choose a master site.

- `DB_EVENT_REP_LOCAL_SITE_REMOVED`

  The local site has been removed from the group.

- `DB_EVENT_REP_NEWMASTER`

  An election was held and a new environment was made a master. However, the current environment <span class="emphasis">*is not*</span> the master. This event exists so that you can cause your code to take some unique action in the event that the replication groups switches masters.

- `DB_EVENT_REP_MASTER`

  The local environment is now a master.

- `DB_EVENT_REP_MASTER_FAILURE`

  The connection to the remote master replication site has failed.

- `DB_EVENT_REP_PERM_FAILED`

  The Replication Manager did not receive enough acknowledgements to ensure the transaction's durability within the replicationg group. The Replication Manager has therefore flushed the transaction to the master's local disk for storage.

  How the Replication Manager knows whether the acknowledgements it has received is determined by the ack policy you have set for your applicaton. See <a href="fwrkpermmessage.md#fmwrkpermpolicy" class="xref" title="Identifying Permanent Message Policies">Identifying Permanent Message Policies</a> for more information.

- `DB_EVENT_REP_SITE_ADDED`

  A new site has joined the replication group.

- `DB_EVENT_REP_SITE_REMOVED`

  An existing site has been removed from the replication group.

- `DB_EVENT_REP_STARTUPDONE`

  The replica has completed startup synchronization and is now processing log records received from the master.

- `DB_EVENT_WRITE_FAILED`

  A Berkeley DB write to stable storage failed.

Note that these events are raised whenever the state is established. That is, when the current environment becomes a replica, and that includes at application startup, the event is raised. Also, when an election is held and a replica is elected to be a master, then the event occurs.

The implementation of this callback is fairly simple. First you pass a structure to the environment handle that you can use to record the environment's state, and then you implement a switch statement within the callback that you use to record the current state, depending on the arriving event.

For example:

``` c
#include <db_cxx.h>
/* Forward declaration */
void *event_callback(DbEnv *, u_int32_t, void *);

...

/* The structure we use to track our environment's state */
typedef struct {
    int is_master;
} APP_DATA;

...

/*
 * Inside our main() function, we declare an APP_DATA variable.
 */
APP_DATA my_app_data;
my_app_data.is_master = 0; /* Assume we start as a replica */

...

/*
 * Now we open our environment handle and set the APP_DATA structure
 * to it's app_private member.
 */
DbEnv *dbenv = new DbEnv(0);
dbenv->set_app_private(&my_app_data);

/* Having done that, register the callback with the
 * Berkeley DB library
 */
dbenv->set_event_notify(event_callback); 
```

That done, we still need to implement the callback itself. This implementation can be fairly trivial.

``` c
/*
 * A callback used to determine whether the local environment is a 
 * replica or a master. This is called by the Replication Manager
 * when the local environment changes state.
 */
void *
event_callback(DbEnv *dbenv, u_int32_t which, void *info)
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

Notice how we access the `APP_DATA` information using the environment handle's `app_private` data member. We also ignore the `DB_EVENT_REP_NEWMASTER` and `DB_EVENT_REP_STARTUPDONE` cases since these are not relevant for simple replicated applications.

Of course, this only gives us the current state of the environment. We still need the code that determines what to do when the environment changes state and how to behave depending on the state (described in the next section).
