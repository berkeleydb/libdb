---
title: "Chapter 3. The DB Replication Manager"
api-name: "Chapter 3. The DB Replication Manager"
source: docs/gsg_db_rep/C/repapp.html
---
## Chapter 3. The DB Replication Manager

**Table of Contents**

<span class="sect1"> [The DB_SITE Handle](repapp.md#repmgr_grpmgmt) </span>

<span class="sect1"> [Starting and Stopping Replication](rep_init_code.md) </span>

<span class="sect2"> [Managing Election Policies](rep_init_code.md#election_flags) </span>

<span class="sect2"> [Selecting the Number of Threads](rep_init_code.md#thread_count) </span>

<span class="sect1"> [Adding the Replication Manager to ex_rep_gsg_simple](repmgr_init_example_c.md) </span>

<span class="sect1"> [Permanent Message Handling](fwrkpermmessage.md) </span>

<span class="sect2"> [Identifying Permanent Message Policies](fwrkpermmessage.md#fmwrkpermpolicy) </span>

<span class="sect2"> [Setting the Permanent Message Timeout](fwrkpermmessage.md#fmwrkpermtimeout) </span>

<span class="sect2"> [Adding a Permanent Message Policy to ex_rep_gsg_repmgr](fwrkpermmessage.md#perm2fmwrkexample) </span>

<span class="sect1"> [Managing Election Times](electiontimes.md) </span>

<span class="sect2"> [Managing Election Timeouts](electiontimes.md#electiontimeout) </span>

<span class="sect2"> [Managing Election Retry Times](electiontimes.md#electretrytime) </span>

<span class="sect1"> [Managing Connection Retries](fmwrkconnectretry.md) </span>

<span class="sect1"> [Managing Heartbeats](heartbeats.md) </span>

The easiest way to add replication to your transactional application is to use the Replication Manager. The Replication Manager provides a comprehensive communications layer that enables replication. For a brief listing of the Replication Manager's feature set, see <a href="apioverview.md#repframeworkoverview" class="xref" title="Replication Manager Overview">Replication Manager Overview</a>.

To use the Replication Manager, you make use of a combination of the `DB_SITE` class and related methods, plus special methods off the `DB_ENV` class. That is:

1.  Create an environment handle as normal.

2.  Configure your environment handle as needed (e.g. set the error file and error prefix values, if desired).

3.  Use the Replication Manager replication classes and methods to configure the Replication Manager. Using these classes and methods causes DB to know that you are using the Replication Manager.

    Configuring the Replication Manager entails setting the replication environment's priority, setting the TCP/IP address that this replication environment will use for incoming replication messages, identifying TCP/IP addresses of other replication environments, setting the number of replication environments in the replication group, and so forth. These actions are discussed throughout the remainder of this chapter.

4.  Open your environment handle. When you do this, be sure to specify `DB_INIT_REP` and `DB_THREAD` to your open flags. (This is in addition to the flags that you normally use for a single-threaded transactional application). The first of these causes replication to be initialized for the application. The second causes your environment handle to be free-threaded (thread safe). Both flags are required for Replication Manager usage.

5.  Start replication by calling `DB_ENV->repmgr_start()`.

6.  Open your databases as needed. Masters must open their databases for read and write activity. Replicas can open their databases for read-only activity, but doing so means they must re-open the databases if the replica ever becomes a master. Either way, replicas should never attempt to write to the database(s) directly.

### Note

The Replication Manager allows you to only use one environment handle per process.

When you are ready to shut down your application:

1.  Close any open `DB_SITE` handles that you might have open.

2.  Close your databases

3.  Close your environment. This causes replication to stop as well.

### Note

Before you can use the Replication Manager, you may have to enable it in your DB library. This is <span class="emphasis">*not*</span> a requirement for Microsoft Windows systems, or Unix systems that use pthread mutexes by default. Other systems, notably BSD and BSD-derived systems (such as Mac OS X), must enable the Replication Manager when you configure the DB build.

You do this by <span class="emphasis">*not*</span> disabling replication and by configuring the library with POSIX threads support. In other words, replication must be turned on in the build (it is by default), and POSIX thread support must be enabled if it is not already by default. To do this, use the `--enable-pthread_api` switch on the configure script.

For example:

``` c
../dist/configure --enable-pthread-api
```

## The DB_SITE Handle

Before continuing, it is useful to mention the `DB_SITE` handle. This class is used to configure important attributes about a site such as its host name and port number, and whether it is the local site. It is also used to indicate whether a site is a <span class="emphasis">*group creator*</span>, which is important when you are starting the very first site in a replication group for the very first time.

The `DB_SITE` handle is used whenever you start up a site. It must be closed before you close your `DB_ENV` handle.

The `DB_SITE` handle is plays an important role in replication group management. This topic is fully described in the *Berkeley DB Programmer's Reference Guide*.
