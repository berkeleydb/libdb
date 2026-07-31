---
title: "Replication PRAGMAs"
api-name: "Replication PRAGMAs"
source: docs/bdb-sql/reppragma.html
---
## Replication PRAGMAs

<span class="sect2"> [PRAGMA replication](reppragma.md#pragma_replication) </span>

<span class="sect2"> [PRAGMA replication_initial_master](reppragma.md#pragma_replication_initial_master) </span>

<span class="sect2"> [PRAGMA replication_local_site](reppragma.md#pragma_replication_local_site) </span>

<span class="sect2"> [PRAGMA replication_remote_site](reppragma.md#pragma_replication_remote_site) </span>

<span class="sect2"> [PRAGMA replication_remove_site](reppragma.md#pragma_replication_remove_site) </span>

<span class="sect2"> [PRAGMA replication_verbose_output](reppragma.md#pragma_replication_verbose_output) </span>

<span class="sect2"> [PRAGMA replication_verbose_file](reppragma.md#pragma_replication_verbose_file) </span>

To control replication when using the Berkeley DB SQL interface, you use the following PRAGMAs. For an example of how to use these, see <a href="rep_usageexamples.md" class="xref" title="Replication Usage Examples">Replication Usage Examples</a>.

### PRAGMA replication

``` c
PRAGMA replication=ON|OFF 
```

Enables the local environment to participate in replication.

Before invoking this PRAGMA for a brand new database (one that has never been opened), you must invoke the `replication_local_site` PRAGMA and then either the `replication_initial_master` or the `replication_remote_site` PRAGMA. These actions define the way this site fits into the replication group.

If you are enabling replication for an existing database, it must become the initial master for a new replication group. You must invoke the `replication_local_site` PRAGMA followed by the `replication_initial_master` PRAGMA before enabling replication.

If you use this PRAGMA to turn off replication, then replication is completely disabled for the environment. In order to enable replication again, you follow the procedure used to enable replication on an existing database; that is, invoke the `replication_local_site` PRAGMA followed by the `replication_initial_master` PRAGMA, followed by `PRAGMA replication=ON`.

### PRAGMA replication_initial_master

``` c
PRAGMA replication_initial_master=ON|OFF 
```

Causes the local environment to start up as a master site. This PRAGMA must be used once and only once in the replicated lifetime of a BDB SQL environment.

This PRAGMA is usually invoked for the first site in a new replication group before the `replication` PRAGMA is invoked and before BDB SQL initially creates the underlying BDB environment for a SQL database. Starting replication on the initial master site establishes the new replication group so that other sites can join it.

However, you must call this PRAGMA when enabling replication for a database that already exists. Doing so causes the existing database to become the replication master for a new replication group.

Note that subsequent election activity can cause other sites in the replication group to become master. Do not assume that the initial master site will remain master indefinitely, or that it will rejoin the replication group as master after a shutdown.

### PRAGMA replication_local_site

``` c
PRAGMA replication_local_site="hostname:port" 
```

Sets the local site information for replication.

### PRAGMA replication_remote_site

``` c
PRAGMA replication_remote_site="hostname:port" 
```

Sets information about a remote helper site in the replication group.

This PRAGMA is needed when a site first joins an existing replication group to specify a site that is already in the replication group. It must be invoked before the `replication` PRAGMA is invoked. This PRAGMA is not needed on the initial master site or when restarting a site that is already a member of the replication group. However, supplying this PRAGMA in those situations does no harm.

Note that the information provided to this PRAGMA can be superseded by normal replication activity over the course of the environment's lifetime.

### PRAGMA replication_remove_site

``` c
PRAGMA replication_remove_site="hostname:port" 
```

Removes the specified site from the replication group. Use this PRAGMA if you truly want to remove the site permanently from the group. It is not desirable to call this PRAGMA if a site has been temporarily shut down or disconnected from the rest of the replication group.

Removing a site from the replication group means that the site is no longer counted towards the total number of sites belonging to the group. This is important when the replication group requires knowledge about whether a quorum has been reached (such as when, for example, elections are held).

### PRAGMA replication_verbose_output

``` c
PRAGMA replication_verbose_output=ON|OFF 
```

If set to TRUE, additional logging information specifically related to replication is created.

### PRAGMA replication_verbose_file

``` c
PRAGMA replication_verbose_file="filename" 
```

Indicates that verbose replication output should be sent to the specified file, as opposed to STDOUT.
