---
title: "Ex_rep: a replication example"
api-name: "Ex_rep: a replication example"
source: docs/programmer_reference/rep_ex.html
---
## Ex_rep: a replication example

Ex_rep, found in the `examples_c/ex_rep` subdirectory of the Berkeley DB distribution, is a simple but complete demonstration of a replicated application. The application is a mock stock ticker. The master accepts a stock symbol and a numerical value as input, and stores this information into a replicated database; either master or clients can display the contents of the database, given an empty input line.

There are two versions of the application: ex_rep_mgr uses Replication Manager, while ex_rep_base uses the replication Base API. This is intended to demonstrate that, while the basic function of the application is the same in either case, the replication support infrastructure differs markedly.

The communication infrastructure demonstrated with ex_rep_base has the same dependencies on system networking and threading support as does the Replication Manager (see the <a href="rep.md#rep_intro" class="xref" title="Replication introduction">Replication introduction</a>). The Makefile created by the standard UNIX configuration will build the ex_rep examples on most platforms. Enter "make ex_rep_mgr" and/or "make ex_rep_base" to build them.

The synopsis for both programs is as follows:

`ex_rep_xxx `**`-h home`**` `**`-l host:port`**` [`**`-MC`**`] [`**`-r host:port`**`] [`**`-R host:port`**`] [`**`-a all|quorum`**`] [`**`-b`**`] [`**`-n sites`**`] [`**`-p priority`**`] [`**`-v`**`]`

where "ex_rep_xxx" is either "ex_rep_mgr" or "ex_rep_base". The only difference is that:

- specifying **-M** or **-C** is optional for ex_rep_mgr, but one of these options must be specified for ex_rep_base.

- The **-n** option is not supported supported by ex_rep_mgr. That option specifies the number of nodes in the replication group. When you use the Replication Manager, this number is automatically determined for you.

The options apply to either version of the program except where noted. They are as follows:

<span class="term"> **-h** </span>  
Specify a home directory for the database environment.

<span class="term"> **-l** </span>  
Listen on local host "host" at port "port" for incoming connections.

<span class="term"> **-M** </span>  
Configure this process as a master.

<span class="term"> **-C** </span>  
Configure this process as a client.

<span class="term"> **-r** </span>  
Identifies the helper site used for joining the group.

<span class="term"> **-R** </span>  
Identifies a remote peer to be used for joining the group. This peer is used for syncing purposes. See <a href="rep_mastersync.md#rep_c2c_sync" class="xref" title="Client-to-client synchronization">Client-to-client synchronization</a> for more information.

<span class="term"> **-a** </span>  
Specify repmgr acknowledgement policy of all or quorum. See <a href="../../api/c/repmgrset_ack_policy.md" class="olink">DB_ENV-&gt;repmgr_set_ack_policy()</a> for more information (ex_rep_mgr only.)

<span class="term"> **-b** </span>  
Indicates that bulk transfer should be used. See <a href="rep_bulk.md" class="xref" title="Bulk transfer">Bulk transfer</a> for more information.

<span class="term"> **-n** </span>  
Specify the total number of sites in the replication group (ex_rep_base only).

<span class="term"> **-p** </span>  
Set the election priority. See <a href="rep_elect.md" class="xref" title="Elections">Elections</a> for more information.

<span class="term"> **-v** </span>  
Indicates that additional informational and debugging output should be enabled.

A typical ex_rep_mgr session begins with a command such as the following, to start a master:

``` c
ex_rep_mgr -M -p 100 -h DIR1 -l localhost:30100
```

and several clients:

``` c
ex_rep_mgr -C  -p 50 -h DIR2 -l localhost:30101 -r localhost:30100
ex_rep_mgr -C -p 10 -h DIR3 -l localhost:30102 -r localhost:30100
ex_rep_mgr -C -p 0 -h DIR4 -l localhost:30103 -r localhost:30100
```

In this example, the client with home directory DIR4 can never become a master (its priority is 0). Both of the other clients can become masters, but the one with home directory DIR2 is preferred. Priorities are assigned by the application and should reflect the desirability of having particular clients take over as master in the case that the master fails.
