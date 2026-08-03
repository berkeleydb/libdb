---
title: "Running Replication Manager in multiple processes"
api-name: "Running Replication Manager in multiple processes"
source: docs/programmer_reference/rep_mgrmulti.html
---
## Running Replication Manager in multiple processes

<span class="sect2"> [One replication process and multiple subordinate processes](rep_mgrmulti.md#idp52420616) </span>

<span class="sect2"> [Persistence of local site network address configuration](rep_mgrmulti.md#idp52417008) </span>

<span class="sect2"> [Programming considerations](rep_mgrmulti.md#idp52400144) </span>

<span class="sect2"> [Handling failure](rep_mgrmulti.md#idp52414488) </span>

<span class="sect2"> [Other miscellaneous rules](rep_mgrmulti.md#idp52412256) </span>

Replication Manager supports shared access to a database environment from multiple processes.

### One replication process and multiple subordinate processes

Each site in a replication group has just one network address (TCP/IP host name and port number). This means that only one process can accept incoming connections. At least one application process must invoke the <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a> method to initiate communications and management of the replication state.

If it is convenient, multiple processes may issue calls to the Replication Manager configuration methods, and multiple processes may call <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a>. Replication Manager automatically opens the TCP/IP listening socket in the first process to do so (we'll call it the "replication process" here), and ignores this step in any subsequent processes ("subordinate processes").

### Persistence of local site network address configuration

The local site network address is stored in shared memory, and remains intact even when (all) processes close their environment handles gracefully and terminate. A process which opens an environment handle without running recovery automatically inherits the existing local site network address configuration. Such a process may not change the local site address (although it is allowed to redundantly specify a local site configuration matching that which is already in effect).

In order to change the local site network address, the application must run recovery. The application can then specify a new local site address before restarting Replication Manager. The application should also remove the old local site address from the replication group if it is no longer needed.

### Programming considerations

Note that Replication Manager applications must follow all the usual rules for Berkeley DB multi-threaded and/or multi-process applications, such as ensuring that the recovery operation occurs single-threaded, only once, before any other thread or processes operate in the environment. Since Replication Manager creates its own background threads which operate on the environment, all environment handles must be opened with the <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> flag, even if the application is otherwise single-threaded per process.

At the replication master site, each Replication Manager process opens outgoing TCP/IP connections to all clients in the replication group. It uses these direct connections to send to clients any log records resulting from update transactions that the process executes. But all other replication activity —message processing, elections, etc.— takes place only in the "replication process".

Replication Manager notifies the application of certain events, using the callback function configured with the <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a> method. These notifications occur only in the process where the event itself occurred. Generally this means that most notifications occur only in the "replication process". Currently the only replication notification that can occur in a "subordinate process" is <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_PERM_FAILED" class="olink">DB_EVENT_REP_PERM_FAILED</a>.

It is not supported for a process running Replication Manager to spawn a subprocess.

### Handling failure

Multi-process Replication Manager applications should handle failures in a manner consistent with the rules described in <a href="transapp_fail.md" class="xref" title="Handling failure in Transactional Data Store applications">Handling failure in Transactional Data Store applications</a>. To summarize, there are two ways to handle failure of a process:

1.  The simple way is to kill all remaining processes, run recovery, and then restart all processes from the beginning. But this can be a bit drastic.

2.  Using the <a href="../../api/c/envfailchk.md" class="olink">DB_ENV-&gt;failchk()</a> method, it is sometimes possible to leave surviving processes running, and just restart the failed process.

    Multi-process Replication Manager applications using this technique must start a new process when an old process fails. It is not possible for a "subordinate process" to take over the duties of a failed "replication process". If the failed process happens to be the replication process, then after a failchk() call the next process to call <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a> will become the new replication process.

### Other miscellaneous rules

1.  A database environment may not be shared between a Replication Manager application process and a Base API application process.
2.  It is not possible to run multiple Replication Manager processes during mixed-version live upgrades from Berkeley DB versions prior to 4.8.
