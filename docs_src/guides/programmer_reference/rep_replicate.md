---
title: "Running Replication using the db_replicate Utility"
api-name: "Running Replication using the db_replicate Utility"
source: docs/programmer_reference/rep_replicate.html
---
## Running Replication using the db_replicate Utility

<span class="sect2"> [One Replication Process and Multiple Subordinate Processes](rep_replicate.md#idp52430544) </span>

<span class="sect2"> [Common Use Case](rep_replicate.md#idp52447760) </span>

<span class="sect2"> [Avoiding Rollback](rep_replicate.md#idp52457840) </span>

<span class="sect2"> [When to Consider an Integrated HA Application](rep_replicate.md#idp52462952) </span>

Replication Manager supports shared access to a database environment from multiple processes. Berkeley DB provides a replication-aware utility, db_replicate, that enables you to upgrade an existing Transactional Data Store application, as discussed in the <a href="transapp.md#transapp_intro" class="xref" title="Transactional Data Store introduction">Transactional Data Store introduction</a> section, to an HA application with minor modifications. While the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility simplifies the use of replication with a TDS application, you must still understand replication and its impact on the application.

### One Replication Process and Multiple Subordinate Processes

Based on the terminology introduced in the <a href="rep_mgrmulti.md" class="xref" title="Running Replication Manager in multiple processes">Running Replication Manager in multiple processes</a> section, application processes are "subordinate processes" and the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility is the "primary replication process".

You must consider the following items when planning to use the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility in combination with a TDS application.

- Memory regions

  The <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility requires shared memory access among separate processes, and therefore cannot be used with <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a>.

- Multi-process implications

  You must understand and accept all of the TDS implications of multi-process use as specified in <a href="transapp_app.md" class="xref" title="Architecting Transactional Data Store applications">Architecting Transactional Data Store applications</a>. Special attention should be paid to the coordination needed for unrelated processes to start up correctly.

- Replication configuration

  Several configuration settings are required for replication. You must set the <a href="../../api/c/envopen.md#envopen_DB_INIT_REP" class="olink">DB_INIT_REP</a> and <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> flags for the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method. Another required configuration item is the local address. You identify this by creating a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle and then setting the `DB_LOCAL_SITE` parameter using the <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a> method. You also tell sites how to contact other sites by creating <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handles for those sites. Most replication configuration options start with reasonable defaults, but applications have to customize at least some of them. You can set all replication related configuration options either programmatically or in the <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file.

- Starting the application and replication

  The <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility assumes that an environment exists and that the application has run recovery, if necessary, and created and configured the environment. The startup flow of a typical TDS application may not be the best flow for a replication application and you must understand the issues involved. For instance, if an application starts, runs recovery, and performs update operations before starting the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility, then if that site becomes a client when replication starts, those update operations will be rolled back.

- Handling events

  Almost all of the replication-specific events are handled by the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility process, and therefore the application process does not see them. If the application needs to know the information from those replication-specific events, such as role changes, the application must call the <a href="../../api/c/repstat.md" class="olink">rep_stat()</a> method method. The one replication-specific event the application can now receive is the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_PERM_FAILED" class="olink">DB_EVENT_REP_PERM_FAILED</a> event. See <a href="rep_mgr_ack.md" class="xref" title="Choosing a Replication Manager Ack Policy">Choosing a Replication Manager Ack Policy</a> for additional information about this event.

- Handling errors

  There are some error return values that relate only to replication. Specifically, the `DB_REP_HANDLE_DEAD` error should now be handled by the application. Also, if master leases are in use, then the application also needs to consider the `DB_REP_LEASE_EXPIRED` error.

- Flexibility tradeoff

  You are giving up flexibility for the ease of use of the utility. Application complexity or requirements may eventually dictate integrating HA calls into the application over using the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility.

- Read-only client application

  The application requires additional changes to manage the read-only status when the application takes on the role of a client.

### Common Use Case

This section lists the steps needed to get replication running for a common use case of the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility. The use case presented is an existing TDS application that already has its environment and databases created and is up and running. At some point, HA is considered because failover protection or balancing the read load may now be desired.

1.  To understand the issues involved in a replication/HA application, see the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility section in the <span class="emphasis">*API Reference Guide*</span>, the <a href="../../api/c/rep.md" class="olink">Replication Chapter</a> in the <span class="emphasis"> *Programmer's Reference Guide*</span>, and the source code of the ex_rep_mgr example program.

2.  Make a local hot backup of the current application environment to a new location to use as a testing area.

3.  Add the <a href="../../api/c/envopen.md#envopen_DB_INIT_REP" class="olink">DB_INIT_REP</a> and <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> flags (if not already being used) to the application or the <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file.

4.  Modify the <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file to add the necessary replication configuration values. At a minimum, the local host and port information must be added using the <a href="../../api/c/repmgr_site_parameter.md" class="olink">repmgr_site</a> method parameter. As more sites are added to the group, remote host and port information can optionally also be added by adding more <a href="../../api/c/repmgr_site_parameter.md" class="olink">repmgr_site</a> method parameters to the <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file file.

5.  Rebuild the application and restart it in the current testing directory.

6.  Start the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility on the master site with the -M option and any other options needed such as -h for the home directory. At this point you have a lone master site running in an environment with no other replicated sites in the group.

7.  Optionally, prepare to start a client site by performing a manual hot backup of the running master environment to initialize a client target directory. While replication can make its own copy, the hot backup will expedite the synchronization process. Also, if the application assumes the existence of a database and the client site is started without data, the application may have errors or incorrectly attempt to create the database.

8.  Copy the application to the client target.

9.  Modify the client environment's <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file to set the client's local host and port values and to add remote site information for the master site and any other replication configuration choices necessary.

10. Start the application on the client. The client application should not update data at this point, as explained previously.

11. Start the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility specifying the client environment's home directory using the -h option. Omit the -M option in this case, because the utility defaults to starting in the client role.

Once the initial replication group is established, do not use the -M option with the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility. After the initial start, <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility assumes the use of elections. If a site crashes, it should rejoin the group as a client so that it can synchronize with the rest of the group.

### Avoiding Rollback

Depending on how an application is structured, transactional rollback can occur. If this is possible, then you must make application changes or be prepared for successful transactions to disappear. Consider a common program flow where the application first creates and opens the environment with recovery. Then, immediately after that, the application opens up the databases it expects to use. Often an application will use the <a href="../../api/c/dbopen.md#open_DB_CREATE" class="olink">DB_CREATE</a> flag so that if the database does not exist it is created, otherwise the existing one is used automatically. Then the application begins servicing transactions to write and read data.

When replication is introduced, particularly via the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility, the possibility of rollback exists unless the application takes steps to prevent it. In the situation described above, if all of the above steps occur before the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility process starts, and the site is started as a client, then all the operations will be rolled back when the site finds the master. The client site will synchronize with the log and operations on the master site, so any operations that occurred in the client application before it knew it was a client will be discarded.

One way to reduce the possibility of rollback is to modify the application so that it only performs update operations (including creation of a database) if it is the master site. If the application refrains from updating until it is the master, then it will not perform operations when it is in the undefined state before replication has been started. The event indicating a site is master will be delivered to the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility process, so the application process must look for that information via the <a href="../../api/c/repstat.md" class="olink">rep_stat()</a> method. A site that is expecting to perform updates may need to poll via the <a href="../../api/c/repstat.md" class="olink">rep_stat()</a> method to see the state change from an undefined role to either the master or client role. Similarly, since a client site cannot create a database, it may need to poll for the database's existence while the client synchronizes with the master until the database is created at the client site.

### When to Consider an Integrated HA Application

The <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility provides the means to achieve a replicated application quickly. However, the trade-off for this rapid implementation is that the full flexibility of replication is not available. Some applications may eventually need to consider integrating directly with replication rather than using the <a href="../../api/c/db_replicate.md" class="olink">db_replicate</a> utility if greater flexibility is desired.

One likely reason for considering integration would be the convenience of receiving all replication-related events in the application process and gaining direct knowledge of such things as role changes. Using the event callback is cleaner and easier than polling for state changes via the <a href="../../api/c/repstat.md" class="olink">rep_stat()</a> method.

A second likely reason for integrating replication directly into the application is the multi-process aspect of the utility program. The developer may find it easier to insert the start of replication directly into the code once the environment is created, recovered, or opened, and avoid the scenario where the application is running in the undefined state. Also it may simply be easier to start the application once than to coordinate different processes and their startup order in the system.
