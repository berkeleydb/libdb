---
title: "Chapter 12.  Berkeley DB Replication"
api-name: "Chapter 12.  Berkeley DB Replication"
source: docs/programmer_reference/rep.html
---
## Chapter 12.  Berkeley DB Replication

**Table of Contents**

<span class="sect1"> [Replication introduction](rep.md#rep_intro) </span>

<span class="sect1"> [Replication environment IDs](rep_id.md) </span>

<span class="sect1"> [Replication environment priorities](rep_pri.md) </span>

<span class="sect1"> [Building replicated applications](rep_app.md) </span>

<span class="sect1"> [Replication Manager methods](rep_mgr_meth.md) </span>

<span class="sect1"> [Base API Methods](rep_base_meth.md) </span>

<span class="sect1"> [Building the communications infrastructure](rep_comm.md) </span>

<span class="sect1"> [Connecting to a new site](rep_newsite.md) </span>

<span class="sect1"> [Managing Replication Manager Group Membership](group_membership.md) </span>

<span class="sect2"> [Adding Sites to a Replication Group](group_membership.md#group_mem_add) </span>

<span class="sect2"> [Removing Sites from a Replication Group](group_membership.md#group_mem_remove) </span>

<span class="sect2"> [Primordial Startups](group_membership.md#group_mem_primordialstartup) </span>

<span class="sect2"> [Upgrading Groups](group_membership.md#group_mem_upgrade) </span>

<span class="sect1"> [Managing Replication Files](rep_filename.md) </span>

<span class="sect1"> [Running Replication Manager in multiple processes](rep_mgrmulti.md) </span>

<span class="sect2"> [One replication process and multiple subordinate processes](rep_mgrmulti.md#idp52420616) </span>

<span class="sect2"> [Persistence of local site network address configuration](rep_mgrmulti.md#idp52417008) </span>

<span class="sect2"> [Programming considerations](rep_mgrmulti.md#idp52400144) </span>

<span class="sect2"> [Handling failure](rep_mgrmulti.md#idp52414488) </span>

<span class="sect2"> [Other miscellaneous rules](rep_mgrmulti.md#idp52412256) </span>

<span class="sect1"> [Running Replication using the db_replicate Utility](rep_replicate.md) </span>

<span class="sect2"> [One Replication Process and Multiple Subordinate Processes](rep_replicate.md#idp52430544) </span>

<span class="sect2"> [Common Use Case](rep_replicate.md#idp52447760) </span>

<span class="sect2"> [Avoiding Rollback](rep_replicate.md#idp52457840) </span>

<span class="sect2"> [When to Consider an Integrated HA Application](rep_replicate.md#idp52462952) </span>

<span class="sect1"> [Choosing a Replication Manager Ack Policy](rep_mgr_ack.md) </span>

<span class="sect1"> [Elections](rep_elect.md) </span>

<span class="sect1"> [Synchronizing with a master](rep_mastersync.md) </span>

<span class="sect2"> [Delaying client synchronization](rep_mastersync.md#rep_delay_sync) </span>

<span class="sect2"> [Client-to-client synchronization](rep_mastersync.md#rep_c2c_sync) </span>

<span class="sect2"> [Blocked client operations](rep_mastersync.md#idp52488504) </span>

<span class="sect2"> [Clients too far out-of-date to synchronize](rep_mastersync.md#idp52510624) </span>

<span class="sect1"> [Initializing a new site](rep_init.md) </span>

<span class="sect1"> [Bulk transfer](rep_bulk.md) </span>

<span class="sect1"> [Transactional guarantees](rep_trans.md) </span>

<span class="sect1"> [Master Leases](rep_lease.md) </span>

<span class="sect2"> [Changing Group Size](rep_lease.md#masterlease_change_groupsize) </span>

<span class="sect1"> [Read your writes consistency](rep_ryw.md) </span>

<span class="sect2"> [Getting a token](rep_ryw.md#gettoken) </span>

<span class="sect2"> [Token handling](rep_ryw.md#tokenhandling) </span>

<span class="sect2"> [Using a token to check or wait for a transaction](rep_ryw.md#usingtoken) </span>

<span class="sect1"> [Clock Skew](rep_clock_skew.md) </span>

<span class="sect1"> [Using Replication Manager message channels](repmgr_channels.md) </span>

<span class="sect2"> [DB_CHANNEL](repmgr_channels.md#dbchannel_class) </span>

<span class="sect2"> [Sending messages over a message channel](repmgr_channels.md#dbchannel_send) </span>

<span class="sect2"> [Receiving messages](repmgr_channels.md#dbchannel_receive) </span>

<span class="sect1"> [Special considerations for two-site replication groups](rep_twosite.md) </span>

<span class="sect1"> [Network partitions](rep_partition.md) </span>

<span class="sect1"> [Replication FAQ](rep_faq.md) </span>

<span class="sect1"> [Ex_rep: a replication example](rep_ex.md) </span>

<span class="sect1"> [Ex_rep_base: a TCP/IP based communication infrastructure](rep_ex_comm.md) </span>

<span class="sect1"> [Ex_rep_base: putting it all together](rep_ex_rq.md) </span>

<span class="sect1"> [Ex_rep_chan: a Replication Manager channel example](rep_ex_chan.md) </span>

## Replication introduction

Berkeley DB includes support for building highly available applications based on replication. Berkeley DB replication groups consist of some number of independently configured database environments. There is a single <span class="emphasis">*master*</span> database environment and one or more <span class="emphasis">*client*</span> database environments. Master environments support both database reads and writes; client environments support only database reads. If the master environment fails, applications may upgrade a client to be the new master. The database environments might be on separate computers, on separate hardware partitions in a non-uniform memory access (NUMA) system, or on separate disks in a single server. As always with Berkeley DB environments, any number of concurrent processes or threads may access a database environment. In the case of a master environment, any number of threads of control may read and write the environment, and in the case of a client environment, any number of threads of control may read the environment.

Applications may be written to provide various degrees of consistency between the master and clients. The system can be run synchronously such that replicas are guaranteed to be up-to-date with all committed transactions, but doing so may incur a significant performance penalty. Higher performance solutions sacrifice total consistency, allowing the clients to be out of date for an application-controlled amount of time.

There are two ways to build replicated applications. The simpler way is to use the Berkeley DB Replication Manager. The Replication Manager provides a standard communications infrastructure, and it creates and manages the background threads needed for processing replication messages.

The Replication Manager implementation is based on TCP/IP sockets, and uses POSIX 1003.1 style networking and thread support. (On Windows systems, it uses standard Windows thread support.) As a result, it is not as portable as the rest of the Berkeley DB library itself.

The alternative is to use the lower-level replication "Base APIs". This approach affords more flexibility, but requires the application to provide some critical components:

1.  A communication infrastructure. Applications may use whatever wire protocol is appropriate for their application (for example, RPC, TCP/IP, UDP, VI or message-passing over the backplane).
2.  The application is responsible for naming. Berkeley DB refers to the members of a replication group using an application-provided ID, and applications must map that ID to a particular database environment or communication channel.
3.  The application is responsible for monitoring the status of the master and clients, and identifying any unavailable database environments.
4.  The application must provide whatever security policies are needed. For example, the application may choose to encrypt data, use a secure socket layer, or do nothing at all. The level of security is left to the sole discretion of the application.

(Note that Replication Manager does not provide wire security for replication messages.)

The following pages present various programming considerations, many of which are directly relevant only for Base API applications. However, even when using Replication Manager it is important to understand the concepts.

Finally, the Berkeley DB replication implementation has one other additional feature to increase application reliability. Replication in Berkeley DB is implemented to perform database updates using a different code path than the standard ones. This means operations that manage to crash the replication master due to a software bug will not necessarily also crash replication clients.

For more information on the replication manager operations, see the <a href="../../api/c/rep.md#replist" class="olink">Replication and Related Methods</a> section in the *Berkeley DB C API Reference Guide.*
