---
title: "Master Leases"
api-name: "Master Leases"
source: docs/programmer_reference/rep_lease.html
---
## Master Leases

<span class="sect2"> [Changing Group Size](rep_lease.md#masterlease_change_groupsize) </span>

Some applications have strict requirements about the consistency of data read on a master site. Berkeley DB provides a mechanism called master leases to provide such consistency. Without master leases, it is sometimes possible for Berkeley DB to return old data to an application when newer data is available due to unfortunate scheduling as illustrated below:

1.  **Application on master site**: Read data item <span class="emphasis">*foo*</span> via Berkeley DB <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> call.
2.  **Application on master site**: sleep, get descheduled, etc.
3.  **System**: Master changes role, becomes a client.
4.  **System**: New site is elected master.
5.  **System**: New master modifies data item <span class="emphasis">*foo*</span>.
6.  **Application**: Berkeley DB returns old data for <span class="emphasis">*foo*</span> to application.

By using master leases, Berkeley DB can provide guarantees about the consistency of data read on a master site. The master site can be considered a recognized authority for the data and consequently can provide authoritative reads. Clients grant master leases to a master site. By doing so, clients acknowledge the right of that site to retain the role of master for a period of time. During that period of time, clients cannot elect a new master, become master, nor grant their lease to another site.

By holding a collection of granted leases, a master site can guarantee to the application that the data returned is the current, authoritative value. As a master performs operations, it continually requests updated grants from the clients. When a read operation is required, the master guarantees that it holds a valid collection of lease grants from clients before returning data to the application. By holding leases, Berkeley DB provides several guarantees to the application:

1.  Authoritative reads: A guarantee that the data being read by the application is the current value.

2.  Durability from rollback: A guarantee that the data being written or read by the application is permanent across a majority of client sites and will never be rolled back.

    The rollback guarantee also depends on the <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a> flag. The guarantee is effective as long as there isn't total replication group failure while clients have granted leases but are holding the updates in their cache. The application must weigh the performance impact of synchronous transactions against the risk of total replication group failure. If clients grant a lease while holding updated data in cache, and total failure occurs, then the data is no longer present on the clients and rollback can occur if the master also crashes.

    The guarantee that data will not be rolled back applies only to data successfully committed on a master. Data read on a client, or read while ignoring leases can be rolled back.

3.  Freshness: A guarantee that the data being read by the application on the <span class="emphasis">*master*</span> is up-to-date and has not been modified or removed during the read.

    The read authority is only on the master. Read operations on a client always ignore leases and consequently, these operations can return stale data.

4.  Master viability: A guarantee that a current master with valid leases cannot encounter a duplicate master situation.

    Leases remove the possibility of a duplicate master situation that forces the current master to downgrade to a client. However, it is still possible that old masters with expired leases can discover a later master and return <a href="../../api/c/repmessage.md#repmsg_DB_REP_DUPMASTER" class="olink">DB_REP_DUPMASTER</a> to the application.

There are several requirements of the application using leases:

1.  Replication Manager applications must configure a majority (or larger) acknowledgement policy via the <a href="../../api/c/repmgrset_ack_policy.md" class="olink">DB_ENV-&gt;repmgr_set_ack_policy()</a> method. Base API applications must implement and enforce such a policy on their own.
2.  Base API applications must return an error from the send callback function when the majority acknowledgement policy is not met for permanent records marked with <a href="../../api/c/reptransport.md#transport_DB_REP_PERMANENT" class="olink">DB_REP_PERMANENT</a>. Note that the Replication Manager automatically fulfills this requirement.
3.  Base API applications must set the number of sites in the group using the <a href="../../api/c/repnsites.md" class="olink">DB_ENV-&gt;rep_set_nsites()</a> method before starting replication and cannot change it during operation.
4.  Using leases in a replication group is all or none. Behavior is undefined when some sites configure leases and others do not. Use the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method to turn on leases.
5.  The configured lease timeout value must be the same on all sites in a replication group, set via the <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> method.
6.  The configured clock_scale_factor value must be the same on all sites in a replication group. This value defaults to no skew, but can be set via the <a href="../../api/c/repclockskew.md" class="olink">DB_ENV-&gt;rep_set_clockskew()</a> method.
7.  Applications that care about read guarantees must perform all read operations on the master. Reading on a client does not guarantee freshness.
8.  The application must use elections to choose a master site. It must never simply declare a master without having won an election (as is allowed without Master Leases).

Master leases are based on timeouts. Berkeley DB assumes that time always runs forward. Users who change the system clock on either client or master sites when leases are in use void all guarantees and can get undefined behavior. See the <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> method for more information.

Applications using master leases should be prepared to handle `DB_REP_LEASE_EXPIRED` errors from read operations on a master and from the <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a> method.

Read operations on a master that should not be subject to leases can use the <a href="../../api/c/dbget.md#get_DB_IGNORE_LEASE" class="olink">DB_IGNORE_LEASE</a> flag to the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> method. Read operations on a client always imply leases are ignored.

Master lease checks cannot succeed until a majority of sites have completed client synchronization. Read operations on a master performed before this condition is met can use the <a href="../../api/c/dbget.md#get_DB_IGNORE_LEASE" class="olink">DB_IGNORE_LEASE</a> flag to avoid errors.

Clients are forbidden from participating in elections while they have an outstanding lease granted to a master. Therefore, if the <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> method is called, then Berkeley DB will block, waiting until its lease grant expires before participating in any election. While it waits, the client attempts to contact the current master. If the client finds a current master, then it returns from the <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> method. When leases are configured and the lease has never yet been granted (on start-up), clients must wait a full lease timeout before participating in an election.

### Changing Group Size

If you are using master leases and you change the size of your replication group, there is a remote possibility that you can lose some data previously thought to be durable. This is only true for users of the Base API.

The problem can arise if you are removing sites from your replication group. (You might be increasing the size of your site overall, but if you remove all of the wrong sites you can lose data.)

Suppose you have a replication group with five sites; A, B, C, D and E; and you are using a quorum acknowledgement policy. Then:

1.  Master A replicates a transaction to replicas B and C. Those sites acknowledge the write activity.

2.  Sites D and E do not receive the transaction. However, B and C have acknowledged the transaction, which means the acknowledgement policy is met and so the transaction is considered durable.

3.  You shutdown sites B and C. Now only A has the transaction.

4.  You increase the size of your replication group to 3 using <a href="../../api/c/repnsites.md" class="olink">DB_ENV-&gt;rep_set_nsites()</a>.

5.  You shutdown or otherwise lose site A.

6.  Sites D and E hold an election. Because the size of the replication group is 3, they have enough sites to successfully hold an election. However, neither site has the transaction in question. In this way, the transaction can become lost.

An alternative scenario exists where you do not change the size of your replication group, or you actually increase the size of your replication group, but in the process you happen to remove the exact wrong sites:

1.  Master A replicates a transaction to replicas B and C. Those sites acknowledge the write activity.

2.  Sites D and E do not receive the transaction. However, B and C have acknowledged the transaction, which means the acknowledgement policy is met and so the transaction is considered durable.

3.  You shutdown sites B and C. Now only A has the transaction.

4.  You add three new sites to your replication group: F, G and H, increasing the size of your replication group to 6 using <a href="../../api/c/repnsites.md" class="olink">DB_ENV-&gt;rep_set_nsites()</a>.

5.  You shutdown or otherwise lose site A before F, G and H can be fully populated with data.

6.  Sites D, E, F, G and H hold an election. Because the size of the replication group is 6, they have enough sites to successfully hold an election. However, none of these sites has the transaction in question. In this way, the transaction can become lost.

This scenario represents a race condition that would be highly unlikely to be seen outside of a lab environment. To minimize the chance of this race condition occurring to the absolute minimum, do one or more of the following when using master leases with the Base API:

1.  Require all sites to acknowledge transaction commits.

2.  Never change the size of your replication group unless all sites in the group are running and communicating normally with one another.

3.  Don't remove (or replace) a large percentage of your sites from your replication group unless all sites in the group are running and communicating normally with one another. If you are going to remove a large percentage of your sites from your replication group, try removing just one site at a time, pausing in between each removal to give the replication group a chance to fully distribute all writes before removing the next site.
