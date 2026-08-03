---
title: "DB_ENV->rep_elect()"
api-name: "DB_ENV->rep_elect()"
source: docs/api_reference/C/repelect.html
---
## DB_ENV-\>rep_elect()

``` c
#include <db.h>

int
DB_ENV->rep_elect(DB_ENV *env,
    u_int32_t nsites, u_int32_t nvotes, u_int32_t flags);  
```

The `DB_ENV->rep_elect()` method holds an election for the master of a replication group.

The `DB_ENV->rep_elect()` method is not called by most replication applications. It should only be called by Base API applications implementing their own network transport layer, explicitly holding replication group elections and handling replication messages outside of the Replication Manager framework.

If the election is successful, Berkeley DB will notify the application of the results of the election by means of either the <a href="envevent_notify.md#event_notify_DB_EVENT_REP_ELECTED" class="link">DB_EVENT_REP_ELECTED</a> or <a href="envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="link">DB_EVENT_REP_NEWMASTER</a> events (see <a href="envevent_notify.md" class="xref" title="DB_ENV-&gt;set_event_notify()">DB_ENV-&gt;set_event_notify()</a> method for more information). The application is responsible for adjusting its relationship to the other database environments in the replication group, including directing all database updates to the newly selected master, in accordance with the results of the election.

The thread of control that calls the `DB_ENV->rep_elect()` method must not be the thread of control that processes incoming messages; processing the incoming messages is necessary to successfully complete an election.

Before calling this method do the following:

- open the database environment by calling the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.

- configure the database environment to send replication messages by calling the <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a> method.

- configure the database environment as a client or a master by calling the <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> method.

### How Elections are Held

Elections are done in two parts: first, replication sites collect information from the other replication sites they know about, and second, replication sites cast their votes for a new master. The second phase is triggered by one of two things: either the replication site gets election information from **nsites** sites, or the election timeout expires. Once the second phase is triggered, the replication site will cast a vote for the new master of its choice if, and only if, the site has election information from at least **nvotes** sites. If a site receives **nvotes** votes for it to become the new master, then it will become the new master.

We recommend **nvotes** be set to at least:

``` c
         (sites participating in the election / 2) + 1
```

to ensure there are never more than two masters active at the same time even in the case of a network partition. When a network partitions, the side of the partition with more than half the environments will elect a new master and continue, while the environments communicating with fewer than half of the environments will fail to find a new master, as no site can get **nvotes** votes.

We recommend **nsites** be set to:

``` c
         number of sites in the replication group - 1
```

when choosing a new master after a current master fails. This allows the group to reach a consensus without having to wait for the timeout to expire.

When choosing a master from among a group of client sites all restarting at the same time, it makes more sense to set **nsites** to the total number of sites in the group, since there is no known missing site. Furthermore, in order to ensure the best choice from among sites that may take longer to boot than the local site, setting **nvotes** also to this same total number of sites will guarantee that every site in the group is considered. Alternatively, using the special timeout for full elections allows full participation on restart but allows election of a master if one site does not reboot and rejoin the group in a reasonable amount of time. (See the <a href="../../guides/programmer_reference/rep_elect.md" class="olink">Elections</a> section in the *Berkeley DB Programmer's Reference Guide* for more information.)

Setting **nsites** to lower values can increase the speed of an election, but can also result in election failure, and is usually not recommended.

### Parameters

#### nsites

The **nsites** parameter specifies the number of replication sites expected to participate in the election. Once the current site has election information from that many sites, it will short-circuit the election and immediately cast its vote for a new master. The **nsites** parameter must be no less than **nvotes**, or 0 if the election should use the value previously set using the <a href="repnsites.md" class="xref" title="DB_ENV-&gt;rep_set_nsites()">DB_ENV-&gt;rep_set_nsites()</a> method. If an application is using master leases, then the value **must** be 0 and the value from <a href="repnsites.md" class="xref" title="DB_ENV-&gt;rep_set_nsites()">DB_ENV-&gt;rep_set_nsites()</a> method must be used.

#### nvotes

The **nvotes** parameter specifies the minimum number of replication sites from which the current site must have election information, before the current site will cast a vote for a new master. The **nvotes** parameter must be no greater than **nsites**, or 0 if the election should use the value ((**nsites** / 2) + 1) as the **nvotes** argument.

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB_ENV->rep_elect()` method may fail and return one of the following non-zero errors:

#### DB_REP_UNAVAIL

The replication group was unable to elect a master, or was unable to complete the election in the election timeout period (see <a href="repset_timeout.md" class="xref" title="DB_ENV-&gt;rep_set_timeout()">DB_ENV-&gt;rep_set_timeout()</a> method for more information).

#### EINVAL

If the database environment was not already configured to communicate with a replication group by a call to <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a>; if the database environment was not already opened; if this method is called from a Replication Manager application; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
