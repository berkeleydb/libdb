---
title: "Special considerations for two-site replication groups"
api-name: "Special considerations for two-site replication groups"
source: docs/programmer_reference/rep_twosite.html
---
## Special considerations for two-site replication groups

One of the benefits of replication is that it helps your application remain available for writes even when a site crashes. Another benefit is the added durability achieved by storing multiple copies of your application data at different sites. However, if your replication group contains only two sites, you must prioritize which of these benefits is more important to your application.

A two-site replication group is particularly vulnerable to duplicate masters if there is a loss of communication between the sites. The original master continues to accept new transactions. If the original client detects the loss of the master and elects itself master, it also starts accepting new transactions. When communications are restored, there are duplicate masters and one site's new transactions will be rolled back.

If it is unacceptable to your application for any new transactions to be rolled back, the alternative in a two-site replication group is to require both sites to be present in order to elect a master. This stops a client from electing itself master when it loses contact with the master and prevents creation of parallel sets of transactions, one of which must be rolled back.

However, requiring both sites to be present to elect a master results in a loss of write availability when the master crashes. The client cannot take over as master and the replication group exists in a read-only state until the original master site rejoins the replication group.

Replication Manager applications use the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method <a href="../../api/c/repconfig.md#config_DB_REPMGR_CONF_2SITE_STRICT" class="olink">DB_REPMGR_CONF_2SITE_STRICT</a> flag to make this tradeoff between write availability and transaction durability. When this flag is turned on, Replication Manager favors transaction durability. When it is turned off, Replication Manager favors write availability.

A two-site Replication Manager application that uses heartbeats in an environment with frequent communications disruptions generally should operate with the <a href="../../api/c/repconfig.md#config_DB_REPMGR_CONF_2SITE_STRICT" class="olink">DB_REPMGR_CONF_2SITE_STRICT</a> flag turned on. Otherwise, frequent heartbeat failures will cause frequent duplicate masters and the resulting elections and client synchronizations will make one or both sites unavailable for extended periods of time.

Base API applications use the values of the **nvotes** and **nsites** parameters in calls to the <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> method to make this tradeoff. For more information, see <a href="rep_elect.md" class="xref" title="Elections">Elections</a>.

A replication group containing only two electable sites is subject to duplicate masters and rollback of one site's new transactions even when it contains additional unelectable sites. The <a href="../../api/c/repconfig.md#config_DB_REPMGR_CONF_2SITE_STRICT" class="olink">DB_REPMGR_CONF_2SITE_STRICT</a> does not apply in this case because the replication group is larger than two sites.

If both write availability and transaction durability are important to your application, you should strongly consider having three or more electable sites in your replication group. You should also carefully choose an acknowledgement policy that requires at least a quorum of sites. It is best to have an odd number of electable sites to provide a clear majority in the event of a network partition.
