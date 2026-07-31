---
title: "Chapter 4. Using Replication with the SQL API"
api-name: "Chapter 4. Using Replication with the SQL API"
source: docs/bdb-sql/sqlrep.html
---
## Chapter 4. Using Replication with the SQL API

**Table of Contents**

<span class="sect1"> [Replication Overview](sqlrep.md#repoverview) </span>

<span class="sect2"> [Replication Masters](sqlrep.md#repmasters) </span>

<span class="sect2"> [Elections](sqlrep.md#repelect) </span>

<span class="sect2"> [Durability Guarantees](sqlrep.md#repdurability) </span>

<span class="sect2"> [Two-Site Replication Groups](sqlrep.md#twositerep) </span>

<span class="sect1"> [Replication PRAGMAs](reppragma.md) </span>

<span class="sect2"> [PRAGMA replication](reppragma.md#pragma_replication) </span>

<span class="sect2"> [PRAGMA replication_initial_master](reppragma.md#pragma_replication_initial_master) </span>

<span class="sect2"> [PRAGMA replication_local_site](reppragma.md#pragma_replication_local_site) </span>

<span class="sect2"> [PRAGMA replication_remote_site](reppragma.md#pragma_replication_remote_site) </span>

<span class="sect2"> [PRAGMA replication_remove_site](reppragma.md#pragma_replication_remove_site) </span>

<span class="sect2"> [PRAGMA replication_verbose_output](reppragma.md#pragma_replication_verbose_output) </span>

<span class="sect2"> [PRAGMA replication_verbose_file](reppragma.md#pragma_replication_verbose_file) </span>

<span class="sect1"> [Displaying Replication Statistics](repstatistics.md) </span>

<span class="sect1"> [Replication Usage Examples](rep_usageexamples.md) </span>

<span class="sect2"> [Example 1: Distributed Read at 3 Sites](rep_usageexamples.md#rep_ex1) </span>

<span class="sect2"> [Example 2: 2-Site Failover](rep_usageexamples.md#rep_ex2) </span>

The Berkeley DB SQL interface allows you to use Berkeley DB's replication feature. You configure and start replication using PRAGMAs that are specific to the task.

This chapter provides a high-level introduction of Berkeley DB replication. It then shows how to configure and use replication with the SQL API.

For a more detailed description of Berkeley DB replication, see:

- *Berkeley DB Getting Started with Replicated Applications*

- *Berkeley DB Programmer's Reference Guide*

### Note

You cannot access a BDB SQL database using multiple processes if you enable replication for that database.

## Replication Overview

<span class="sect2"> [Replication Masters](sqlrep.md#repmasters) </span>

<span class="sect2"> [Elections](sqlrep.md#repelect) </span>

<span class="sect2"> [Durability Guarantees](sqlrep.md#repdurability) </span>

<span class="sect2"> [Two-Site Replication Groups](sqlrep.md#twositerep) </span>

Berkeley DB's replication feature allows you to automatically distribute your database write operations to one or more read-only <span class="emphasis">*replicas*</span>. For this reason, BDB's replication implementation is said to be a <span class="emphasis">*single master, multiple replica*</span> replication strategy.

A single replication master and all of its replicas are referred to as a <span class="emphasis">*replication group*</span>. Each replication group can have one and only one master site.

When discussing Berkeley DB replication, we sometimes refer to <span class="emphasis">*replication sites*</span>. This is because most production applications place each of their replication participants on separate physical machines. In fact, each replication participant must be assigned a hostname/port pair that is unique within the replication group.

Note that under the hood, the unit of replication is the environment. That is, data is replicated from one Berkeley DB environment to one or more other Berkeley DB environments. However, when used with the BDB SQL interface, you can think of this as replicating between Berkeley DB databases, because the BDB SQL interface results in a single database file for each environment.

### Replication Masters

Every replication group has one and only one master. The master site is where you perform write operations. These operations are then automatically replicated to the other sites in the replication group. Because the other replica sites in the replication group are read-only, it is an error for you to attempt to perform write operatons on them.

The replication master is usually automatically selected by the replication group using elections. Replication elections simply determine which replication site has the most up-to-date copy of the data, and so is in the best position to serve as the master site.

Note that when you initially start up your BDB SQL replicated application, you must explicitly designate a specific site as the master. Over time, the master site can move from one environment to the next. For example, if the master site is shut down, becomes unavailable, or a network partition causes it to lose contact with the rest of the replication group, then the replication group will elect a new master if it can successfully hold an election. When the old master comes back online, it rejoins the replication group as a read-only replica site.

Also, if you are enabling replication for an existing database, then that database must be designated as the master. Doing this is required; otherwise the entire contents of the existing database might be deleted during the replication startup process.

### Elections

A replication group selects the master site by holding an election. In simplistic terms, each participant in the replication group votes on who it believes has the most up-to-date version of the data that the replication group is managing. The site that receives the most number of votes becomes the master site, and all data write activity must occur there.

In order to hold an election, the replication group must have a quorum. In order to achieve a quorum, a simple majority of the sites must be available to select the master. That is, <span class="emphasis">*n/2 + 1*</span> sites must be available, where <span class="emphasis">*n*</span> is the total number of replication group participants. By requiring a simple majority, the replication group avoids the possibility of simultaneously running with two master sites due to a network partition.

If a replication group cannot select a master, then it can only be used in read-only mode.

### Durability Guarantees

Durability is a term that means data modifications have met some pre-defined set of guarantees that the modifications will remain persistent across application run times. Usually, this means that there is some assurance that the data modification has been written to stable storage (that is, written to a hard drive).

For replicated BDB SQL applications, the durability guarantee is extended because data modifications are also replicated to those environments that are participating in the replication group. This ensures higher data durability than non-replicated applications by placing data in multiple environments that usually reside on separate physical machines.

### Two-Site Replication Groups

In a replication group that consists of exactly two sites, both sites must be available in order to achieve a quorum. Without a quorum, a new master site cannot be elected. This means that if the master site is unable to participate in the replication group, then the remaining read-only replica cannot become the master site.

In other words, if you have a group that consists of exactly two sites, if you lose your master site then the replication group must exist in read-only mode until the master site becomes available again.
