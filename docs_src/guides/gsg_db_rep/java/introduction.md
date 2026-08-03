---
title: "Chapter 1. Introduction"
api-name: "Chapter 1. Introduction"
source: docs/gsg_db_rep/JAVA/introduction.html
---
## Chapter 1. Introduction

**Table of Contents**

<span class="sect1"> [Overview](introduction.md#overview) </span>

<span class="sect2"> [Replication Environments](introduction.md#repenvirons) </span>

<span class="sect2"> [Replication Databases](introduction.md#repdbs) </span>

<span class="sect2"> [Communications Layer](introduction.md#commlayer) </span>

<span class="sect2"> [Selecting a Master](introduction.md#masterselect) </span>

<span class="sect1"> [Replication Benefits](repadvantage.md) </span>

<span class="sect1"> [The Replication APIs](apioverview.md) </span>

<span class="sect2"> [Replication Manager Overview](apioverview.md#repframeworkoverview) </span>

<span class="sect2"> [Replication Base API Overview](apioverview.md#repapioverview) </span>

<span class="sect1"> [Holding Elections](elections.md) </span>

<span class="sect2"> [Influencing Elections](elections.md#influencingelections) </span>

<span class="sect2"> [Winning Elections](elections.md#winningelections) </span>

<span class="sect2"> [Switching Masters](elections.md#switchingmasters) </span>

<span class="sect1"> [Permanent Message Handling](permmessages.md) </span>

<span class="sect2"> [When Not to Manage Permanent Messages](permmessages.md#permmessagenot) </span>

<span class="sect2"> [Managing Permanent Messages](permmessages.md#permmanage) </span>

<span class="sect2"> [Implementing Permanent Message Handling](permmessages.md#permimplement) </span>

This book provides a thorough introduction and discussion on replication as used with Berkeley DB (DB). It begins by offering a general overview to replication and the benefits it provides. It also describes the APIs that you use to implement replication, and it describes architecturally the things that you need to do to your application code in order to use the replication APIs. Finally, it discusses the differences in backup and restore strategies that you might pursue when using replication, especially where it comes to log file removal.

You should understand the concepts from the *Berkeley DB Getting Started with Transaction Processing* guide before reading this book.

## Overview

<span class="sect2"> [Replication Environments](introduction.md#repenvirons) </span>

<span class="sect2"> [Replication Databases](introduction.md#repdbs) </span>

<span class="sect2"> [Communications Layer](introduction.md#commlayer) </span>

<span class="sect2"> [Selecting a Master](introduction.md#masterselect) </span>

The DB replication APIs allow you to distribute your database write operations (performed on a read-write master) to one or more read-only <span class="emphasis">*replicas*</span>. For this reason, DB's replication implementation is said to be a <span class="emphasis">*single master, multiple replica*</span> replication strategy.

Note that your database write operations can occur only on the master; any attempt to write to a replica results in an error being raised by the DB API used to perform the write.

A single replication master and all of its replicas are referred to as a <span class="emphasis">*replication group*</span>. While all members of the replication group can reside on the same machine, usually each replication participant is placed on a separate physical machine somewhere on the network.

Note that all replication applications must first be transactional applications. The data that the master transmits to its replicas are log records that are generated as records are updated. Upon transactional commit, the master transmits a transaction record which tells the replicas to commit the records they previously received from the master. In order for all of this to work, your replicated application must also be a transactional application. For this reason, it is recommended that you write and debug your DB application as a stand-alone transactional application before introducing the replication layer to your code.

### Replication Environments

The most important requirement for a replication participant is that it must use a unique Berkeley DB database environment independent of all other replication participants. So while multiple replication participants can reside on the same physical machine, no two such participants can share the same environment home directory.

For this reason, technically replication occurs between unique <span class="emphasis">*database environments*</span>. So in the strictest sense, a replication group consists of a <span class="emphasis">*master environment*</span> and one or more <span class="emphasis">*replica environments*</span>. However, the reality is that for production code, each such environment will usually be located on its own unique machine. Consequently, this manual sometimes talks about <span class="emphasis">*replication sites*</span>, meaning the unique combination of environment home directory, host and port that a specific replication application is using.

There is no DB-specified limit to the number of environments which can participate in a replication group. The only limitation here is one of resources — network bandwidth, for example.

(Note, however, that the Replication Manager does place a limit on the number of environments you can use. See <a href="apioverview.md#repframeworkoverview" class="xref" title="Replication Manager Overview">Replication Manager Overview</a> for details.)

Also, DB's replication implementation requires all participating environments to be assigned IDs that are locally unique to the given environment. Depending on the replication APIs that you choose to use, you may or may not need to manage this particular detail.

For detailed information on database environments, see the *Berkeley DB Getting Started with Transaction Processing* guide. For more information on environment IDs, see the *Berkeley DB Programmer's Reference Guide*.

### Replication Databases

DB's databases are managed and used in exactly the same way as if you were writing a non-replicated application, with a couple of caveats. First, the databases maintained in a replicated environment must reside either in the `ENV_HOME` directory, or in the directory identified by the `EnvironmentConfig.addDataDir()` method. Unlike non-replication applications, you cannot place your databases in a subdirectory below these locations. You should also not use full path names for your databases or environments as these are likely to break when they are replicated to other machines.

### Communications Layer

In order to transmit database writes to the replication replicas, DB requires a communications layer. DB is agnostic as to what this layer should look like. The only requirement is that it be capable of passing two opaque data objects and an environment ID from the master to its replicas without corruption.

Because replicas are usually placed on different machines on the network, the communications layer is usually some kind of a network-aware implementation. Beyond that, its implementation details are largely up to you. It could use TCP/IP sockets, for example, or it could use raw sockets if they perform better for your particular application.

Note that you may not have to write your own communications layer. DB provides a Replication Manager that includes a fully-functional TCP/IP-based communications layer. See <a href="apioverview.md" class="xref" title="The Replication APIs">The Replication APIs</a> for more information.

See the *Berkeley DB Programmer's Reference Guide* for a description of how to write your own custom replication communications layer.

### Selecting a Master

Every replication group is allowed one and only one master environment. Usually masters are selected by holding an <span class="emphasis">*election*</span>, although it is possible to turn elections off and manually select masters (this is not recommended for most replicated applications).

When elections are being used, they are performed by the underlying Berkeley DB replication code so you have to do very little to implement them.

When holding an election, replicas "vote" on who should be the master. Among replicas participating in the election, the one with the most up-to-date set of log records will win the election. Note that it's possible for there to be a tie. When this occurs, priorities are used to select the master. See <a href="elections.md" class="xref" title="Holding Elections">Holding Elections</a> for details.

For more information on holding and managing elections, see <a href="elections.md" class="xref" title="Holding Elections">Holding Elections</a>.
