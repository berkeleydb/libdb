---
title: "Getting Started with Replicated Berkeley DB Applications"
api-name: "Getting Started with Replicated Berkeley DB Applications"
source: docs/gsg_db_rep/C/index.html
---
# Getting Started with Replicated Berkeley DB Applications

**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

Other names may be trademarks of their respective owners.

To obtain a copy of this document's original source code, please submit a request to the Oracle Technology Network forum at: <a href="http://forums.oracle.com/forums/forum.jspa?forumID=271" class="ulink" target="_top">http://forums.oracle.com/forums/forum.jspa?forumID=271</a>

9/9/2013

------------------------------------------------------------------------

**Table of Contents**

<span class="preface"> [Preface](preface.md) </span>

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

<span class="chapter"> [1. Introduction](introduction.md) </span>

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

<span class="chapter"> [2. Transactional Application](txnapp.md) </span>

<span class="sect1"> [Application Overview](txnapp.md#appoverview) </span>

<span class="sect1"> [Program Listing](simpleprogramlisting.md) </span>

<span class="sect2"> [Function: main()](simpleprogramlisting.md#main_c) </span>

<span class="sect2"> [Function: create_env()](simpleprogramlisting.md#create_env_c) </span>

<span class="sect2"> [Function: env_init()](simpleprogramlisting.md#env_init_c) </span>

<span class="sect2"> [Function: doloop()](simpleprogramlisting.md#doloop_c) </span>

<span class="sect2"> [Function: print_stocks()](simpleprogramlisting.md#printstocks_c) </span>

<span class="chapter"> [3. The DB Replication Manager](repapp.md) </span>

<span class="sect1"> [The DB_SITE Handle](repapp.md#repmgr_grpmgmt) </span>

<span class="sect1"> [Starting and Stopping Replication](rep_init_code.md) </span>

<span class="sect2"> [Managing Election Policies](rep_init_code.md#election_flags) </span>

<span class="sect2"> [Selecting the Number of Threads](rep_init_code.md#thread_count) </span>

<span class="sect1"> [Adding the Replication Manager to ex_rep_gsg_simple](repmgr_init_example_c.md) </span>

<span class="sect1"> [Permanent Message Handling](fwrkpermmessage.md) </span>

<span class="sect2"> [Identifying Permanent Message Policies](fwrkpermmessage.md#fmwrkpermpolicy) </span>

<span class="sect2"> [Setting the Permanent Message Timeout](fwrkpermmessage.md#fmwrkpermtimeout) </span>

<span class="sect2"> [Adding a Permanent Message Policy to ex_rep_gsg_repmgr](fwrkpermmessage.md#perm2fmwrkexample) </span>

<span class="sect1"> [Managing Election Times](electiontimes.md) </span>

<span class="sect2"> [Managing Election Timeouts](electiontimes.md#electiontimeout) </span>

<span class="sect2"> [Managing Election Retry Times](electiontimes.md#electretrytime) </span>

<span class="sect1"> [Managing Connection Retries](fmwrkconnectretry.md) </span>

<span class="sect1"> [Managing Heartbeats](heartbeats.md) </span>

<span class="chapter"> [4. Replica versus Master Processes](fwrkmasterreplica.md) </span>

<span class="sect1"> [Determining State](fwrkmasterreplica.md#determinestate) </span>

<span class="sect1"> [Processing Loop](processingloop.md) </span>

<span class="sect1"> [Example Processing Loop](exampledoloop.md) </span>

<span class="sect2"> [Running It](exampledoloop.md#runningit) </span>

<span class="chapter"> [5. Additional Features](addfeatures.md) </span>

<span class="sect1"> [Delayed Synchronization](addfeatures.md#delayedsync) </span>

<span class="sect1"> [Managing Blocking Operations](manageblock.md) </span>

<span class="sect1"> [Stop Auto-Initialization](autoinit.md) </span>

<span class="sect1"> [Read-Your-Writes Consistency](rywc.md) </span>

<span class="sect1"> [Client to Client Transfer](c2ctransfer.md) </span>

<span class="sect2"> [Identifying Peers](c2ctransfer.md#fmwrkpeerserver) </span>

<span class="sect1"> [Bulk Transfers](bulk.md) </span>
