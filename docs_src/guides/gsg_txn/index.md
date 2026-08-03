---
title: "Getting Started with Berkeley DB Transaction Processing"
api-name: "Getting Started with Berkeley DB Transaction Processing"
source: docs/gsg_txn/C/index.html
---
# Getting Started with Berkeley DB Transaction Processing

**Language:** C (this page) · [C++](cxx/index.md) · [Java](java/index.md)

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

<span class="sect1"> [Transaction Benefits](introduction.md#txnintro) </span>

<span class="sect1"> [A Note on System Failure](sysfailure.md) </span>

<span class="sect1"> [Application Requirements](apireq.md) </span>

<span class="sect1"> [Multi-threaded and Multi-process Applications](multithread-intro.md) </span>

<span class="sect1"> [Recoverability](recovery-intro.md) </span>

<span class="sect1"> [Performance Tuning](perftune-intro.md) </span>

<span class="chapter"> [2. Enabling Transactions](enabletxn.md) </span>

<span class="sect1"> [Environments](enabletxn.md#environments) </span>

<span class="sect2"> [File Naming](enabletxn.md#filenaming) </span>

<span class="sect2"> [Error Support](enabletxn.md#errorsupport) </span>

<span class="sect2"> [Shared Memory Regions](enabletxn.md#sharedmemory) </span>

<span class="sect2"> [Security Considerations](enabletxn.md#security) </span>

<span class="sect1"> [Opening a Transactional Environment and Database](envopen.md) </span>

<span class="chapter"> [3. Transaction Basics](usingtxns.md) </span>

<span class="sect1"> [Committing a Transaction](usingtxns.md#commitresults) </span>

<span class="sect1"> [Non-Durable Transactions](nodurabletxn.md) </span>

<span class="sect1"> [Aborting a Transaction](abortresults.md) </span>

<span class="sect1"> [Auto Commit](autocommit.md) </span>

<span class="sect1"> [Nested Transactions](nestedtxn.md) </span>

<span class="sect1"> [Transactional Cursors](txncursor.md) </span>

<span class="sect1"> [Secondary Indices with Transaction Applications](txnindices.md) </span>

<span class="sect1"> [Configuring the Transaction Subsystem](maxtxns.md) </span>

<span class="chapter"> [4. Concurrency](txnconcurrency.md) </span>

<span class="sect1"> [Which DB Handles are Free-Threaded](txnconcurrency.md#concurrenthandles) </span>

<span class="sect1"> [Locks, Blocks, and Deadlocks](blocking_deadlocks.md) </span>

<span class="sect2"> [Locks](blocking_deadlocks.md#locks) </span>

<span class="sect2"> [Blocks](blocking_deadlocks.md#blocks) </span>

<span class="sect2"> [Deadlocks](blocking_deadlocks.md#deadlocks) </span>

<span class="sect1"> [The Locking Subsystem](lockingsubsystem.md) </span>

<span class="sect2"> [Configuring the Locking Subsystem](lockingsubsystem.md#configuringlock) </span>

<span class="sect2"> [Configuring Deadlock Detection](lockingsubsystem.md#configdeadlkdetect) </span>

<span class="sect2"> [Resolving Deadlocks](lockingsubsystem.md#deadlockresolve) </span>

<span class="sect2"> [Setting Transaction Priorities](lockingsubsystem.md#setpriority) </span>

<span class="sect1"> [Isolation](isolation.md) </span>

<span class="sect2"> [Supported Degrees of Isolation](isolation.md#degreesofisolation) </span>

<span class="sect2"> [Reading Uncommitted Data](isolation.md#dirtyreads) </span>

<span class="sect2"> [Committed Reads](isolation.md#readcommitted) </span>

<span class="sect2"> [Using Snapshot Isolation](isolation.md#snapshot_isolation) </span>

<span class="sect1"> [Transactional Cursors and Concurrent Applications](txn_ccursor.md) </span>

<span class="sect2"> [Using Cursors with Uncommitted Data](txn_ccursor.md#cursordirtyreads) </span>

<span class="sect1"> [Exclusive Database Handles](exclusivelock.md) </span>

<span class="sect1"> [Read/Modify/Write](readmodifywrite.md) </span>

<span class="sect1"> [No Wait on Blocks](txnnowait.md) </span>

<span class="sect1"> [Reverse BTree Splits](reversesplit.md) </span>

<span class="chapter"> [5. Managing DB Files](filemanagement.md) </span>

<span class="sect1"> [Checkpoints](filemanagement.md#checkpoints) </span>

<span class="sect1"> [Backup Procedures](backuprestore.md) </span>

<span class="sect2"> [About Unix Copy Utilities](backuprestore.md#copyutilities) </span>

<span class="sect2"> [Offline Backups](backuprestore.md#standardbackup) </span>

<span class="sect2"> [Hot Backup](backuprestore.md#hotbackup) </span>

<span class="sect2"> [Incremental Backups](backuprestore.md#incrementalbackups) </span>

<span class="sect1"> [Recovery Procedures](recovery.md) </span>

<span class="sect2"> [Normal Recovery](recovery.md#normalrecovery) </span>

<span class="sect2"> [Catastrophic Recovery](recovery.md#catastrophicrecovery) </span>

<span class="sect1"> [Designing Your Application for Recovery](architectrecovery.md) </span>

<span class="sect2"> [Recovery for Multi-Threaded Applications](architectrecovery.md#multithreadrecovery) </span>

<span class="sect2"> [Recovery in Multi-Process Applications](architectrecovery.md#multiprocessrecovery) </span>

<span class="sect1"> [Using Hot Failovers](hotfailover.md) </span>

<span class="sect1"> [Removing Log Files](logfileremoval.md) </span>

<span class="sect1"> [Configuring the Logging Subsystem](logconfig.md) </span>

<span class="sect2"> [Setting the Log File Size](logconfig.md#logfilesize) </span>

<span class="sect2"> [Configuring the Logging Region Size](logconfig.md#logregionsize) </span>

<span class="sect2"> [Configuring In-Memory Logging](logconfig.md#inmemorylogging) </span>

<span class="sect2"> [Setting the In-Memory Log Buffer Size](logconfig.md#logbuffer) </span>

<span class="chapter"> [6. Summary and Examples](wrapup.md) </span>

<span class="sect1"> [Anatomy of a Transactional Application](wrapup.md#anatomy) </span>

<span class="sect1"> [Transaction Example](txnexample_c.md) </span>

<span class="sect1"> [In-Memory Transaction Example](inmem_txnexample_c.md) </span>
