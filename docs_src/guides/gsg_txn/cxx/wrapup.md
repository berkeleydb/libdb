---
title: "Chapter 6. Summary and Examples"
api-name: "Chapter 6. Summary and Examples"
source: docs/gsg_txn/CXX/wrapup.html
---
## Chapter 6. Summary and Examples

**Table of Contents**

<span class="sect1"> [Anatomy of a Transactional Application](wrapup.md#anatomy) </span>

<span class="sect1"> [Transaction Example](txnexample_c.md) </span>

<span class="sect1"> [In-Memory Transaction Example](inmem_txnexample_c.md) </span>

Throughout this manual we have presented the concepts and mechanisms that you need to provide transactional protection for your application. In this chapter, we summarize these mechanisms, and we provide a complete example of a multi-threaded transactional DB application.

## Anatomy of a Transactional Application

Transactional applications are characterized by performing the following activities:

1.  Create your environment handle.

2.  Open your environment, specifying that the following subsystems be used:

    - Transactional Subsystem (this also initializes the logging subsystem).

    - Memory pool (the in-memory cache).

    - Logging subsystem.

    - Locking subsystem (if your application is multi-process or multi-threaded).

    It is also highly recommended that you run normal recovery upon first environment open. Normal recovery examines only those logs required to ensure your database files are consistent relative to the information found in your log files.

3.  Optionally spawn off any utility threads that you might need. Utility threads can be used to run checkpoints periodically, or to periodically run a deadlock detector if you do not want to use DB's built-in deadlock detector.

4.  Open whatever database handles that you need.

5.  Spawn off worker threads. How many of these you need and how they split their DB workload is entirely up to your application's requirements. However, any worker threads that perform write operations will do the following:

    1.  Begin a transaction.

    2.  Perform one or more read and write operations.

    3.  Commit the transaction if all goes well.

    4.  Abort and retry the operation if a deadlock is detected.

    5.  Abort the transaction for most other errors.

6.  On application shutdown:

    1.  Make sure there are no opened cursors.

    2.  Make sure there are no active transactions. Either abort or commit all transactions before shutting down.

    3.  Close your databases.

    4.  Close your environment.

### Note

Robust DB applications should monitor their worker threads to make sure they have not died unexpectedly. If a thread does terminate abnormally, you must shutdown all your worker threads and then run normal recovery (you will have to reopen your environment to do this). This is the only way to clear any resources (such as a lock or a mutex) that the abnormally exiting worker thread might have been holding at the time that it died.

Failure to perform this recovery can cause your still-functioning worker threads to eventually block forever while waiting for a lock that will never be released.

In addition to these activities, which are all entirely handled by code within your application, there are some administrative activities that you should perform:

- Periodically checkpoint your application. Checkpoints will reduce the time to run recovery in the event that one is required. See <a href="filemanagement.md#checkpoints" class="xref" title="Checkpoints">Checkpoints</a> for details.

- Periodically back up your database and log files. This is required in order to fully obtain the durability guarantee made by DB's transaction ACID support. See <a href="backuprestore.md" class="xref" title="Backup Procedures">Backup Procedures</a> for more information.

- You may want to maintain a hot failover if 24x7 processing with rapid restart in the face of a disk hit is important to you. See <a href="hotfailover.md" class="xref" title="Using Hot Failovers">Using Hot Failovers</a> for more information.
