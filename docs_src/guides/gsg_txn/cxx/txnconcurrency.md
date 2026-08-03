---
title: "Chapter 4. Concurrency"
api-name: "Chapter 4. Concurrency"
source: docs/gsg_txn/CXX/txnconcurrency.html
---
## Chapter 4. Concurrency

**Table of Contents**

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

DB offers a great deal of support for multi-threaded and multi-process applications even when transactions are not in use. Many of DB's handles are thread-safe, or can be made thread-safe by providing the appropriate flag at handle creation time, and DB provides a flexible locking subsystem for managing databases in a concurrent application. Further, DB provides a robust mechanism for detecting and responding to deadlocks . All of these concepts are explored in this chapter.

Before continuing, it is useful to define a few terms that will appear throughout this chapter:

- <span class="emphasis">*Thread of control*</span>

  Refers to a thread that is performing work in your application. Typically, in this book that thread will be performing DB operations.

  Note that this term can also be taken to mean a separate process that is performing work — DB supports multi-process operations on your databases.

  Also, DB is agnostic with regard to the type or style of threads in use in your application. So if you are using multiple threads (as opposed to multiple processes) to perform concurrent database access, you are free to use whatever thread package is best for your platform and application. That said, this manual will use pthreads for its threading examples because those have the best chance of being supported across a large range of platforms.

- <span class="emphasis">*Locking*</span>

  When a thread of control obtains access to a shared resource, it is said to be <span class="emphasis">*locking*</span> that resource. Note that DB supports both exclusive and non-exclusive locks. See <a href="blocking_deadlocks.md#locks" class="xref" title="Locks">Locks</a> for more information.

- <span class="emphasis">*Free-threaded*</span>

  Data structures and objects are free-threaded if they can be shared across threads of control without any explicit locking on the part of the application. Some books, libraries, and programming languages may use the term <span class="emphasis">*thread-safe*</span> for data structures or objects that have this characteristic. The two terms mean the same thing.

  For a description of free-threaded DB objects, see <a href="txnconcurrency.md#concurrenthandles" class="xref" title="Which DB Handles are Free-Threaded">Which DB Handles are Free-Threaded</a>.

- <span class="emphasis">*Blocked*</span>

  When a thread cannot obtain a lock because some other thread already holds a lock on that object, the lock attempt is said to be <span class="emphasis">*blocked*</span>. See <a href="blocking_deadlocks.md#blocks" class="xref" title="Blocks">Blocks</a> for more information.

- <span class="emphasis">*Deadlock*</span>

  Occurs when two or more threads of control attempt to access conflicting resource in such a way as none of the threads can any longer make further progress.

  For example, if Thread A is blocked waiting for a resource held by Thread B, while at the same time Thread B is blocked waiting for a resource held by Thread A, then neither thread can make any forward progress. In this situation, Thread A and Thread B are said to be <span class="emphasis">*deadlocked.*</span>

  For more information, see <a href="blocking_deadlocks.md#deadlocks" class="xref" title="Deadlocks">Deadlocks</a>.

## Which DB Handles are Free-Threaded

The following describes to what extent and under what conditions individual handles are free-threaded.

- `DbEnv`

  Free-threaded so long as the `DB_THREAD` flag is provided to the environment `open()` method.

- `Db`

  Free-threaded so long as the `DB_THREAD` flag is provided to the database `open()` method, or if the database is opened using a free-threaded environment handle.

- `Dbc`

  Cursors are not free-threaded. However, they can be used by multiple threads of control so long as the application serializes access to the handle.

- `DbTxn`

  Access must be serialized by the application across threads of control.
