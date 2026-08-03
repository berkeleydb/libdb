---
title: "Chapter 16.  The Locking Subsystem"
api-name: "Chapter 16.  The Locking Subsystem"
source: docs/programmer_reference/lock.html
---
## Chapter 16.  The Locking Subsystem

**Table of Contents**

<span class="sect1"> [Introduction to the locking subsystem](lock.md#lock_intro) </span>

<span class="sect1"> [Configuring locking](lock_config.md) </span>

<span class="sect1"> [Configuring locking: sizing the system](lock_max.md) </span>

<span class="sect1"> [Standard lock modes](lock_stdmode.md) </span>

<span class="sect1"> [Deadlock detection](lock_dead.md) </span>

<span class="sect1"> [Deadlock detection using timers](lock_timeout.md) </span>

<span class="sect1"> [Deadlock debugging](lock_deaddbg.md) </span>

<span class="sect1"> [Locking granularity](lock_page.md) </span>

<span class="sect1"> [Locking without transactions](lock_notxn.md) </span>

<span class="sect1"> [Locking with transactions: two-phase locking](lock_twopl.md) </span>

<span class="sect1"> [Berkeley DB Concurrent Data Store locking conventions](lock_cam_conv.md) </span>

<span class="sect1"> [Berkeley DB Transactional Data Store locking conventions](lock_am_conv.md) </span>

<span class="sect1"> [Locking and non-Berkeley DB applications](lock_nondb.md) </span>

## Introduction to the locking subsystem

The locking subsystem provides interprocess and intraprocess concurrency control mechanisms. Although the lock system is used extensively by the Berkeley DB access methods and transaction system, it may also be used as a standalone subsystem to provide concurrency control to any set of designated resources.

The Lock subsystem is created, initialized, and opened by calls to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> with the <a href="../../api/c/envopen.md#envopen_DB_INIT_LOCK" class="olink">DB_INIT_LOCK</a> or <a href="../../api/c/envopen.md#envopen_DB_INIT_CDB" class="olink">DB_INIT_CDB</a> flags specified.

The <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> method is used to acquire and release locks. The <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> method performs any number of lock operations atomically. It also provides the capability to release all locks held by a particular locker and release all the locks on a particular object. (Performing multiple lock operations atomically is useful in performing Btree traversals -- you want to acquire a lock on a child page and once acquired, immediately release the lock on its parent. This is traditionally referred to as <span class="emphasis">*lock-coupling*</span>). Two additional methods, <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a> and <a href="../../api/c/lockput.md" class="olink">DB_ENV-&gt;lock_put()</a>, are provided. These methods are simpler front-ends to the <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> functionality, where <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a> acquires a lock, and <a href="../../api/c/lockput.md" class="olink">DB_ENV-&gt;lock_put()</a> releases a lock that was acquired using <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a> or <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a>. All locks explicitly requested by an application should be released via calls to <a href="../../api/c/lockput.md" class="olink">DB_ENV-&gt;lock_put()</a> or <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a>. Using <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> instead of separate calls to <a href="../../api/c/lockput.md" class="olink">DB_ENV-&gt;lock_put()</a> and <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a> also reduces the synchronization overhead between multiple threads or processes. The three methods are fully compatible, and may be used interchangeably.

Applications must specify lockers and lock objects appropriately. When used with the Berkeley DB access methods, lockers and objects are handled completely internally, but an application using the lock manager directly must either use the same conventions as the access methods or define its own convention to which it adheres. If an application is using the access methods with locking at the same time that it is calling the lock manager directly, the application must follow a convention that is compatible with the access methods' use of the locking subsystem. See <a href="lock_am_conv.md" class="xref" title="Berkeley DB Transactional Data Store locking conventions">Berkeley DB Transactional Data Store locking conventions</a> for more information.

The <a href="../../api/c/lockid.md" class="olink">DB_ENV-&gt;lock_id()</a> function returns a unique ID that may safely be used as the locker parameter to the <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> method. The access methods use <a href="../../api/c/lockid.md" class="olink">DB_ENV-&gt;lock_id()</a> to generate unique lockers for the cursors associated with a database.

The <a href="../../api/c/lockdetect.md" class="olink">DB_ENV-&gt;lock_detect()</a> function provides the programmatic interface to the Berkeley DB deadlock detector. Whenever two threads of control issue lock requests concurrently, the possibility for deadlock arises. A deadlock occurs when two or more threads of control are blocked, waiting for actions that another one of the blocked threads must take. For example, assume that threads A and B have each obtained read locks on object X. Now suppose that both threads want to obtain write locks on object X. Neither thread can be granted its write lock (because of the other thread's read lock). Both threads block and will never unblock because the event for which they are waiting can never happen.

The deadlock detector examines all the locks held in the environment, and identifies situations where no thread can make forward progress. It then selects one of the participants in the deadlock (according to the argument that was specified to <a href="../../api/c/envset_lk_detect.md" class="olink">DB_ENV-&gt;set_lk_detect()</a>), and forces it to return the value <a href="program_errorret.md#program_errorret.DB_LOCK_DEADLOCK" class="link">DB_LOCK_DEADLOCK</a>, which indicates that a deadlock occurred. The thread receiving such an error must release all of its locks and undo any incomplete modifications to the locked resource. Locks are typically released, and modifications undone, by closing any cursors involved in the operation and aborting any transaction enclosing the operation. The operation may optionally be retried.

The <a href="../../api/c/lockstat.md" class="olink">DB_ENV-&gt;lock_stat()</a> function returns information about the status of the lock subsystem. It is the programmatic interface used by the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility.

The locking subsystem is closed by the call to <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a>.

Finally, the entire locking subsystem may be discarded using the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method.

For more information on the locking subsystem methods, see the <a href="../../api/c/lock.md#locklist" class="olink">Locking Subsystem and Related Methods</a> section in the *Berkeley DB C API Reference Guide.*
