---
title: "Error returns to applications"
api-name: "Error returns to applications"
source: docs/programmer_reference/program_errorret.html
---
## Error returns to applications

Except for the historic <a href="../../api/c/dbm.md" class="olink">dbm</a>, <a href="../../api/c/dbm.md" class="olink">ndbm</a> and <a href="../../api/c/hsearch.md" class="olink">hsearch</a> interfaces, Berkeley DB does not use the global variable `errno` to return error values. The return values for all Berkeley DB functions are grouped into the following three categories:

<span class="term">0</span>  
A return value of 0 indicates that the operation was successful.

<span class="term">\> 0</span>  
A return value that is greater than 0 indicates that there was a system error. The **errno** value returned by the system is returned by the function; for example, when a Berkeley DB function is unable to allocate memory, the return value from the function will be ENOMEM.

<span class="term">\< 0</span>  
A return value that is less than 0 indicates a condition that was not a system failure, but was not an unqualified success, either. For example, a routine to retrieve a key/data pair from the database may return DB_NOTFOUND when the key/data pair does not appear in the database; as opposed to the value of 0, which would be returned if the key/data pair were found in the database.

All values returned by Berkeley DB functions are less than 0 in order to avoid conflict with possible values of **errno**. Specifically, Berkeley DB reserves all values from -30,800 to -30,999 to itself as possible error values. There are a few Berkeley DB interfaces where it is possible for an application function to be called by a Berkeley DB function and subsequently fail with an application-specific return. Such failure returns will be passed back to the function that originally called a Berkeley DB interface. To avoid ambiguity about the cause of the error, error values separate from the Berkeley DB error name space should be used.

Although possible error returns are specified by each individual function's manual page, there are a few error returns that deserve general mention:

**DB_NOTFOUND and DB_KEYEMPTY**

There are two special return values that are similar in meaning and that are returned in similar situations, and therefore might be confused: DB_NOTFOUND and DB_KEYEMPTY.

The DB_NOTFOUND error return indicates that the requested key/data pair did not exist in the database or that start-of- or end-of-file has been reached by a cursor.

The DB_KEYEMPTY error return indicates that the requested key/data pair logically exists but was never explicitly created by the application (the Recno and Queue access methods will automatically create key/data pairs under some circumstances; see <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> for more information), or that the requested key/data pair was deleted and never re-created. In addition, the Queue access method will return DB_KEYEMPTY for records that were created as part of a transaction that was later aborted and never re-created.

**DB_KEYEXIST**

The DB_KEYEXIST error return indicates the <a href="../../api/c/dbput.md#put_DB_NOOVERWRITE" class="olink">DB_NOOVERWRITE</a> option was specified when inserting a key/data pair into the database and the key already exists in the database, or the <a href="../../api/c/dbput.md#put_DB_NODUPDATA" class="olink">DB_NODUPDATA</a> option was specified and the key/data pair already exists in the data.

**DB_LOCK_DEADLOCK**

When multiple threads of control are modifying the database, there is normally the potential for deadlock. In Berkeley DB, deadlock is signified by an error return from the Berkeley DB function of the value DB_LOCK_DEADLOCK. Whenever a Berkeley DB function returns DB_LOCK_DEADLOCK, the enclosing transaction should be aborted.

Any Berkeley DB function that attempts to acquire locks can potentially return DB_LOCK_DEADLOCK. Practically speaking, the safest way to deal with applications that can deadlock is to anticipate a DB_LOCK_DEADLOCK return from any <a href="../../api/c/db.md" class="olink">DB</a> or <a href="../../api/c/dbc.md" class="olink">DBC</a> handle method call, or any <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle method call that references a database, including the database's backing physical file.

**DB_LOCK_NOTGRANTED**

If a lock is requested from the <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a> or <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> methods with the <a href="../../api/c/lockvec.md#vec_DB_LOCK_NOWAIT" class="olink">DB_LOCK_NOWAIT</a> flag specified, the method will return DB_LOCK_NOTGRANTED if the lock is not immediately available.

If the <a href="../../api/c/envset_flags.md#envset_flags_DB_TIME_NOTGRANTED" class="olink">DB_TIME_NOTGRANTED</a> flag is specified to the <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> method, database calls timing out based on lock or transaction timeout values will return DB_LOCK_NOTGRANTED instead of DB_LOCK_DEADLOCK.

**DB_RUNRECOVERY**

There exists a class of errors that Berkeley DB considers fatal to an entire Berkeley DB environment. An example of this type of error is a corrupted database page. The only way to recover from these failures is to have all threads of control exit the Berkeley DB environment, run recovery of the environment, and re-enter Berkeley DB. (It is not strictly necessary that the processes exit, although that is the only way to recover system resources, such as file descriptors and memory, allocated by Berkeley DB.)

When this type of error is encountered, the error value DB_RUNRECOVERY is returned. This error can be returned by any Berkeley DB interface. Once DB_RUNRECOVERY is returned by any interface, it will be returned from all subsequent Berkeley DB calls made by any threads of control participating in the environment.

Applications can handle such fatal errors in one of two ways: first, by checking for DB_RUNRECOVERY as part of their normal Berkeley DB error return checking, similarly to DB_LOCK_DEADLOCK or any other error. Alternatively, applications can specify a fatal-error callback function using the <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a> method. Applications with no cleanup processing of their own should simply exit from the callback function.

**DB_SECONDARY_BAD**

The DB_SECONDARY_BAD error is returned if a secondary index has been corrupted. This may be the result of an application operating on related databases without first associating them.

**DB_SNAPSHOT_CONFLICT**

The DB_SNAPSHOT_CONFLICT error is returned to a transaction started with the <a href="../../api/c/txnbegin.md" class="olink">DB_TXN_SNAPSHOT_SAFE</a> flag (serializable snapshot isolation, SSI) when Berkeley DB detects a conflicting snapshot update — an attempt to commit an update that conflicts with another concurrent snapshot-safe transaction. The affected transaction must be aborted; the application may then retry it. This return is specific to serializable snapshot isolation and does not occur for plain <a href="../../api/c/txnbegin.md" class="olink">DB_TXN_SNAPSHOT</a> transactions.

**DB_SNAPSHOT_UNSAFE**

The DB_SNAPSHOT_UNSAFE error is returned to a transaction started with the <a href="../../api/c/txnbegin.md" class="olink">DB_TXN_SNAPSHOT_SAFE</a> flag (serializable snapshot isolation, SSI) when Berkeley DB detects a potential serializability anomaly through a read/write anti-dependency: the transaction sits as the pivot of a dangerous structure (it is both the read end and the write end of anti-dependency edges among committed and running transactions) that could otherwise produce a non-serializable schedule. The affected transaction is aborted rather than allowed to commit; the application may then retry it. As with DB_SNAPSHOT_CONFLICT, this return is specific to serializable snapshot isolation.
