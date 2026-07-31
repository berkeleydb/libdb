---
title: "Berkeley DB Concurrent Data Store locking conventions"
api-name: "Berkeley DB Concurrent Data Store locking conventions"
source: docs/programmer_reference/lock_cam_conv.html
---
## Berkeley DB Concurrent Data Store locking conventions

The Berkeley DB Concurrent Data Store product has a simple set of conventions for locking. It provides multiple-reader/single-writer semantics, but not per-page locking or transaction recoverability. As such, it does its locking entirely in the Berkeley DB interface layer.

The object it locks is the file, identified by its unique file number. The locking matrix is not one of the two standard lock modes, instead, we use a four-lock set, consisting of the following:

<span class="term">DB_LOCK_NG</span>  
not granted (always 0)

<span class="term">DB_LOCK_READ</span>  
read (shared)

<span class="term">DB_LOCK_WRITE</span>  
write (exclusive)

<span class="term">DB_LOCK_IWRITE</span>  
intention-to-write (shared with NG and READ, but conflicts with WRITE and IWRITE)

The IWRITE lock is used for cursors that will be used for updating (IWRITE locks are implicitly obtained for write operations through the Berkeley DB handles, for example, <a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a> or <a href="../../api/c/dbdel.md" class="olink">DB-&gt;del()</a>). While the cursor is reading, the IWRITE lock is held; but as soon as the cursor is about to modify the database, the IWRITE is upgraded to a WRITE lock. This upgrade blocks until all readers have exited the database. Because only one IWRITE lock is allowed at any one time, no two cursors can ever try to upgrade to a WRITE lock at the same time, and therefore deadlocks are prevented, which is essential because Berkeley DB Concurrent Data Store does not include deadlock detection and recovery.

Applications that need to lock compatibly with Berkeley DB Concurrent Data Store must obey the following rules:

1.  Use only lock modes DB_LOCK_NG, DB_LOCK_READ, DB_LOCK_WRITE, DB_LOCK_IWRITE.
2.  Never attempt to acquire a WRITE lock on an object that is already locked with a READ lock.
