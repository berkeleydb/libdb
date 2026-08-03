---
title: "Chapter 3.  Access Method Operations"
api-name: "Chapter 3.  Access Method Operations"
source: docs/programmer_reference/am.html
---
## Chapter 3.  Access Method Operations

**Table of Contents**

<span class="sect1"> [Database open](am.md#am_open) </span>

<span class="sect1"> [Opening multiple databases in a single file](am_opensub.md) </span>

<span class="sect2"> [Configuring databases sharing a file](am_opensub.md#idp50943544) </span>

<span class="sect2"> [Caching databases sharing a file](am_opensub.md#idp50944288) </span>

<span class="sect2"> [Locking in databases based on sharing a file](am_opensub.md#idp50944984) </span>

<span class="sect1"> [Partitioning databases](am_partition.md) </span>

<span class="sect2"> [Specifying partition keys](am_partition.md#am_partition_keys) </span>

<span class="sect2"> [Partitioning callback](am_partition.md#am_partition_function) </span>

<span class="sect2"> [Placing partition files](am_partition.md#partition_file_placement) </span>

<span class="sect1"> [Retrieving records](am_get.md) </span>

<span class="sect1"> [Storing records](am_put.md) </span>

<span class="sect1"> [Deleting records](am_delete.md) </span>

<span class="sect1"> [Database statistics](am_stat.md) </span>

<span class="sect1"> [Database truncation](am_truncate.md) </span>

<span class="sect1"> [Database upgrade](am_upgrade.md) </span>

<span class="sect1"> [Database verification and salvage](am_verify.md) </span>

<span class="sect1"> [Flushing the database cache](am_sync.md) </span>

<span class="sect1"> [Database close](am_close.md) </span>

<span class="sect1"> [Secondary indexes](am_second.md) </span>

<span class="sect2"> [Error Handling With Secondary Indexes](am_second.md#idp51040080) </span>

<span class="sect1"> [Foreign key indexes](am_foreign.md) </span>

<span class="sect1"> [Cursor operations](am_cursor.md) </span>

<span class="sect2"> [Retrieving records with a cursor](am_cursor.md#am_curget) </span>

<span class="sect2"> [Storing records with a cursor](am_cursor.md#am_curput) </span>

<span class="sect2"> [Deleting records with a cursor](am_cursor.md#am_curdel) </span>

<span class="sect2"> [Duplicating a cursor](am_cursor.md#am_curdup) </span>

<span class="sect2"> [Equality Join](am_cursor.md#am_join) </span>

<span class="sect2"> [Data item count](am_cursor.md#am_count) </span>

<span class="sect2"> [Cursor close](am_cursor.md#am_curclose) </span>

Once a database handle has been created using <a href="../../api/c/dbcreate.md" class="olink">db_create()</a>, there are several standard access method operations. Each of these operations is performed using a method referred to by the returned handle. Generally, the database will be opened using <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a>. If the database is from an old release of Berkeley DB, it may need to be upgraded to the current release before it is opened using <a href="../../api/c/dbupgrade.md" class="olink">DB-&gt;upgrade()</a>.

Once a database has been opened, records may be retrieved (<a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a>), stored (<a href="../../api/c/dbput.md" class="olink">DB-&gt;put()</a>), and deleted (<a href="../../api/c/dbdel.md" class="olink">DB-&gt;del()</a>).

Additional operations supported by the database handle include statistics (<a href="../../api/c/dbstat.md" class="olink">DB-&gt;stat()</a>), truncation (<a href="../../api/c/dbtruncate.md" class="olink">DB-&gt;truncate()</a>), version upgrade (<a href="../../api/c/dbupgrade.md" class="olink">DB-&gt;upgrade()</a>), verification and salvage (<a href="../../api/c/dbverify.md" class="olink">DB-&gt;verify()</a>), flushing to a backing file (<a href="../../api/c/dbsync.md" class="olink">DB-&gt;sync()</a>), and association of secondary indices (<a href="../../api/c/dbassociate.md" class="olink">DB-&gt;associate()</a>). Database handles are eventually closed using <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a>.

For more information on the access method operations supported by the database handle, see the <a href="../../api/c/db.md#dblist" class="olink">Database and Related Methods</a> section in the *Berkeley DB C API Reference Guide.*

## Database open

The <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> method opens a database, and takes five arguments:

<span class="term">file</span>  
The name of the file to be opened.

<span class="term">database</span>  
An optional database name.

<span class="term">type</span>  
The type of database to open. This value will be one of the five access methods Berkeley DB supports: DB_BTREE, DB_HASH, DB_HEAP, DB_QUEUE or DB_RECNO, or the special value DB_UNKNOWN, which allows you to open an existing file without knowing its type.

<span class="term">mode</span>  
The permissions to give to any created file.

There are a few flags that you can set to customize open:

<span class="term"> <a href="../../api/c/dbopen.md#open_DB_CREATE" class="olink">DB_CREATE</a> </span>  
Create the underlying database and any necessary physical files.

<span class="term"> <a href="../../api/c/dbopen.md#open_DB_NOMMAP" class="olink">DB_NOMMAP</a> </span>  
Do not map this database into process memory.

<span class="term"> <a href="../../api/c/dbopen.md#dbopen_DB_RDONLY" class="olink">DB_RDONLY</a> </span>  
Treat the data base as read-only.

<span class="term"> <a href="../../api/c/dbopen.md#open_DB_THREAD" class="olink">DB_THREAD</a> </span>  
The returned handle is free-threaded, that is, it can be used simultaneously by multiple threads within the process.

<span class="term"> <a href="../../api/c/dbopen.md#open_DB_TRUNCATE" class="olink">DB_TRUNCATE</a> </span>  
Physically truncate the underlying database file, discarding all databases it contained. Underlying filesystem primitives are used to implement this flag. For this reason it is only applicable to the physical file and cannot be used to discard individual databases from within physical files.

<span class="term"> <a href="../../api/c/dbset_feedback.md#set_feedback_DB_UPGRADE" class="olink">DB_UPGRADE</a> </span>  
Upgrade the database format as necessary.
