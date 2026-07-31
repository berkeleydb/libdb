---
title: "DB->open()"
api-name: "DB->open()"
source: docs/api_reference/C/dbopen.html
---
## DB-\>open()

``` c
#include <db.h>

int
DB->open(DB *db, DB_TXN *txnid, const char *file,
    const char *database, DBTYPE type, u_int32_t flags, int mode);  
```

The `DB->open()` method opens the database represented by the **file** and **database**.

The currently supported Berkeley DB file formats (or <span class="emphasis">*access methods*</span>) are Btree, Hash, Heap, Queue, and Recno. The Btree format is a representation of a sorted, balanced tree structure. The Hash format is an extensible, dynamic hashing scheme. The Queue format supports fast access to fixed-length records accessed sequentially or by logical record number. The Recno format supports fixed- or variable-length records, accessed sequentially or by logical record number, and optionally backed by a flat text file.

Storage and retrieval for the Berkeley DB access methods are based on key/data pairs; see <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> for more information.

Calling `DB->open()` is a relatively expensive operation, and maintaining a set of open databases will normally be preferable to repeatedly opening and closing the database for each new query.

The `DB->open()` method returns a non-zero error value on failure and 0 on success. If `DB->open()` fails, the <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a> method must be called to discard the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the DB_AUTO_COMMIT flag is specified, the operation will be implicitly transaction protected. Note that transactionally protected operations on a <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle requires the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle itself be transactionally protected during its open. Also note that the transaction must be committed before the handle is closed; see <a href="../../programmer_reference/program_scope.html" class="olink">Berkeley DB handles</a> for more information.

#### file

The **file** parameter is used as the name of an underlying file that will be used to back the database; see <a href="../../programmer_reference/env_naming.html" class="olink">File naming</a> for more information.

In-memory databases never intended to be preserved on disk may be created by setting the **file** parameter to NULL. Whether other threads of control can access this database is driven entirely by whether the **database** parameter is set to NULL.

When using a Unicode build on Windows (the default), the **file** argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

#### database

The **database** parameter is optional, and allows applications to have multiple databases in a single file. Although no **database** parameter needs to be specified, it is an error to attempt to open a second database in a **file** that was not initially created using a **database** name. Further, the **database** parameter is not supported by the Queue format. Finally, when opening multiple databases in the same physical file, it is important to consider locking and memory cache issues; see <a href="../../programmer_reference/am_opensub.html" class="olink">Opening multiple databases in a single file</a> for more information.

If both the **database** and **file** parameters are NULL, the database is strictly temporary and cannot be opened by any other thread of control. Thus the database can only be accessed by sharing the single database handle that created it, in circumstances where doing so is safe.

If the **database** parameter is not set to NULL, the database can be opened by other threads of control and will be replicated to client sites in any replication group, regardless of whether the **file** parameter is set to NULL.

#### type

The **type** parameter is of type DBTYPE, and must be set to one of `DB_BTREE`, `DB_HASH`, `DB_HEAP`, `DB_QUEUE`, `DB_RECNO`, or `DB_UNKNOWN`. If **type** is DB_UNKNOWN, the database must already exist and `DB->open()` will automatically determine its type. The <a href="dbget_type.md" class="xref" title="DB-&gt;get_type()">DB-&gt;get_type()</a> method may be used to determine the underlying type of databases opened using DB_UNKNOWN.

It is an error to specify the incorrect **type** for a database that already exists.

#### flags

The **flags** parameter must be set to zero or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_AUTO_COMMIT`

  Enclose the `DB->open()` call within a transaction. If the call succeeds, the open operation will be recoverable and all subsequent database modification operations based on this handle will be transactionally protected. If the call fails, no database will have been created.

- `DB_CREATE`

  Create the database. If the database does not already exist and the `DB_CREATE` flag is not specified, the `DB->open()` will fail.

- `DB_EXCL`

  Return an error if the database already exists. The `DB_EXCL` flag is only meaningful when specified with the `DB_CREATE`. flag.

- `DB_MULTIVERSION`

  Open the database with support for <a href="../../programmer_reference/transapp_read.html" class="olink">multiversion concurrency control</a>. This will cause updates to the database to follow a copy-on-write protocol, which is required to support snapshot isolation. The `DB_MULTIVERSION` flag requires that the database be transactionally protected during its open and is not supported by the queue format.

- `DB_NOMMAP`

  Do not map this database into process memory (see the <a href="envset_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;set_mp_mmapsize()">DB_ENV-&gt;set_mp_mmapsize()</a> method for further information).

- `DB_RDONLY`

  Open the database for reading only. Any attempt to modify items in the database will fail, regardless of the actual permissions of any underlying files.

- `DB_READ_UNCOMMITTED`

  Support transactional read operations with degree 1 isolation. Read operations on the database may request the return of modified but not yet committed data. This flag must be specified on all <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handles used to perform dirty reads or database updates, otherwise requests for dirty reads may not be honored and the read may block.

- `DB_THREAD`

  Cause the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle returned by `DB->open()` to be <span class="emphasis">*free-threaded*</span>; that is, concurrently usable by multiple threads in the address space.

  Note that this flag is incompatible with the <a href="dbset_lk_exclusive.md" class="link" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> method.

- `DB_TRUNCATE`

  Physically truncate the underlying file, discarding all previous databases it might have held. Underlying filesystem primitives are used to implement this flag. For this reason, it is applicable only to the file and cannot be used to discard databases within a file.

  The `DB_TRUNCATE` flag cannot be lock or transaction-protected, and it is an error to specify it in a locking or transaction-protected environment.

#### mode

On Windows systems, the mode parameter is ignored.

On UNIX systems or in IEEE/ANSI Std 1003.1 (POSIX) environments, files created by the database open are created with mode **mode** (as described in **chmod**(2)) and modified by the process' umask value at the time of creation (see **umask**(2)). Created files are owned by the process owner; the group ownership of created files is based on the system and directory defaults, and is not further specified by Berkeley DB. System shared memory segments created by the database open are created with mode **mode**, unmodified by the process' umask value. If **mode** is 0, the database open will use a default mode of readable and writable by both owner and group.

### Environment Variables

If the database was opened within a database environment, the environment variable **DB_HOME** may be used as the path of the database environment home.

`DB->open()` is affected by any database directory specified using the <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> method, or by setting the "set_data_dir" string in the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file.

- **TMPDIR**

  If the **file** and **dbenv** parameters to `DB->open()` are `NULL`, the environment variable **TMPDIR** may be used as a directory in which to create temporary backing files

### Errors

The `DB->open()` method may fail and return one of the following non-zero errors:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### ENOENT

The file or directory does not exist.

#### ENOENT

A nonexistent **re_source** file was specified.

#### DB_OLD_VERSION

The database cannot be opened without being first upgraded.

#### EEXIST

`DB_CREATE` and `DB_EXCL` were specified and the database exists.

#### EINVAL

If an unknown database type, page size, hash function, pad byte, byte order, or a flag value or parameter that is incompatible with the specified database was specified; the <a href="envopen.md#envopen_DB_THREAD" class="link">DB_THREAD</a> flag was specified and fast mutexes are not available for this architecture; the <a href="envopen.md#envopen_DB_THREAD" class="link">DB_THREAD</a> flag was specified to `DB->open()`, but was not specified to the `DB_ENV->open()` call for the environment in which the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle was created; a backing flat text file was specified with either the <a href="envopen.md#envopen_DB_THREAD" class="link">DB_THREAD</a> flag or the provided database environment supports transaction processing; a Heap database is in use and <a href="dbset_heapsize.md" class="xref" title="DB-&gt;set_heapsize()">DB-&gt;set_heapsize()</a> was used to set a heap size that is different from the value used to create the database or an invalid heap region size was set using <a href="dbset_heap_regionsize.md" class="xref" title="DB-&gt;set_heap_regionsize()">DB-&gt;set_heap_regionsize()</a>; or if an invalid flag value or parameter was specified.

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
