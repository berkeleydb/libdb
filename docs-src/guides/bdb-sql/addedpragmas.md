---
title: "Added PRAGMAs"
api-name: "Added PRAGMAs"
source: docs/bdb-sql/addedpragmas.html
---
## Added PRAGMAs

<span class="sect2"> [PRAGMA bdbsql_error_file](addedpragmas.md#bdbsql_error_file) </span>

<span class="sect2"> [PRAGMA bdbsql_lock_tablesize](addedpragmas.md#bdbsql_lock_tablesize) </span>

<span class="sect2"> [PRAGMA bdbsql_shared_resources](addedpragmas.md#bdbsql_shared_resources) </span>

<span class="sect2"> [PRAGMA bdbsql_single_process](addedpragmas.md#bdbsql_single_process) </span>

<span class="sect2"> [PRAGMA bdbsql_system_memory](addedpragmas.md#bdbsql_system_memory) </span>

<span class="sect2"> [PRAGMA bdbsql_vacuum_fillpercent](addedpragmas.md#bdbsql_vacuum_fillpercent) </span>

<span class="sect2"> [PRAGMA bdbsql_vacuum_pages](addedpragmas.md#bdbsql_vacuum_pages) </span>

<span class="sect2"> [PRAGMA multiversion](addedpragmas.md#multiversion) </span>

<span class="sect2"> [PRAGMA snapshot_isolation](addedpragmas.md#snapshot_isolation) </span>

<span class="sect2"> [PRAGMA trickle](addedpragmas.md#trickle) </span>

<span class="sect2"> [PRAGMA txn_bulk](addedpragmas.md#txn_bulk) </span>

<span class="sect2"> [Replication PRAGMAs](addedpragmas.md#replication_pragmas) </span>

The following PRAGMAs are added in the Berkeley DB SQL interface.

### PRAGMA bdbsql_error_file

``` c
PRAGMA bdbsql_error_file [filename]
```

Redirects internal Berkeley DB error messages to the named file. If a relative path is specified to `[filename]`, then the path is interpreted as being relative to the current working directory.

If this PRAGMA is issued with no filename, then the current target for Berkeley DB error output is returned. By default, error messages are sent to `STDERR`.

This PRAGMA can be issued at any time; initial database access does not have to occur before this PRAGMA can be used.

### PRAGMA bdbsql_lock_tablesize

``` c
PRAGMA bdbsql_lock_tablesize [= N]
```

Sets or reports the number of buckets in the Berkeley DB environment's lock object hash table.

This pragma must be called prior to opening/creating the database environment.

For more details, see <a href="../../api/c/envget_lk_tablesize.md" class="olink">get_lk_tablesize</a> and <a href="../../api/c/envset_lk_tablesize.md" class="olink">set_lk_tablesize</a>.

### PRAGMA bdbsql_shared_resources

``` c
PRAGMA bdbsql_shared_resources [= N]
```

Sets or reports the maximum amount of memory (bytes) to be used by shared structures in the main environment region.

This pragma must be called prior to opening/creating the database environment.

For more details, see <a href="../../api/c/envget_memory_max.md" class="olink">get_memory_max</a> and <a href="../../api/c/envset_memory_max.md" class="olink">set_memory_max</a>.

### PRAGMA bdbsql_single_process

``` c
PRAGMA bdbsql_single_process = boolean
```

To create a private environment rather than a shared environment, enable this pragma. The cache and other region files will be created in memory rather than using file backed shared memory.

In Berkeley DB SQL the default behavior is to allow a database to be opened and operated on by multiple processes simultaneously. When this pragma is enabled, accessing the same database from multiple processes simultaneously can lead to data corruption. Either option supports accessing a database using a single process multi-threaded application.

By default omit sharing is disabled. This pragma must be called prior to opening/creating the database environment. Because the setting is not persistent, you may need to invoke it before every database open, or define compile option `BDBSQL_OMIT_SHARING` instead.

For more information, see <a href="../../guides/programmer_reference/env_region.md" class="olink">Shared memory region</a>. Note that this pragma causes the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag to be specified in the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method.

### PRAGMA bdbsql_system_memory

``` c
PRAGMA bdbsql_system_memory [base segment ID]
```

Queries or sets a flag that causes the database's shared resources to be created in system shared memory. By default the database's shared resources are created in file-backed shared memory.

If a \[base segment ID\] is specified, the shared resources will be created using X/Open style shared memory interfaces. The \[base segment ID\] will be used as the starting ID for shared resources used by the database. Use different \[base segment ID\] values for different databases. It is possible for multi-process applications to use a single database by specifying the same \[base segment ID\] to this PRAGMA. Each connection needs to set this PRAGMA.

This PRAGMA may be used to set a \[base segment ID\] only before the first table is created in the database.

### PRAGMA bdbsql_vacuum_fillpercent

``` c
PRAGMA bdbsql_vacuum_fillpercent [= N]
```

Sets or reports the page full threshold. Any page in the database that is at or below this percentage full is considered for vacuuming when `PRAGMA incremental_vacuum` is enabled. The value is specified as a percentage between 1 and 100. By default, pages 85% full and below are considered for vacuuming.

### PRAGMA bdbsql_vacuum_pages

``` c
PRAGMA bdbsql_vacuum_pages [= N]
```

Sets or reports the maximum number of pages to be returned to the file system from the free page list when incremental vacuuming is enabled. By default, up to 128 pages are removed from the free list.

Page vacuuming is controlled using <a href="changedpragmas.md#auto_vacuum" class="xref" title="PRAGMA auto_vacuum">PRAGMA auto_vacuum</a>.

### PRAGMA multiversion

``` c
PRAGMA multiversion
```

Controls whether Multiversion Concurrency Control (MVCC) is on or off. You can not use this PRAGMA at any time during your application's runtime after your database tables have been accessed.

For more information on MVCC and snapshot isolation, see <a href="mvcc.md" class="xref" title="Using Multiversion Concurrency Control">Using Multiversion Concurrency Control</a>

### PRAGMA snapshot_isolation

``` c
PRAGMA snapshot_isolation
```

Controls whether snapshot isolation is turned on. This PRAGMA can be used at any time during your application's runtime <span class="emphasis">*after*</span> Multiversion Concurrency Control (MVCC) has been turned on.

For more information on MVCC and snapshot isolation, see <a href="mvcc.md" class="xref" title="Using Multiversion Concurrency Control">Using Multiversion Concurrency Control</a>

### PRAGMA trickle

``` c
PRAGMA trickle [percent]
```

Ensures that at least the specified percentage of pages in the shared cache are clean. This can cause pages that have been modified to be flushed to disk.

The trickle functionality enables an application to ensure that a page is available for reading new information into the shared cache without waiting for a write operation to complete.

Specifying this PRAGMA without a percentage value causes the current trickle value to be displayed. Specify `0` to turn the trickle functionality off.

### PRAGMA txn_bulk

``` c
PRAGMA TXN_BULK
```

Enables transactional bulk loading optimization. For more information, see <a href="dbfeatures.md#bulkloading" class="xref" title="Using Bulk Loading">Using Bulk Loading</a>.

### Replication PRAGMAs

Seven PRAGMAs were added to control replication. They are described in <a href="sqlrep.md" class="xref" title="Chapter 4. Using Replication with the SQL API">Using Replication with the SQL API</a>:

### Note

If you are using these replication PRAGMAs and you want to perform a backup, there is an additional backup step for the pragma file. See <a href="admin.md#backup" class="xref" title="Backing Up Berkeley DB SQL Databases">Backing Up Berkeley DB SQL Databases</a> for more information.

- **PRAGMA replication**

- **PRAGMA replication_initial_master**

- **PRAGMA replication_local_site**

- **PRAGMA replication_remote_site**

- **PRAGMA replication_remove_site**

- **PRAGMA replication_verbose_output**

- **PRAGMA replication_verbose_file**
