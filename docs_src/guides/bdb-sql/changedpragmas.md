---
title: "Changed PRAGMAs"
api-name: "Changed PRAGMAs"
source: docs/bdb-sql/changedpragmas.html
---
## Changed PRAGMAs

<span class="sect2"> [PRAGMA auto_vacuum](changedpragmas.md#auto_vacuum) </span>

<span class="sect2"> [PRAGMA incremental_vacuum](changedpragmas.md#incremental_vacuum) </span>

<span class="sect2"> [PRAGMA journal_size_limit](changedpragmas.md#journal_size_limit) </span>

The following PRAGMAs are available in the BDB SQL interface, but they behave differently in some way from standard SQLite.

### PRAGMA auto_vacuum

The syntax for this PRAGMA is:

``` c
PRAGMA auto_vacuum
PRAGMA auto_vacuum = 0 | NONE | 1 | FULL | 2 | INCREMENTAL 
```

Standard SQLite does not allow you to enable or disable auto-vacuum after a table has been created. Berkeley DB, however, allows you to change this at any time.

In the previous syntax, `0` and `NONE` both turn off auto vacuuming.

`1` or `FULL` causes full vacuuming to occur. That is, the BDB SQL interface will vacuum the entire database at each commit using a very low fill percentage (1%) in order to return emptied pages to the file system. Because Berkeley DB allows you to call this PRAGMA at any time, it is recommended that you do not turn on FULL vacuuming because doing so can result in a great deal of overhead to your transaction commits.

If `2` or `INCREMENTAL` is used, then incremental vacuuming is enabled. The amount of vacuuming that is performed for incremental vacuum is controlled using the following PRAGMAs:

|  |
|----|
| <a href="addedpragmas.md#bdbsql_vacuum_fillpercent" class="xref" title="PRAGMA bdbsql_vacuum_fillpercent">PRAGMA bdbsql_vacuum_fillpercent</a> |
| <a href="addedpragmas.md#bdbsql_vacuum_pages" class="xref" title="PRAGMA bdbsql_vacuum_pages">PRAGMA bdbsql_vacuum_pages</a> |

Note that you can call <a href="changedpragmas.md#incremental_vacuum" class="xref" title="PRAGMA incremental_vacuum">PRAGMA incremental_vacuum</a> to perform an incremental vacuum operation on demand.

When performing vacuum operations, Berkeley DB defragments and repacks individual database pages, while SQLite only truncates the freelist pages from the database file.

For more information on auto vacuum, see <a href="http://www.sqlite.org/pragma.html#pragma_auto_vacuum" class="ulink" target="_top">PRAGMA auto_vacuum</a> in the SQLite documentation.

### PRAGMA incremental_vacuum

Performs incremental vacuum operations on demand. You can cause incremental vacuum operations to be performed automatically using <a href="changedpragmas.md#auto_vacuum" class="xref" title="PRAGMA auto_vacuum">PRAGMA auto_vacuum</a>.

Note that for SQLite, this PRAGMA is used to specify the maximum number of pages to be freed during vacuuming. For Berkeley DB, you use `PRAGMA bdbsql_vacuum_pages` instead.

### PRAGMA journal_size_limit

For standard SQLite, this pragma identifies the maximum size that the journal file is allowed to be.

Berkeley DB uses multiple journal files, Berkeley DB journal files are different to a SQLite journal file in that they contain information about multiple transactions, rather than a single transaction (similar to the SQLite WAL journal file). Over the course of the database's lifetime, Berkeley DB will probably create multiple journal files. A new journal file is created when the current journal file has reached the maximum size configured using the journal_size_limit pragma.

Note that a BDB SQL interface journal file is referred to as a log file in the Berkeley DB documentation.
