---
title: "Chapter 1. Berkeley DB SQL: The Absolute Basics"
api-name: "Chapter 1. Berkeley DB SQL: The Absolute Basics"
source: docs/bdb-sql/dbsqlbasics.html
---
## Chapter 1. Berkeley DB SQL: The Absolute Basics

**Table of Contents**

<span class="sect1"> [BDB SQL Is Nearly Identical to SQLite](dbsqlbasics.md#identicalusage) </span>

<span class="sect1"> [Getting and Installing BDB SQL](buildinstall.md) </span>

<span class="sect2"> [On Windows Systems](buildinstall.md#onwin) </span>

<span class="sect2"> [On Unix](buildinstall.md#onunix) </span>

<span class="sect2"> [The BDB SQL ADO.NET Interface](buildinstall.md#ado_net) </span>

<span class="sect1"> [Accessing BDB SQL Databases](accessing_bdb_sql_databases.title.md) </span>

<span class="sect1"> [The Journal Directory](journaldirectory.md) </span>

<span class="sect1"> [Unsupported PRAGMAs](unsupportedpragmas.md) </span>

<span class="sect1"> [Changed PRAGMAs](changedpragmas.md) </span>

<span class="sect2"> [PRAGMA auto_vacuum](changedpragmas.md#auto_vacuum) </span>

<span class="sect2"> [PRAGMA incremental_vacuum](changedpragmas.md#incremental_vacuum) </span>

<span class="sect2"> [PRAGMA journal_size_limit](changedpragmas.md#journal_size_limit) </span>

<span class="sect1"> [Added PRAGMAs](addedpragmas.md) </span>

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

<span class="sect1"> [Miscellaneous Differences](miscdiff.md) </span>

<span class="sect1"> [Berkeley DB Concepts](bdb-concepts.md) </span>

<span class="sect1"> [Encryption](sql_encryption.md) </span>

<span class="sect1"> [Using Sequences](sequencesupport.md) </span>

<span class="sect2"> [create_sequence](sequencesupport.md#create_sequence) </span>

<span class="sect2"> [nextval](sequencesupport.md#seq_nextval) </span>

<span class="sect2"> [currval](sequencesupport.md#seq_currval) </span>

<span class="sect2"> [drop_sequence](sequencesupport.md#seq_drop_sequence) </span>

<span class="sect1"> [Differences for Users of other SQL Engines](normal-sql.md) </span>

Welcome to the Berkeley DB SQL interface. If you are a SQLite user who is using the BDB SQL interface for reasons other than performance enhancements, this chapter tells you the minimum things you need to know about the interface. You should simply read this chapter and then skip the rest of this book.

If, however, you are using the BDB SQL interface for performance reasons, then you need to read this chapter, plus most of the rest of the chapters in this book (although you can probably skip most of <a href="admin.md" class="xref" title="Chapter 5. Administrating Berkeley DB SQL Databases">Administrating Berkeley DB SQL Databases</a>, unless you want to administer your database "the Berkeley DB way").

Also, if you are an existing Berkeley DB user who is interested in the BDB SQL interface, read this chapter plus the rest of this book.

## BDB SQL Is Nearly Identical to SQLite

Your interaction with the BDB SQL interface is almost identical to SQLite. You use the same APIs, the same command shell environment, the same SQL statements, and the same PRAGMAs to work with the database created by the BDB SQL interface as you would if you were using SQLite.

To learn how to use SQLite, see the official <a href="http://www.sqlite.org/docs.html" class="ulink" target="_top">SQLite Documentation Page.</a>

That said, there are a few small differences between the two interfaces. These are described in the remainder of this chapter.
