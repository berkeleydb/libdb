---
title: "Getting Started with the Oracle Berkeley DB SQL APIs"
api-name: "Getting Started with the Oracle Berkeley DB SQL APIs"
source: docs/bdb-sql/index.html
---
# Getting Started with the Oracle Berkeley DB SQL APIs

**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

Other names may be trademarks of their respective owners.

To obtain a copy of this document's original source code, please submit a request to the Oracle Technology Network forum at: <a href="http://forums.oracle.com/forums/forum.jspa?forumID=271" class="ulink" target="_top">http://forums.oracle.com/forums/forum.jspa?forumID=271</a>

9/9/2013

------------------------------------------------------------------------

**Table of Contents**

<span class="preface"> [Preface](preface.md) </span>

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

<span class="chapter"> [1. Berkeley DB SQL: The Absolute Basics](dbsqlbasics.md) </span>

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

<span class="chapter"> [2. Locking Notes](lockingnotes.md) </span>

<span class="sect1"> [Internal Database Usage](lockingnotes.md#dbusage) </span>

<span class="sect1"> [Lock Handling](lockhandling.md) </span>

<span class="sect2"> [SQLite Lock Usage](lockhandling.md#sqllockmodel) </span>

<span class="sect2"> [Lock Usage with the BDB SQL Interface](lockhandling.md#bdblockusage) </span>

<span class="chapter"> [3. Berkeley DB Features](dbfeatures.md) </span>

<span class="sect1"> [Using Bulk Loading](dbfeatures.md#bulkloading) </span>

<span class="sect1"> [Using Multiversion Concurrency Control](mvcc.md) </span>

<span class="sect1"> [Selecting the Page Size](selectpage_size.md) </span>

<span class="chapter"> [4. Using Replication with the SQL API](sqlrep.md) </span>

<span class="sect1"> [Replication Overview](sqlrep.md#repoverview) </span>

<span class="sect2"> [Replication Masters](sqlrep.md#repmasters) </span>

<span class="sect2"> [Elections](sqlrep.md#repelect) </span>

<span class="sect2"> [Durability Guarantees](sqlrep.md#repdurability) </span>

<span class="sect2"> [Two-Site Replication Groups](sqlrep.md#twositerep) </span>

<span class="sect1"> [Replication PRAGMAs](reppragma.md) </span>

<span class="sect2"> [PRAGMA replication](reppragma.md#pragma_replication) </span>

<span class="sect2"> [PRAGMA replication_initial_master](reppragma.md#pragma_replication_initial_master) </span>

<span class="sect2"> [PRAGMA replication_local_site](reppragma.md#pragma_replication_local_site) </span>

<span class="sect2"> [PRAGMA replication_remote_site](reppragma.md#pragma_replication_remote_site) </span>

<span class="sect2"> [PRAGMA replication_remove_site](reppragma.md#pragma_replication_remove_site) </span>

<span class="sect2"> [PRAGMA replication_verbose_output](reppragma.md#pragma_replication_verbose_output) </span>

<span class="sect2"> [PRAGMA replication_verbose_file](reppragma.md#pragma_replication_verbose_file) </span>

<span class="sect1"> [Displaying Replication Statistics](repstatistics.md) </span>

<span class="sect1"> [Replication Usage Examples](rep_usageexamples.md) </span>

<span class="sect2"> [Example 1: Distributed Read at 3 Sites](rep_usageexamples.md#rep_ex1) </span>

<span class="sect2"> [Example 2: 2-Site Failover](rep_usageexamples.md#rep_ex2) </span>

<span class="chapter"> [5. Administrating Berkeley DB SQL Databases](admin.md) </span>

<span class="sect1"> [Backing Up Berkeley DB SQL Databases](admin.md#backup) </span>

<span class="sect2"> [Backing Up Replicated Berkeley DB SQL Databases](admin.md#idp50739296) </span>

<span class="sect1"> [Syncing with Oracle Databases](sync.md) </span>

<span class="sect2"> [Syncing on Unix Platforms](sync.md#syncunix) </span>

<span class="sect2"> [Syncing on Windows Platforms](sync.md#syncwin) </span>

<span class="sect2"> [Syncing on Windows Mobile Platforms](sync.md#syncwinmobile) </span>

<span class="sect1"> [Data Migration](datamigration.md) </span>

<span class="sect2"> [Migration Using the Shells](datamigration.md#shellmigrate) </span>

<span class="appendix"> [A. Using the BFILE Extension](bfile-extension.md) </span>

<span class="sect1"> [Supported Platforms and Languages](bfile-extension.md#bfile-support) </span>

<span class="sect1"> [BFILE SQL Objects and Functions](bfile-sql.md) </span>

<span class="sect2"> [BFILE_CREATE_DIRECTORY](bfile-sql.md#bfile_create_directory) </span>

<span class="sect2"> [BFILE_REPLACE_DIRECTORY](bfile-sql.md#bfile_replace_directory) </span>

<span class="sect2"> [BFILE_DROP_DIRECTORY](bfile-sql.md#bfile_drop_directory) </span>

<span class="sect2"> [BFILE_NAME](bfile-sql.md#bfile_name) </span>

<span class="sect2"> [BFILE_FULLPATH](bfile-sql.md#bfile_fullpath) </span>

<span class="sect2"> [BFILE_OPEN](bfile-sql.md#bfile_open) </span>

<span class="sect2"> [BFILE_READ](bfile-sql.md#bfile_read) </span>

<span class="sect2"> [BFILE_CLOSE](bfile-sql.md#bfile_close) </span>

<span class="sect2"> [BFILE_SIZE](bfile-sql.md#bfile_size) </span>

<span class="sect1"> [BFILE C/C++ Objects and Functions](bfile-c.md) </span>

<span class="sect2"> [sqlite3_column_bfile](bfile-c.md#sqlite3_column_bfile) </span>

<span class="sect2"> [sqlite3_bfile_open](bfile-c.md#sqlite3_bfile_open) </span>

<span class="sect2"> [sqlite3_bfile_close](bfile-c.md#sqlite3_bfile_close) </span>

<span class="sect2"> [sqlite3_bfile_is_open](bfile-c.md#sqlite3_bfile_is_open) </span>

<span class="sect2"> [sqlite3_bfile_read](bfile-c.md#sqlite3_bfile_read) </span>

<span class="sect2"> [sqlite3_bfile_file_exists](bfile-c.md#sqlite3_bfile_file_exists) </span>

<span class="sect2"> [sqlite3_bfile_size](bfile-c.md#sqlite3_bfile_size) </span>

<span class="sect2"> [sqlite3_bfile_final](bfile-c.md#sqlite3_bfile_final) </span>
