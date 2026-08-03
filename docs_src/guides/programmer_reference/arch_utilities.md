---
title: "Supporting utilities"
api-name: "Supporting utilities"
source: docs/programmer_reference/arch_utilities.html
---
## Supporting utilities

The following are the standalone utilities that provide supporting functionality for the Berkeley DB environment:

<span class="term"><a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility</span>  
The <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility supports database backup and archival, and log file administration. It facilitates log reclamation and the creation of database snapshots. Generally, some form of log archival must be done if a database environment has been configured for logging or transactions.

<span class="term"><a href="../../api/c/db_checkpoint.md" class="olink">db_checkpoint</a> utility</span>  
The <a href="../../api/c/db_checkpoint.md" class="olink">db_checkpoint</a> utility runs as a daemon process, monitoring the database log and periodically issuing checkpoints. It facilitates log reclamation and the creation of database snapshots. Generally, some form of database checkpointing must be done if a database environment has been configured for transactions.

<span class="term"><a href="../../api/c/db_deadlock.md" class="olink">db_deadlock</a> utility</span>  
The <a href="../../api/c/db_deadlock.md" class="olink">db_deadlock</a> utility runs as a daemon process, periodically traversing the database lock structures and aborting transactions when it detects a deadlock. Generally, some form of deadlock detection must be done if a database environment has been configured for locking.

<span class="term"><a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility</span>  
The <a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility writes a copy of the database to a flat-text file in a portable format.

<span class="term"><a href="../../api/c/db_hotbackup.md" class="olink">db_hotbackup</a> utility</span>  
The <a href="../../api/c/db_hotbackup.md" class="olink">db_hotbackup</a> utility creates "hot backup" or "hot failover" snapshots of Berkeley DB database environments.

<span class="term"><a href="../../api/c/db_load.md" class="olink">db_load</a> utility</span>  
The <a href="../../api/c/db_load.md" class="olink">db_load</a> utility reads the flat-text file produced by the <a href="../../api/c/db_load.md" class="olink">db_load</a> utility and loads it into a database file.

<span class="term"><a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility</span>  
The <a href="../../api/c/db_printlog.md" class="olink">db_printlog</a> utility displays the contents of Berkeley DB log files in a human-readable and parsable format.

<span class="term"><a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility</span>  
The <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility runs after an unexpected Berkeley DB or system failure to restore the database to a consistent state. Generally, some form of database recovery must be done if databases are being modified.

<span class="term">db_sql_codegen</span>  
The db_sql_codegen utility translates a schema description written in a SQL Data Definition Language dialect into C code that implements the schema using Berkeley DB.

<span class="term"><a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility</span>  
The <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility displays statistics for databases and database environments.

<span class="term"><a href="../../api/c/db_tuner.md" class="olink">db_tuner</a> utility</span>  
The <a href="../../api/c/db_tuner.md" class="olink">db_tuner</a> utility suggests a page size for btree databases that optimizes cache efficiency and storage space requirements.

<span class="term"><a href="../../api/c/db_upgrade.md" class="olink">db_upgrade</a> utility</span>  
The <a href="../../api/c/db_upgrade.md" class="olink">db_upgrade</a> utility provides a command-line interface for upgrading underlying database formats.

<span class="term"><a href="../../api/c/db_verify.md" class="olink">db_verify</a> utility</span>  
The <a href="../../api/c/db_verify.md" class="olink">db_verify</a> utility provides a command-line interface for verifying the database format.

All of the functionality implemented for these utilities is also available as part of the standard Berkeley DB API. This means that threaded applications can easily create a thread that calls the same Berkeley DB functions as do the utilities. This often simplifies an application environment by removing the necessity for multiple processes to negotiate database and database environment creation and shut down.
