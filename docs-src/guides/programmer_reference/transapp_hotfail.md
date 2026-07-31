---
title: "Hot failover"
api-name: "Hot failover"
source: docs/programmer_reference/transapp_hotfail.html
---
## Hot failover

For some applications, it may be useful to periodically snapshot the database environment for use as a hot failover should the primary system fail. The following steps can be taken to keep a backup environment in close synchrony with an active environment. The active environment is entirely unaffected by these procedures, and both read and write operations are allowed during all steps described here.

The procedure described here is not compatible with the concurrent use of the transactional bulk insert optimization (transactions started with the <a href="../../api/c/txnbegin.md#txnbegin_DB_TXN_BULK" class="olink">DB_TXN_BULK</a> flag). After the bulk optimization is used, the archive must be created again from scratch starting with step 1.

The <a href="../../api/c/db_hotbackup.md" class="olink">db_hotbackup</a> utility is the preferred way to automate generating a hot failover system. The first step is to run <a href="../../api/c/db_hotbackup.md" class="olink">db_hotbackup</a> utility without the **-u** flag. This will create hot backup copy of the databases in your environment. After that point periodically running the <a href="../../api/c/db_hotbackup.md" class="olink">db_hotbackup</a> utility with the **-u** flag will copy the new log files and run recovery on the backup copy to bring it current with the primary environment.

Note that you can also create your own hot backup solution using the <a href="../../api/c/envbackup.md" class="olink">DB_ENV-&gt;backup()</a> or <a href="../../api/c/envdbbackup.md" class="olink">DB_ENV-&gt;dbbackup()</a> methods.

To implement your own hot fail over system, the steps below can be followed. However, care should be taken on non-UNIX based systems when copying the database files to be sure that they are either quiescent, or that either the <a href="../../api/c/envbackup.md" class="olink">DB_ENV-&gt;backup()</a> or <a href="../../api/c/db_copy.md" class="olink">db_copy()</a> routine is used to ensure atomic reads of the database pages.

1.  Run the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility with the **-s** option in the active environment to identify all of the active environment's database files, and copy them to the backup directory.

    If the database files are stored in a separate directory from the other Berkeley DB files, it will be simpler (and much faster!) to copy the directory itself instead of the individual files (see <a href="../../api/c/envadd_data_dir.md" class="olink">DB_ENV-&gt;add_data_dir()</a> for additional information).

    ### Note

    If any of the database files did not have an open <a href="../../api/c/db.md" class="olink">DB</a> handle during the lifetime of the current log files, the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility will not list them in its output. This is another reason it may be simpler to use a separate database file directory and copy the entire directory instead of archiving only the files listed by the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility.

2.  Remove all existing log files from the backup directory.

3.  Run the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility with the **-l** option in the active environment to identify all of the active environment's log files, and copy them to the backup directory.

4.  Run the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility with the **-c** option in the backup directory to catastrophically recover the copied environment.

Steps 2, 3 and 4 may be repeated as often as you like. If Step 1 (the initial copy of the database files) is repeated, then Steps 2, 3 and 4 **must** be performed at least once in order to ensure a consistent database environment snapshot.

These procedures must be integrated with your other archival procedures, of course. If you are periodically removing log files from your active environment, you must be sure to copy them to the backup directory before removing them from the active directory. Not copying a log file to the backup directory and subsequently running recovery with it present may leave the backup snapshot of the environment corrupted. A simple way to ensure this never happens is to archive the log files in Step 2 as you remove them from the backup directory, and move inactive log files from your active environment into your backup directory (rather than copying them), in Step 3. The following steps describe this procedure in more detail:

1.  Run the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility with the **-s** option in the active environment to identify all of the active environment's database files, and copy them to the backup directory.
2.  Archive all existing log files from the backup directory, moving them to a backup device such as CD-ROM, alternate disk, or tape.
3.  Run the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility (without any option) in the active environment to identify all of the log files in the active environment that are no longer in use, and **move** them to the backup directory.
4.  Run the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility with the **-l** option in the active environment to identify all of the remaining log files in the active environment, and **copy** the log files to the backup directory.
5.  Run the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility with the **-c** option in the backup directory to catastrophically recover the copied environment.

As before, steps 2, 3, 4 and 5 may be repeated as often as you like. If Step 1 (the initial copy of the database files) is repeated, then Steps 2 through 5 **must** be performed at least once in order to ensure a consistent database environment snapshot.
