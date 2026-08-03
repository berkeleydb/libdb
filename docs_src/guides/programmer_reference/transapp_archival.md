---
title: "Database and log file archival"
api-name: "Database and log file archival"
source: docs/programmer_reference/transapp_archival.html
---
## Database and log file archival

The third component of the administrative infrastructure, archival for catastrophic recovery, concerns the recoverability of the database in the face of catastrophic failure. Recovery after catastrophic failure is intended to minimize data loss when physical hardware has been destroyed — for example, loss of a disk that contains databases or log files. Although the application may still experience data loss in this case, it is possible to minimize it.

First, you may want to periodically create snapshots (that is, backups) of your databases to make it possible to recover from catastrophic failure. These snapshots are either a standard backup, which creates a consistent picture of the databases as of a single instant in time; or an on-line backup (also known as a <span class="emphasis">*hot*</span> backup), which creates a consistent picture of the databases as of an unspecified instant during the period of time when the snapshot was made. The advantage of a hot backup is that applications may continue to read and write the databases while the snapshot is being taken. The disadvantage of a hot backup is that more information must be archived, and recovery based on a hot backup is to an unspecified time between the start of the backup and when the backup is completed.

Second, after taking a snapshot, you should periodically archive the log files being created in the environment. It is often helpful to think of database archival in terms of full and incremental filesystem backups. A snapshot is a full backup, whereas the periodic archival of the current log files is an incremental backup. For example, it might be reasonable to take a full snapshot of a database environment weekly or monthly, and archive additional log files daily. Using both the snapshot and the log files, a catastrophic crash at any time can be recovered to the time of the most recent log archival; a time long after the original snapshot.

When incremental backups are implemented using this procedure, it is important to know that a database copy taken prior to a bulk loading event (that is, a transaction started with the <a href="../../api/c/txnbegin.md#txnbegin_DB_TXN_BULK" class="olink">DB_TXN_BULK</a> flag) can no longer be used as the target of an incremental backup. This is true because bulk loading omits logging of some record insertions, so these insertions cannot be rolled forward by recovery. It is recommended that a full backup be scheduled following a bulk loading event.

To create a standard backup of your database that can be used to recover from catastrophic failure, take the following steps:

1.  Commit or abort all ongoing transactions.

2.  Stop writing your databases until the backup has completed. Read-only operations are permitted, but no write operations and no filesystem operations may be performed (for example, the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> and <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> methods may not be called).

3.  Force an environment checkpoint (see the <a href="../../api/c/db_checkpoint.md" class="olink">db_checkpoint</a> utility for more information).

4.  Run the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility with option **-s** to identify all the database data files, and copy them to a backup device such as CD-ROM, alternate disk, or tape.

    If the database files are stored in a separate directory from the other Berkeley DB files, it may be simpler to archive the directory itself instead of the individual files (see <a href="../../api/c/envset_data_dir.md" class="olink">DB_ENV-&gt;set_data_dir()</a> for additional information).

    ### Note

    If any of the database files did not have an open <a href="../../api/c/db.md" class="olink">DB</a> handle during the lifetime of the current log files, the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility will not list them in its output. This is another reason it may be simpler to use a separate database file directory and archive the entire directory instead of archiving only the files listed by the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility.

5.  Run the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility with option **-l** to identify all the log files, and copy the last one (that is, the one with the highest number) to a backup device such as CD-ROM, alternate disk, or tape.

To create a <span class="emphasis">*hot*</span> backup of your database that can be used to recover from catastrophic failure, take the following steps:

1.  Set the <a href="../../api/c/envset_flags.md#set_flags_DB_HOTBACKUP_IN_PROGRESS" class="olink">DB_HOTBACKUP_IN_PROGRESS</a> flag in the environment. This affects the behavior of transactions started with the <a href="../../api/c/txnbegin.md#txnbegin_DB_TXN_BULK" class="olink">DB_TXN_BULK</a> flag.

2.  Archive your databases, as described in the previous step \#4. You do not have to halt ongoing transactions or force a checkpoint. As this is a hot backup, and the databases may be modified during the copy, it is critical that database pages be read atomically as described by <a href="transapp_reclimit.md" class="xref" title="Berkeley DB recoverability">Berkeley DB recoverability</a>.

    Note that only UNIX based systems are known to support the atomicity of reads. These systems include: Solaris, Mac OSX, HPUX and various BSD based systems. Linux and Windows based systems do not support atomic filesystem reads directly. The XFS file system supports atomic reads despite the lack of it in Linux. On systems that do not support atomic file system reads, the <a href="../../api/c/db_hotbackup.md" class="olink">db_hotbackup</a> utility should be used or a tool can be constructed using the <a href="../../api/c/envbackup.md" class="olink">DB_ENV-&gt;backup()</a> method. Alternatively, you can construct a tool using the the <a href="../../api/c/db_copy.md" class="olink">db_copy()</a> method. You can also perform a hot backup of just a single database in your environment using the <a href="../../api/c/envdbbackup.md" class="olink">DB_ENV-&gt;dbbackup()</a> method.

3.  Archive **all** of the log files. The order of these two operations is required, and the database files must be archived **before** the log files. This means that if the database files and log files are in the same directory, you cannot simply archive the directory; you must make sure that the correct order of archival is maintained.

    To archive your log files, run the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility using the **-l** option to identify all the database log files, and copy them to your backup media. If the database log files are stored in a separate directory from the other database files, it may be simpler to archive the directory itself instead of the individual files (see the <a href="../../api/c/envset_lg_dir.md" class="olink">DB_ENV-&gt;set_lg_dir()</a> method for more information).

4.  Reset the <a href="../../api/c/envset_flags.md#set_flags_DB_HOTBACKUP_IN_PROGRESS" class="olink">DB_HOTBACKUP_IN_PROGRESS</a> flag.

To minimize the archival space needed for log files when doing a hot backup, run db_archive to identify those log files which are not in use. Log files which are not in use do not need to be included when creating a hot backup, and you can discard them or move them aside for use with previous backups (whichever is appropriate), before beginning the hot backup.

After completing one of these two sets of steps, the database environment can be recovered from catastrophic failure (see <a href="transapp_recovery.md" class="xref" title="Recovery procedures">Recovery procedures</a> for more information).

To update either a hot or cold backup so that recovery from catastrophic failure is possible to a new point in time, repeat step \#2 under the hot backup instructions and archive **all** of the log files in the database environment. Each time both the database and log files are copied to backup media, you may discard all previous database snapshots and saved log files. Archiving additional log files does not allow you to discard either previous database snapshots or log files. Generally, updating a backup must be integrated with the application's log file removal procedures.

The time to restore from catastrophic failure is a function of the number of log records that have been written since the snapshot was originally created. Perhaps more importantly, the more separate pieces of backup media you use, the more likely it is that you will have a problem reading from one of them. For these reasons, it is often best to make snapshots on a regular basis.

**Obviously, the reliability of your archive media will affect the safety of your data. For archival safety, ensure that you have multiple copies of your database backups, verify that your archival media is error-free and readable, and that copies of your backups are stored offsite!**

The functionality provided by the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility is also available directly from the Berkeley DB library. The following code fragment prints out a list of log and database files that need to be archived:

``` c
void
log_archlist(DB_ENV *dbenv)
{
    int ret;
    char **begin, **list;

    /* Get the list of database files. */
    if ((ret = dbenv->log_archive(dbenv,
        &list, DB_ARCH_ABS | DB_ARCH_DATA)) != 0) {
        dbenv->err(dbenv, ret, "DB_ENV->log_archive: DB_ARCH_DATA");
        exit (1);
    }
    if (list != NULL) {
        for (begin = list; *list != NULL; ++list)
            printf("database file: %s\n", *list);
        free (begin);
    }

    /* Get the list of log files. */
    if ((ret = dbenv->log_archive(dbenv,
        &list, DB_ARCH_ABS | DB_ARCH_LOG)) != 0) {
        dbenv->err(dbenv, ret, "DB_ENV->log_archive: DB_ARCH_LOG");
        exit (1);
    }
    if (list != NULL) {
        for (begin = list; *list != NULL; ++list)
            printf("log file: %s\n", *list);
        free (begin);
    }
}
```
