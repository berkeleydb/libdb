---
title: "Backup Procedures"
api-name: "Backup Procedures"
source: docs/gsg_txn/C/backuprestore.html
---
## Backup Procedures

<span class="sect2"> [About Unix Copy Utilities](backuprestore.md#copyutilities) </span>

<span class="sect2"> [Offline Backups](backuprestore.md#standardbackup) </span>

<span class="sect2"> [Hot Backup](backuprestore.md#hotbackup) </span>

<span class="sect2"> [Incremental Backups](backuprestore.md#incrementalbackups) </span>

<span class="emphasis">*Durability*</span> is an important part of your transactional guarantees. It means that once a transaction has been successfully committed, your application will always see the results of that transaction.

Of course, no software algorithm can guarantee durability in the face of physical data loss. Hard drives can fail, and if you have not copied your data to locations other than your primary disk drives, then you will lose data when those drives fail. Therefore, in order to truly obtain a durability guarantee, you need to ensure that any data stored on disk is backed up to secondary or alternative storage, such as secondary disk drives, or offline tapes.

There are three different types of backups that you can perform with DB databases and log files. They are:

- Offline backups

  This type of backup is perhaps the easiest to perform as it involves simply copying database and log files to an offline storage area. It also gives you a snapshot of the database at a fixed, known point in time. However, you cannot perform this type of a backup while you are performing writes to the database.

- Hot backups

  This type of backup gives you a snapshot of your database. Since your application can be writing to the database at the time that the snapshot is being taken, you do not necessarily know what the exact state of the database is for that given snapshot.

- Incremental backups

  This type of backup refreshes a previously performed backup.

Once you have performed a backup, you can perform <span class="emphasis">*catastrophic recovery*</span> to restore your databases from the backup. See <a href="recovery.md#catastrophicrecovery" class="xref" title="Catastrophic Recovery">Catastrophic Recovery</a> for more information.

Note that you can also maintain a hot failover. See <a href="hotfailover.md" class="xref" title="Using Hot Failovers">Using Hot Failovers</a> for more information.

### About Unix Copy Utilities

If you are copying database files you must copy databases atomically, in multiples of the database page size. In other words, the reads made by the copy program must not be interleaved with writes by other threads of control, and the copy program must read the databases in multiples of the underlying database page size. Generally, this is not a problem because operating systems already make this guarantee and system utilities normally read in power-of-2 sized chunks, which are larger than the largest possible Berkeley DB database page size.

On some platforms (most notably, some releases of Solaris), the copy utility (`cp`) was implemented using the `mmap()` system call rather than the `read()` system call. Because `mmap()` did not make the same guarantee of read atomicity as did `read()`, the `cp` utility could create corrupted copies of the databases.

Also, some platforms have implementations of the `tar` utility that performs 10KB block reads by default. Even when an output block size is specified, the utility will still not read the underlying databases in multiples of the specified block size. Again, the result can be a corrupted backup.

To fix these problems, use the `dd` utility instead of `cp` or `tar`. When you use `dd`, make sure you specify a block size that is equal to, or an even multiple of, your database page size. Finally, if you plan to use a system utility to copy database files, you may want to use a system call trace utility (for example, `ktrace` or `truss`) to make sure you are not using a I/O size that is smaller than your database page size. You can also use these utilities to make sure the system utility is not using a system call other than `read()`.

### Offline Backups

To create an offline backup:

1.  Commit or abort all on-going transactions.

2.  Pause all database writes.

3.  Force a checkpoint. See <a href="filemanagement.md#checkpoints" class="xref" title="Checkpoints">Checkpoints</a> for details.

4.  Copy all your database files to the backup location. Note that you can simply copy all of the database files, or you can determine which database files have been written during the lifetime of the current logs. To do this, use either the `DB_ENV->log_archive()` method with the `DB_ARCH_DATA` option, or use the <span class="command">**db_archive**</span> command with the `-s` option.

    However, be aware that backing up just the modified databases only works if you have all of your log files. If you have been removing log files for any reason then using `log_archive()` can result in an unrecoverable backup because you might not be notified of a database file that was modified.

5.  Copy the <span class="emphasis">*last*</span> log file to your backup location. Your log files are named `log.`<span class="emphasis">*`xxxxxxxxxx`*</span>, where <span class="emphasis">*xxxxxxxxxx*</span> is a sequential number. The last log file is the file with the highest number.

### Hot Backup

To create a hot backup, you do not have to stop database operations. Transactions may be on-going and you can be writing to your database at the time of the backup. However, this means that you do not know exactly what the state of your database is at the time of the backup.

You can use the <span class="command">**db_hotbackup**</span> command line utility to create a hot backup. This program optionally runs a checkpoint, and then copies all necessary files to a target directory.

You can also create your own hot backup facility using the `DB_ENV->backup()` method.

Alternatively, you can manually create a hot backup as follows:

1.   Set the `DB_HOTBACKUP_IN_PROGRESS` flag in your environment. For more information, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/api_reference/C/envset_flags.html" class="ulink" target="_top">DB_ENV-&gt;set_flags() API reference page.</a>

2.  Copy all your database files to the backup location. Note that you can simply copy all of the database files, or you can determine which database files have been written during the lifetime of the current logs. To do this, use either the `DB_ENV->log_archive()` with the `DB_ARCH_DATA` option, or use the <span class="command">**db_archive**</span> command with the `-s` option.

3.  Copy all logs to your backup location.

4.   Reset the `DB_HOTBACKUP_IN_PROGRESS` flag.

### Note

It is important to copy your database files <span class="emphasis">*and then*</span> your logs. In this way, you can complete or roll back any database operations that were only partially completed when you copied the databases.

### Incremental Backups

Once you have created a full backup (that is, either a offline or hot backup), you can create incremental backups. To do this, simply copy all of your currently existing log files to your backup location.

Incremental backups do not require you to run a checkpoint or to cease database write operations.

If your application uses the transactional bulk insert optimization, it is important to know that a database copy taken prior to a bulk loading event can no longer be used as the target of an incremental backup. This is true because bulk loading omits logging of some record insertions, so recovery cannot roll forward these insertions. It is recommended that a full backup be scheduled following a bulk loading event.

For more information, see the description of the `DB_TXN_BULK` flag in the <a href="http://download.oracle.com/docs/cd/E17076_02/html/api_reference/C/txnbegin.html" class="ulink" target="_top">DB_ENV-&gt;txn_begin() API reference page.</a>

When you are working with incremental backups, remember that the greater the number of log files contained in your backup, the longer recovery will take. You should run full backups on some interval, and then do incremental backups on a shorter interval. How frequently you need to run a full backup is determined by the rate at which your databases change and how sensitive your application is to lengthy recoveries (should one be required).

You can also shorten recovery time by running recovery against the backup as you take each incremental backup. Running recovery as you go means that there will be less work for DB to do if you should ever need to restore your environment from the backup.
