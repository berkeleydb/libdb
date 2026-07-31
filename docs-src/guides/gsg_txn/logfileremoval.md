---
title: "Removing Log Files"
api-name: "Removing Log Files"
source: docs/gsg_txn/C/logfileremoval.html
---
## Removing Log Files

By default DB does not delete log files for you. For this reason, DB's log files will eventually grow to consume an unnecessarily large amount of disk space. To guard against this, you should periodically take administrative action to remove log files that are no longer in use by your application.

You can remove a log file if all of the following are true:

- the log file is not involved in an active transaction.

- a checkpoint has been performed <span class="emphasis">*after*</span> the log file was created.

- the log file is not the only log file in the environment.

- the log file that you want to remove has already been included in an offline or hot backup. Failure to observe this last condition can cause your backups to be unusable.

DB provides several mechanisms to remove log files that meet all but the last criteria (DB has no way to know which log files have already been included in a backup). The following mechanisms make it easy to remove unneeded log files, but can result in an unusable backup if the log files are not first saved to your archive location. All of the following mechanisms automatically delete unneeded log files for you:

- Run the <span class="command">**db_archive**</span> command line utility with the `-d` option.

- From within your application, call the `DB_ENV->log_archive()` method with the `DB_ARCH_REMOVE` flag.

-  Call `DB_ENV->log_set_config()` method with the `DB_LOG_AUTO_REMOVE` flag. Note that this flag can be set at any point in the lifetime of your application. Setting this parameter affects all environment handles opened against the environment; not just the handle used to set the flag.

  Note that unlike the other log removal mechanisms identified here, this method actually causes log files to be removed on an on-going basis as they become unnecessary. This is extremely desirable behavior if what you want is to use the absolute minimum amount of disk space possible for your application. This mechanism <span class="emphasis">*will*</span> leave you with the log files that are required to run normal recovery. However, it is highly likely that this mechanism will prevent you from running catastrophic recovery.

  Do NOT use this mechanism if you want to be able to perform catastrophic recovery, or if you want to be able to maintain a hot backup.

In order to safely remove log files and still be able to perform catastrophic recovery, use the <span class="command">**db_archive**</span> command line utility as follows:

1.  Run either a normal or hot backup as described in <a href="backuprestore.md" class="xref" title="Backup Procedures">Backup Procedures</a>. Make sure that all of this data is safely stored to your backup media before continuing.

2.  If you have not already done so, perform a checkpoint. See <a href="filemanagement.md#checkpoints" class="xref" title="Checkpoints">Checkpoints</a> for more information.

3.  If you are maintaining a hot backup, perform the hot backup procedure as described in <a href="hotfailover.md" class="xref" title="Using Hot Failovers">Using Hot Failovers</a>.

4.  Run the <span class="command">**db_archive**</span> command line utility with the `-d` option against your production environment.

5.  Run the <span class="command">**db_archive**</span> command line utility with the `-d` option against your failover environment, if you are maintaining one.
