---
title: "Using Hot Failovers"
api-name: "Using Hot Failovers"
source: docs/gsg_txn/CXX/hotfailover.html
---
## Using Hot Failovers

You can maintain a backup that can be used for failover purposes. Hot failovers differ from the backup and restore procedures described previously in this chapter in that data used for traditional backups is typically copied to offline storage. Recovery time for a traditional backup is determined by:

- How quickly you can retrieve that storage media. Typically storage media for critical backups is moved to a safe facility in a remote location, so this step can take a relatively long time.

- How fast you can read the backup from the storage media to a local disk drive. If you have very large backups, or if your storage media is very slow, this can be a lengthy process.

- How long it takes you to run catastrophic recovery against the newly restored backup. As described earlier in this chapter, this process can be lengthy because every log file must be examined during the recovery process.

When you use a hot failover, the backup is maintained at a location that is reasonably fast to access. Usually, this is a second disk drive local to the machine. In this situation, recovery time is very quick because you only have to reopen your environment and database, using the failover environment for the environment open.

Hot failovers obviously do not protect you from truly catastrophic disasters (such as a fire in your machine room) because the backup is still local to the machine. However, you can guard against more mundane problems (such as a broken disk drive) by keeping the backup on a second drive that is managed by an alternate disk controller.

To maintain a hot failover:

1.  Copy all the active database files to the failover directory. Use the <span class="command">**db_archive**</span> command line utility with the `-s` option to identify all the active database files.

2.  Identify all the inactive log files in your production environment and <span class="emphasis">*move*</span> these to the failover directory. Use the <span class="command">**db_archive**</span> command with no command line options to obtain a list of these log files.

3.  Identify the active log files in your production environment, and <span class="emphasis">*copy*</span> these to the failover directory. Use the <span class="command">**db_archive**</span> command with the `-l` option to obtain a list of these log files.

4.  Run catastrophic recovery against the failover directory. Use the <span class="command">**db_recover**</span> command with the `-c` option to do this.

5.  Optionally copy the backup to an archival location.

Once you have performed this procedure, you can maintain an active hot backup by repeating steps 2 - 5 as often as is required by your application.

### Note

If you perform step 1, steps 2-5 must follow in order to ensure consistency of your hot backup.

### Note

Rather than use the previous procedure, you can use the <span class="command">**db_hotbackup**</span> command line utility to do the same thing. This utility will (optionally) run a checkpoint and then copy all necessary files to a target directory for you.

To actually perform a failover, simply:

1.  Shut down all processes which are running against the original environment.

2.  If you have an archival copy of the backup environment, you can optionally try copying the remaining log files from the original environment and running catastrophic recovery against that backup environment. Do this <span class="emphasis">*only*</span> if you have a an archival copy of the backup environment.

    This step can allow you to recover data created or modified in the original environment, but which did not have a chance to be reflected in the hot backup environment.

3.  Reopen your environment and databases as normal, but use the backup environment instead of the production environment.
