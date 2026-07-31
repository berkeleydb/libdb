---
title: "Chapter 2.  Upgrading from previous versions of Berkeley DB"
api-name: "Chapter 2.  Upgrading from previous versions of Berkeley DB"
source: docs/upgrading/upgrade_process.html
---
## Chapter 2.  Upgrading from previous versions of Berkeley DB

The following information describes the general process of upgrading Berkeley DB installations. There are four areas to be considered when upgrading Berkeley DB applications and database environments: the application API, the database environment's region files, the underlying database formats, and, in the case of transactional database environments, the log files. The upgrade procedures required depend on whether or not the release is a major or minor release (in which either the major or minor number of the version changed), or a patch release (in which only the patch number in the version changed). Berkeley DB major and minor releases may optionally include changes in all four areas, that is, the application API, region files, database formats, and log files may not be backward-compatible with previous releases.

Each Berkeley DB major or minor release described in this book has a chapter indicating how to upgrade to the new release. The chapter describes any API changes made in the release. Application maintainers should review the API changes and update their applications as necessary before recompiling with the new release. In addition, each chapter includes a section specifying whether the log file format or database formats changed in non-backward-compatible ways as part of the release. Because there are several underlying Berkeley DB database formats, and they do not all necessarily change in the same release, changes to a database format in a release may not affect any particular application. Further, database and log file formats may have changed but be entirely backward-compatible, in which case no upgrade will be necessary.

A Berkeley DB patch release will never modify the API, regions, log files, or database formats in incompatible ways, and so applications need only be relinked (or, in the case of a shared library, pointed at the new version of the shared library) to upgrade to a new release. Note that internal Berkeley DB interfaces may change at any time and in any release (including patch releases) without warning. This means the library must be entirely recompiled and reinstalled when upgrading to new releases of the library because there is no guarantee that modules from one version of the library will interact correctly with modules from another release. We recommend using the same compiler release when building patch releases as was used to build the original release; in the default configuration, the Berkeley DB library shares data structures from underlying shared memory between threads of control, and should the compiler re-order fields or otherwise change those data structures between the two builds, errors may result.

If the release is a patch release, do the following:

1.  Shut down the old version of the application.

2.  Install the new version of the application by relinking or installing a new version of the Berkeley DB shared library.

3.  Restart the application.

Otherwise, if the application **does not** have a Berkeley DB transactional environment, the application may be installed in the field using the following steps:

1.  Shut down the old version of the application.

2.  Remove any Berkeley DB environment using the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method or an appropriate system utility.

3.  Recompile and install the new version of the application.

4.  If necessary, upgrade the application's databases. See <a href="../../guides/programmer_reference/am_upgrade.md" class="olink">Database upgrade</a> for more information.

5.  Restart the application.

Otherwise, if the application has a Berkeley DB transactional environment, but neither the log file nor database formats need upgrading, the application may be installed in the field using the following steps:

1.  Shut down the old version of the application.

2.  Run recovery on the database environment using the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method or the <a href="../../api/c/db_recover.md" class="olink">db_recover utility</a>.

3.  Remove any Berkeley DB environment using the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method or an appropriate system utility.

4.  Recompile and install the new version of the application.

5.  Restart the application.

If the application has a Berkeley DB transactional environment, and the log files need upgrading but the databases do not, the application may be installed in the field using the following steps:

1.  Shut down the old version of the application.

2.  Still using the old version of Berkeley DB, run recovery on the database environment using the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method, or the <a href="../../api/c/db_recover.md" class="olink">db_recover utility</a>.

3.  If you used the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method to run recovery, make sure that the Berkeley DB environment is removed using the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method or an appropriate system utility.

4.  Archive the database environment for catastrophic recovery using the `db_archive` utility as described in the <a href="../../guides/programmer_reference/transapp_archival.md" class="olink">Database and log file archival</a> section in the *Berkeley DB Programmer's Reference Guide*.

5.  Recompile and install the new version of the application.

6.  Force a checkpoint using the <a href="../../api/c/txncheckpoint.md" class="olink">DB_ENV-&gt;txn_checkpoint()</a> method or the <a href="../../api/c/db_checkpoint.md" class="olink">db_checkpoint utility</a>. If you use the <a href="../../api/c/db_checkpoint.md" class="olink">db_checkpoint utility</a>, make sure to use the new version of the utility; that is, the version that came with the release of Berkeley DB to which you are upgrading.

    Note that forcing a checkpoint might result in warning messages about log files that are being skipped. This is normal, and can be safely ignored.

7.  Remove unnecessary log files from the environment using the `-d` option on the <a href="../../api/c/db_archive.md" class="olink">db_archive utility</a>, or from an application which calls the <a href="../../api/c/logarchive.md" class="olink">DB_ENV-&gt;log_archive()</a> method with the <a href="../../api/c/logarchive.md#archive_DB_ARCH_REMOVE" class="olink">DB_ARCH_REMOVE</a> flag.

    Note that removing log files in this way might result in warning messages about log files that are being skipped. This is normal, and can be safely ignored.

    Note that if you are upgrading a replicated application, then you should <span class="emphasis">*not*</span> perform this step until all of the replication sites have been upgraded to the current release level. If you run this site before all your sites are upgraded, then errors can occur in your replication activities because important version information might be lost.

8.  Restart the application.

Otherwise, if the application has a Berkeley DB transactional environment and the databases need upgrading, the application may be installed in the field using the following steps:

1.  Shut down the old version of the application.

2.  Still using the old version of Berkeley DB, run recovery on the database environment using the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method, or the <a href="../../api/c/db_recover.md" class="olink">db_recover utility</a>.

3.  If you used the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method to run recovery, make sure that the Berkeley DB environment is removed using the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method or an appropriate system utility.

4.  Archive the database environment for catastrophic recovery using the `db_archive` utility as described in the <a href="../../guides/programmer_reference/transapp_archival.md" class="olink">Database and log file archival</a> section in the *Berkeley DB Programmer's Reference Guide*.

5.  Recompile and install the new version of the application.

6.  Upgrade the application's databases. See <a href="../../guides/programmer_reference/am_upgrade.md" class="olink">Database upgrade</a> for more information.

7.  Archive the database for catastrophic recovery again (using different media than before, of course). Note: This archival is not strictly necessary. However, if you have to perform catastrophic recovery after restarting the application, that recovery must be done based on the last archive you have made. If you make this second archive, you can use it as the basis of that catastrophic recovery. If you do not make this second archive, you have to use the archive you made in step 4 as the basis of your recovery, and you have to do a full upgrade on it before you can apply log files created after the upgrade to it.

8.  Force a checkpoint using the <a href="../../api/c/txncheckpoint.md" class="olink">DB_ENV-&gt;txn_checkpoint()</a> method or the <a href="../../api/c/db_checkpoint.md" class="olink">db_checkpoint utility</a>. If you use the <a href="../../api/c/db_checkpoint.md" class="olink">db_checkpoint utility</a>, make sure to use the new version of the utility; that is, the version that came with the release of Berkeley DB to which you are upgrading.

    Note that forcing a checkpoint might result in warning messages about log files that are being skipped. This is normal, and can be safely ignored.

9.  Remove unnecessary log files from the environment using the `-d` option on the <a href="../../api/c/db_archive.md" class="olink">db_archive utility</a>, or from an application which calls the <a href="../../api/c/logarchive.md" class="olink">DB_ENV-&gt;log_archive()</a> method with the <a href="../../api/c/logarchive.md#archive_DB_ARCH_REMOVE" class="olink">DB_ARCH_REMOVE</a> flag.

    Note that removing log files in this way might result in warning messages about log files that are being skipped. This is normal, and can be safely ignored.

    Note that if you are upgrading a replicated application, then you should <span class="emphasis">*not*</span> perform this step until all of the replication sites have been upgraded to the current release level. If you run this site before all your sites are upgraded, then errors can occur in your replication activities because important version information might be lost.

10. Restart the application.

Finally, Berkeley DB supports the live upgrade of a replication group, by allowing mixed version operation (replication sites running at the newer software version can inter-operate with older version sites). All client sites must be upgraded first; the master site must be upgraded last. In other words, at all times the master must be running the lowest version of Berkeley DB. To upgrade a replication group, you must:

1.  Bring all clients up to date with the master (that is, all clients must be brought up to the most current log record as measured by the master's log sequence number (LSN)).

2.  Perform the upgrade procedures described previously on each of the individual database environments that are part of the replication group. Each individual client may be upgraded and restarted to join the replication group.

3.  Shut down the master site and upgrade that site last.

During live replication upgrade, while sites are running at different versions, adding new (empty) clients to the replication group is not allowed. Those empty client environments must be added after the entire group is upgraded.

Also, all removal of log files must be suspended throughout this entire procedure, so that there is no chance of a client needing internal initialization.

Alternatively, it may be simpler to discard the contents of all of the client database environments, upgrade the master database environment, and then re-add all of the clients to the replication group using the standard replication procedures for new sites.
