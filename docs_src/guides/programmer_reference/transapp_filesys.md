---
title: "Recovery and filesystem operations"
api-name: "Recovery and filesystem operations"
source: docs/programmer_reference/transapp_filesys.html
---
## Recovery and filesystem operations

The Berkeley DB API supports creating, removing and renaming files. Creating files is supported by the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> method. Removing files is supported by the <a href="../../api/c/envdbremove.md" class="olink">DB_ENV-&gt;dbremove()</a> and <a href="../../api/c/dbremove.md" class="olink">DB-&gt;remove()</a> methods. Renaming files is supported by the <a href="../../api/c/envdbrename.md" class="olink">DB_ENV-&gt;dbrename()</a> and <a href="../../api/c/dbrename.md" class="olink">DB-&gt;rename()</a> methods. (There are two methods for removing and renaming files because one of the methods is transactionally protected and one is not.)

Berkeley DB does not permit specifying the <a href="../../api/c/dbopen.md#open_DB_TRUNCATE" class="olink">DB_TRUNCATE</a> flag when opening a file in a transaction-protected environment. This is an implicit file deletion, but one that does not always require the same operating system file permissions as deleting and creating a file do.

If you have changed the name of a file or deleted it outside of the Berkeley DB library (for example, you explicitly removed a file using your normal operating system utilities), then it is possible that recovery will not be able to find a database to which the log refers. In this case, the <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility will produce a warning message, saying it was unable to locate a file it expected to find. This message is only a warning because the file may have been subsequently deleted as part of normal database operations before the failure occurred, so is not necessarily a problem.

Generally, any filesystem operations that are performed outside the Berkeley DB interface should be performed at the same time as making a snapshot of the database. To perform filesystem operations correctly, do the following:

1.  Cleanly shut down database operations.

    To shut down database operations cleanly, all applications accessing the database environment must be shut down and a transaction checkpoint must be taken. If the applications are not implemented so they can be shut down gracefully (that is, closing all references to the database environment), recovery must be performed after all applications have been killed to ensure that the underlying databases are consistent on disk.

2.  Perform the filesystem operations; for example, remove or rename one or more files.

3.  Make an archival snapshot of the database.

    Although this step is not strictly necessary, it is strongly recommended. If this step is not performed, recovery from catastrophic failure will require that recovery first be performed up to the time of the filesystem operations, the filesystem operations be redone, and then recovery be performed from the filesystem operations forward.

4.  Restart the database applications.
