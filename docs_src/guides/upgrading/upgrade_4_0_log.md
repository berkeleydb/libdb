---
title: "log_XXX"
api-name: "log_XXX"
source: docs/upgrading/upgrade_4_0_log.html
---
## log_XXX

The C API for the Berkeley DB Logging subsystem was reworked in the 4.0 release as follows:

| Historic functional interface | Berkeley DB 4.X method |
|----|----|
| log_archive | <a href="../../api/c/logarchive.md" class="olink">DB_ENV-&gt;log_archive()</a> |
| log_file | <a href="../../api/c/logfile.md" class="olink">DB_ENV-&gt;log_file()</a> |
| log_flush | <a href="../../api/c/logflush.md" class="olink">DB_ENV-&gt;log_flush()</a> |
| log_get | <a href="../../api/c/logcursor.md" class="olink">DB_ENV-&gt;log_cursor()</a> |
| log_put | <a href="../../api/c/logput.md" class="olink">DB_ENV-&gt;log_put()</a> |
| log_register | DB_ENV-\>log_register |
| log_stat | <a href="../../api/c/logstat.md" class="olink">DB_ENV-&gt;log_stat()</a> |
| log_unregister | DB_ENV-\>log_unregister |

Applications calling any of these functions should update their calls to use the enclosing <a href="../../api/c/env.md" class="olink">DB_ENV class</a> handle's method (in all cases other than the log_get call, this is easily done as the first argument to the existing call is the correct handle to use).

Application calls to the historic log_get function must be replaced with the creation of a log file cursor (a <a href="../../api/c/logc.md" class="olink">DB_LOGC class</a> object), using the <a href="../../api/c/logcursor.md" class="olink">DB_ENV-&gt;log_cursor()</a> method to retrieve log records and calls to the <a href="../../api/c/logcclose.md" class="olink">DB_LOGC-&gt;close()</a> method to destroy the cursor. It may also be possible to simplify some applications. In previous releases of Berkeley DB, the DB_CURRENT, DB_NEXT, and DB_PREV flags to the log_get function could not be used by a free-threaded <a href="../../api/c/env.md" class="olink">DB_ENV class</a> handle. If their <a href="../../api/c/env.md" class="olink">DB_ENV class</a> handle was free-threaded, applications had to create an additional, unique environment handle by separately calling <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>. This is no longer an issue in the log cursor interface, and applications may be able to remove the now unnecessary creation of the additional <a href="../../api/c/env.md" class="olink">DB_ENV class</a> object.

Finally, the <a href="../../api/c/logstat.md" class="olink">DB_ENV-&gt;log_stat()</a> call has been changed in the 4.0 release to take a flags argument. To leave their historic behavior unchanged, applications should add a final argument of 0 to any calls made to <a href="../../api/c/logstat.md" class="olink">DB_ENV-&gt;log_stat()</a>.
