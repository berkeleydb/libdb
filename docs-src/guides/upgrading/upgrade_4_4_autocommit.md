---
title: "DB_AUTO_COMMIT"
api-name: "DB_AUTO_COMMIT"
source: docs/upgrading/upgrade_4_4_autocommit.html
---
## DB_AUTO_COMMIT

In previous Berkeley DB releases, the <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> flag was used in the C and C++ Berkeley DB APIs to wrap operations within a transaction without explicitly creating a transaction and passing the <a href="../../api/c/txn.md" class="olink">TXN</a> handle as part of the operation method call. In the 4.4 release, the <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> flag no longer needs to be explicitly specified.

In the 4.4 release, specifying the <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> flag to the <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> method causes all database modifications in that environment to be transactional; specifying <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> to the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> method causes all modifications to that database to be transactional; specifying <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> to the <a href="../../api/c/envdbremove.md" class="olink">DB_ENV-&gt;dbremove()</a> methods causes those specific operations to be transactional.

No related application changes are required for this release, as the <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> flag is ignored where it is no longer needed. However, application writers are encouraged to remove uses of the <a href="../../api/c/envset_flags.md#envset_flags_DB_AUTO_COMMIT" class="olink">DB_AUTO_COMMIT</a> flag in places where it is no longer needed.

Similar changes have been made to the Berkeley DB Tcl API. These changes are not optional, and Tcl applications will need to remove the -auto_commit flag from methods where it is no longer needed.
