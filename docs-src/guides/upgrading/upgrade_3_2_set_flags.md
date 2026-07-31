---
title: "DB_ENV->set_flags"
api-name: "DB_ENV->set_flags"
source: docs/upgrading/upgrade_3_2_set_flags.html
---
## DB_ENV-\>set_flags

A new method has been added to the Berkeley DB environment handle, <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a>. This method currently takes three flags: <a href="../../api/c/envset_flags.md#set_flags_DB_CDB_ALLDB" class="olink">DB_CDB_ALLDB</a>, <a href="../../api/c/dbopen.md#open_DB_NOMMAP" class="olink">DB_NOMMAP</a>, and <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a>. The first of these flags, <a href="../../api/c/envset_flags.md#set_flags_DB_CDB_ALLDB" class="olink">DB_CDB_ALLDB</a>, provides new functionality, allowing Berkeley DB Concurrent Data Store applications to do locking across multiple databases.

The other two flags, <a href="../../api/c/dbopen.md#open_DB_NOMMAP" class="olink">DB_NOMMAP</a> and <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a>, were specified to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method in previous releases. In the 3.2 release, they have been moved to the <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> method because this allows the database environment's value to be toggled during the life of the application as well as because it is a more appropriate place for them. Applications specifying either the <a href="../../api/c/dbopen.md#open_DB_NOMMAP" class="olink">DB_NOMMAP</a> or <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a> flags to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method should replace those flags with calls to the <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> method.
