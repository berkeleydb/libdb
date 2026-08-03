---
title: "DB_EXCL"
api-name: "DB_EXCL"
source: docs/upgrading/upgrade_4_1_excl.html
---
## DB_EXCL

The <a href="../../api/c/dbopen.md#open_DB_EXCL" class="olink">DB_EXCL</a> flag to the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> method now works for subdatabases as well as physical files, and it is now possible to use the <a href="../../api/c/dbopen.md#open_DB_EXCL" class="olink">DB_EXCL</a> flag to check for the previous existence of subdatabases.
