---
title: "db_xa_open"
api-name: "db_xa_open"
source: docs/upgrading/upgrade_3_0_xa.html
---
## db_xa_open

The following change applies only to applications using Berkeley DB as an XA Resource Manager. If your application is not using Berkeley DB in this way, you can ignore this change.

The db_xa_open function has been replaced with the `DB_XA_CREATE` flag to the <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> function. All calls to db_xa_open should be replaced with calls to <a href="../../api/c/dbcreate.md" class="olink">db_create()</a> with the `DB_XA_CREATE` flag set, followed by a call to the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> function.

A similar change has been made for the C++ API, where the `DB_XA_CREATE` flag should be specified to the Db constructor. All calls to the Db::xa_open method should be replaced with the `DB_XA_CREATE` flag to the Db constructor, followed by a call to the DB::open method.
