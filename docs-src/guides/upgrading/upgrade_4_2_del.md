---
title: "DB->del"
api-name: "DB->del"
source: docs/upgrading/upgrade_4_2_del.html
---
## DB-\>del

In previous releases, the C++ <a href="../api_reference/CXX/dbdel.html" class="olink">Db::del</a> and Java `Db.delete()` methods threw exceptions encapsulating the <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> error in some cases when called on Queue and Recno databases. Unfortunately, this was undocumented behavior.

For consistency with the other Berkeley DB methods that handle <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a>, this is no longer the case. Applications calling the <a href="../api_reference/CXX/dbdel.html" class="olink">Db::del</a> and Java `Db.delete()` methods on Queue or Recno databases, and handling the <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> exception specially, should be modified to check for a return value of <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> instead.
