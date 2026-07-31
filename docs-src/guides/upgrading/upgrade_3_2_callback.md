---
title: "DB callback functions, app_private field"
api-name: "DB callback functions, app_private field"
source: docs/upgrading/upgrade_3_2_callback.html
---
## DB callback functions, app_private field

In the Berkeley DB 3.2 release, four application callback functions (the callback functions set by <a href="../../api/c/dbset_bt_compare.md" class="olink">DB-&gt;set_bt_compare()</a>, <a href="../../api/c/dbset_bt_prefix.md" class="olink">DB-&gt;set_bt_prefix()</a>, <a href="../../api/c/dbset_dup_compare.md" class="olink">DB-&gt;set_dup_compare()</a> and <a href="../../api/c/dbset_h_hash.md" class="olink">DB-&gt;set_h_hash()</a>) were modified to take a reference to a <a href="../../api/c/db.md" class="olink">DB</a> object as their first argument. This change allows the Berkeley DB Java API to reasonably support these interfaces. There is currently no need for the callback functions to do anything with this additional argument.

C and C++ applications that specify their own Btree key comparison, Btree prefix comparison, duplicate data item comparison or Hash functions should modify these functions to take a reference to a <a href="../../api/c/db.md" class="olink">DB</a> structure as their first argument. No further change is required.

The app_private field of the <a href="../../api/c/dbt.md" class="olink">DBT</a> structure (accessible only from the Berkeley DB C API) has been removed in the 3.2 release. It was replaced with app_private fields in the <a href="../../api/c/env.md" class="olink">DB_ENV</a> handles. Applications using this field will have to convert to using one of the replacement fields.
