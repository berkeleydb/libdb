---
title: "Retrieved key/data permanence for C/C++"
api-name: "Retrieved key/data permanence for C/C++"
source: docs/programmer_reference/am_misc_perm.html
---
## Retrieved key/data permanence for C/C++

When using the non-cursor Berkeley DB calls to retrieve key/data items under the C/C++ APIs (for example, <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a>), the memory to which the pointer stored into the <a href="../../api/c/dbt.md" class="olink">DBT</a> refers is only valid until the next call to Berkeley DB using the <a href="../../api/c/db.md" class="olink">DB</a> handle. (This includes **any** use of the returned <a href="../../api/c/db.md" class="olink">DB</a> handle, including by another thread of control within the process. For this reason, when multiple threads are using the returned <a href="../../api/c/db.md" class="olink">DB</a> handle concurrently, one of the <a href="../../api/c/dbt.md#dbt_DB_DBT_MALLOC" class="olink">DB_DBT_MALLOC</a>, <a href="../../api/c/dbt.md#dbt_DB_DBT_REALLOC" class="olink">DB_DBT_REALLOC</a> or <a href="../../api/c/dbt.md#dbt_DB_DBT_USERMEM" class="olink">DB_DBT_USERMEM</a> flags must be specified with any non-cursor <a href="../../api/c/dbt.md" class="olink">DBT</a> used for key or data retrieval.)

When using the cursor Berkeley DB calls to retrieve key/data items under the C/C++ APIs (for example, <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a>), the memory to which the pointer stored into the <a href="../../api/c/dbt.md" class="olink">DBT</a> refers is only valid until the next call to Berkeley DB using the <a href="../../api/c/dbc.md" class="olink">DBC</a> returned by <a href="../../api/c/dbcursor.md" class="olink">DB-&gt;cursor()</a>.
