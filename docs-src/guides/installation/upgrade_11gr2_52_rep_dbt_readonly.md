---
title: "Read Only flag for DBT"
api-name: "Read Only flag for DBT"
source: docs/installation/upgrade_11gr2_52_rep_dbt_readonly.html
---
## Read Only flag for DBT

<span class="sect2"> [New Flag](upgrade_11gr2_52_rep_dbt_readonly.md#idp907000) </span>

A <a href="../../api/c/dbt.md" class="olink">DBT</a> can now be set as read-only, when passed to the <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> method, using the flag `DB_DBT_READONLY`. This is useful when using a static string as a key value, because this flag will prevent Berkeley DB from updating the <a href="../../api/c/dbt.md" class="olink">DBT</a>.

### New Flag

- `DB_DBT_READONLY`
