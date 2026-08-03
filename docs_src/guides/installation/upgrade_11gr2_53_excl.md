---
title: "Exclusive Database Handles"
api-name: "Exclusive Database Handles"
source: docs/installation/upgrade_11gr2_53_excl.html
---
## Exclusive Database Handles

<span class="sect2"> [New Functions](upgrade_11gr2_53_excl.md#idp811424) </span>

Database handles can now be configured to allow exclusive access to the database. To enable exclusive access, call <a href="../../api/c/dbset_lk_exclusive.md" class="olink">DB-&gt;set_lk_exclusive()</a> before calling <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a>. Set nowait_onoff to non-zero to have <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> return immediately, with the error `DB_LOCK_NOTGRANTED` if it cannot immediately get exclusive access to the database, and to 0 to have <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> block until it can gain exclusive access.

### New Functions

- <a href="../../api/c/dbset_lk_exclusive.md" class="olink">DB-&gt;set_lk_exclusive()</a>
- <a href="../../api/c/dbget_lk_exclusive.md" class="olink">DB-&gt;get_lk_exclusive()</a>
