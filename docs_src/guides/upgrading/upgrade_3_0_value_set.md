---
title: "db_value_set"
api-name: "db_value_set"
source: docs/upgrading/upgrade_3_0_value_set.html
---
## db_value_set

The db_value_set function has been removed from the Berkeley DB 3.0 release, replaced by method calls on the <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle.

The following table lists the db_value_set arguments previously used by applications and the function that should now be used instead.

| db_value_set argument | Berkeley DB 3.X method |
|----|----|
| DB_MUTEX_LOCKS | dbenv_set_mutexlocks |
| DB_REGION_ANON | The DB_REGION_ANON functionality has been replaced by the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> and <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flags to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> function. A direct translation is not available, please review the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> manual page for more information. |
| DB_REGION_INIT | dbenv_set_region_init |
| DB_REGION_NAME | The DB_REGION_NAME functionality has been replaced by the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> and <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flags to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> function. A direct translation is not available, please review the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> manual page for more information. |
| DB_TSL_SPINS | dbenv_set_tas_spins |
