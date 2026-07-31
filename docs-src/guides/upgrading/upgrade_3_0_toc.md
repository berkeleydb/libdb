---
title: "Chapter 14. Upgrading Berkeley DB 2.X applications to Berkeley DB 3.0"
api-name: "Chapter 14. Upgrading Berkeley DB 2.X applications to Berkeley DB 3.0"
source: docs/upgrading/upgrade_3_0_toc.html
---
## Chapter 14. Upgrading Berkeley DB 2.X applications to Berkeley DB 3.0

**Table of Contents**

<span class="sect1"> [introduction](upgrade_3_0_toc.md#upgrade_3_0_intro) </span>

<span class="sect1"> [environment open/close/unlink](upgrade_3_0_envopen.md) </span>

<span class="sect1"> [function arguments](upgrade_3_0_func.md) </span>

<span class="sect1"> [DB_ENV structure](upgrade_3_0_dbenv.md) </span>

<span class="sect1"> [database open/close](upgrade_3_0_open.md) </span>

<span class="sect1"> [db_xa_open](upgrade_3_0_xa.md) </span>

<span class="sect1"> [DB structure](upgrade_3_0_db.md) </span>

<span class="sect1"> [DBINFO structure](upgrade_3_0_dbinfo.md) </span>

<span class="sect1"> [DB-\>join](upgrade_3_0_join.md) </span>

<span class="sect1"> [DB-\>stat](upgrade_3_0_stat.md) </span>

<span class="sect1"> [DB-\>sync and DB-\>close](upgrade_3_0_close.md) </span>

<span class="sect1"> [lock_put](upgrade_3_0_lock_put.md) </span>

<span class="sect1"> [lock_detect](upgrade_3_0_lock_detect.md) </span>

<span class="sect1"> [lock_stat](upgrade_3_0_lock_stat.md) </span>

<span class="sect1"> [log_register](upgrade_3_0_log_register.md) </span>

<span class="sect1"> [log_stat](upgrade_3_0_log_stat.md) </span>

<span class="sect1"> [memp_stat](upgrade_3_0_memp_stat.md) </span>

<span class="sect1"> [txn_begin](upgrade_3_0_txn_begin.md) </span>

<span class="sect1"> [txn_commit](upgrade_3_0_txn_commit.md) </span>

<span class="sect1"> [txn_stat](upgrade_3_0_txn_stat.md) </span>

<span class="sect1"> [DB_RMW](upgrade_3_0_rmw.md) </span>

<span class="sect1"> [DB_LOCK_NOTHELD](upgrade_3_0_lock_notheld.md) </span>

<span class="sect1"> [EAGAIN](upgrade_3_0_eagain.md) </span>

<span class="sect1"> [EACCES](upgrade_3_0_eacces.md) </span>

<span class="sect1"> [db_jump_set](upgrade_3_0_jump_set.md) </span>

<span class="sect1"> [db_value_set](upgrade_3_0_value_set.md) </span>

<span class="sect1"> [DbEnv class for C++ and Java](upgrade_3_0_dbenv_cxx.md) </span>

<span class="sect1"> [Db class for C++ and Java](upgrade_3_0_db_cxx.md) </span>

<span class="sect1"> [additional C++ changes](upgrade_3_0_cxx.md) </span>

<span class="sect1"> [additional Java changes](upgrade_3_0_java.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_3_0_disk.md) </span>

## introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 2.X release interfaces to the Berkeley DB 3.0 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
