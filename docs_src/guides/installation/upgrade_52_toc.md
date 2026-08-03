---
title: "Chapter 10.  Upgrading Berkeley DB 11.2.5.1 applications to Berkeley DB 11.2.5.2"
api-name: "Chapter 10.  Upgrading Berkeley DB 11.2.5.1 applications to Berkeley DB 11.2.5.2"
source: docs/installation/upgrade_52_toc.html
---
## Chapter 10.  Upgrading Berkeley DB 11.2.5.1 applications to Berkeley DB 11.2.5.2

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_52_toc.md#upgrade_52_intro) </span>

<span class="sect1"> [SQLite Interface Upgrade](upgrade_11gr2_52_sqlite_ver.md) </span>

<span class="sect1"> [32bit/64bit Compatibility on Windows](upgrade_11gr2_52_bit_cmp_win.md) </span>

<span class="sect1"> [Read Only flag for DBT](upgrade_11gr2_52_rep_dbt_readonly.md) </span>

<span class="sect2"> [New Flag](upgrade_11gr2_52_rep_dbt_readonly.md#idp907000) </span>

<span class="sect1"> [Dynamic Environment Configuration](upgrade_11gr2_52_dyn_env.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_dyn_env.md#idp902144) </span>

<span class="sect2"> [Deprecated Functions](upgrade_11gr2_52_dyn_env.md#idp912000) </span>

<span class="sect1"> [Exclusive Transactions in the SQL Layer](upgrade_11gr2_52_excl_txn_sql.md) </span>

<span class="sect1"> [Group Membership in Repmgr](upgrade_11gr2_52_grp_mbr.md) </span>

<span class="sect2"> [Upgrading](upgrade_11gr2_52_grp_mbr.md#idp929720) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_grp_mbr.md#idp910056) </span>

<span class="sect2"> [Modified Functions](upgrade_11gr2_52_grp_mbr.md#idp901088) </span>

<span class="sect2"> [New Events](upgrade_11gr2_52_grp_mbr.md#idp924520) </span>

<span class="sect2"> [Removed Functions](upgrade_11gr2_52_grp_mbr.md#idp937928) </span>

<span class="sect2"> [New Parameters](upgrade_11gr2_52_grp_mbr.md#idp909344) </span>

<span class="sect2"> [New Structure](upgrade_11gr2_52_grp_mbr.md#idp924776) </span>

<span class="sect1"> [Heap Access Method](upgrade_11gr2_52_heap.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_heap.md#idp936848) </span>

<span class="sect2"> [Modified Functions](upgrade_11gr2_52_heap.md#idp930424) </span>

<span class="sect2"> [New Definition](upgrade_11gr2_52_heap.md#idp931776) </span>

<span class="sect1"> [Enabling Transaction Snapshots in the SQL Layer](upgrade_11gr2_52_mvcc_sql.md) </span>

<span class="sect2"> [New Pragmas](upgrade_11gr2_52_mvcc_sql.md#idp951464) </span>

<span class="sect1"> [2SITE_STRICT Enabled by Default in Replication](upgrade_11gr2_52_rep_2site_strict.md) </span>

<span class="sect1"> [Enabling Replication in the SQL Layer](upgrade_11gr2_52_rep_sql.md) </span>

<span class="sect2"> [New Pragmas](upgrade_11gr2_52_rep_sql.md#idp962696) </span>

<span class="sect1"> [Repmgr Message Channels](upgrade_11gr2_52_repmgr_channels.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_repmgr_channels.md#idp919280) </span>

<span class="sect1"> [Sequence Support in the SQL Layer](upgrade_11gr2_52_seq_sql.md) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_seq_sql.md#idp963480) </span>

<span class="sect1"> [Berkeley DB X/Open Compliant XA Resource Manager](upgrade_11gr2_52_xa.md) </span>

<span class="sect2"> [Constraints](upgrade_11gr2_52_xa.md#idp973264) </span>

<span class="sect2"> [New Flag](upgrade_11gr2_52_xa.md#idp978200) </span>

<span class="sect2"> [Modified Function](upgrade_11gr2_52_xa.md#idp982256) </span>

<span class="sect1"> [Hot Backup Changes](upgrade_11gr2_52_hot_backup.md) </span>

<span class="sect1"> [Berkeley DB Library Version 11.2.5.2 Change Log](changelog_5_2.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes](changelog_5_2.md#idp972456) </span>

<span class="sect2"> [New Features](changelog_5_2.md#idp978720) </span>

<span class="sect2"> [Database Environment Changes](changelog_5_2.md#idp984720) </span>

<span class="sect2"> [Concurrent Data Store Changes](changelog_5_2.md#idp995752) </span>

<span class="sect2"> [Access Method Changes](changelog_5_2.md#idp989160) </span>

<span class="sect2"> [SQL API Changes](changelog_5_2.md#idp989544) </span>

<span class="sect2"> [C API Changes](changelog_5_2.md#idp971912) </span>

<span class="sect2"> [Tcl-specific API Changes](changelog_5_2.md#idp996528) </span>

<span class="sect2"> [C#-specific API Changes](changelog_5_2.md#idp972000) </span>

<span class="sect2"> [Replication Changes](changelog_5_2.md#idp994456) </span>

<span class="sect2"> [Locking Subsystem Changes](changelog_5_2.md#idp996912) </span>

<span class="sect2"> [Logging Subsystem Changes](changelog_5_2.md#idp1010640) </span>

<span class="sect2"> [Memory Pool Subsystem Changes](changelog_5_2.md#idp992728) </span>

<span class="sect2"> [Mutex Subsystem Changes](changelog_5_2.md#idp1018872) </span>

<span class="sect2"> [Transaction Subsystem Changes](changelog_5_2.md#idp1011056) </span>

<span class="sect2"> [Test Suite Changes](changelog_5_2.md#idp1003424) </span>

<span class="sect2"> [Utility Changes](changelog_5_2.md#idp1029752) </span>

<span class="sect2"> [Configuration, Documentation, Sample Apps, Portability and Build Changes](changelog_5_2.md#idp1031368) </span>

<span class="sect2"> [Example Changes](changelog_5_2.md#idp1003200) </span>

<span class="sect2"> [Miscellaneous Bug Fixes](changelog_5_2.md#idp1034280) </span>

<span class="sect2"> [Deprecated Features](changelog_5_2.md#idp1035816) </span>

<span class="sect2"> [Known Bugs](changelog_5_2.md#idp1037736) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 11.2.5.1 library to the Berkeley DB 11.2.5.2 library (both of which belong to Berkeley DB 11<span class="emphasis">*g*</span> Release 2). This information does not describe how to upgrade Berkeley DB 1.85 release applications.

For information on the general process of upgrading Berkeley DB installations and upgrade instructions related to historical releases, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/upgrading/index.html" class="ulink" target="_top">Berkeley DB Upgrade Guide.</a>
