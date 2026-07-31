---
title: "Chapter 10. Upgrading Berkeley DB 3.3 applications to Berkeley DB 4.0"
api-name: "Chapter 10. Upgrading Berkeley DB 3.3 applications to Berkeley DB 4.0"
source: docs/upgrading/upgrade_4_0_toc.html
---
## Chapter 10. Upgrading Berkeley DB 3.3 applications to Berkeley DB 4.0

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_4_0_toc.md#upgrade_4_0_intro) </span>

<span class="sect1"> [db_deadlock](upgrade_4_0_deadlock.md) </span>

<span class="sect1"> [lock_XXX](upgrade_4_0_lock.md) </span>

<span class="sect1"> [log_XXX](upgrade_4_0_log.md) </span>

<span class="sect1"> [memp_XXX](upgrade_4_0_mp.md) </span>

<span class="sect1"> [txn_XXX](upgrade_4_0_txn.md) </span>

<span class="sect1"> [db_env_set_XXX](upgrade_4_0_env.md) </span>

<span class="sect1"> [DB_ENV-\>set_server](upgrade_4_0_rpc.md) </span>

<span class="sect1"> [DB_ENV-\>set_lk_max](upgrade_4_0_set_lk_max.md) </span>

<span class="sect1"> [DB_ENV-\>lock_id_free](upgrade_4_0_lock_id_free.md) </span>

<span class="sect1"> [Java CLASSPATH environment variable](upgrade_4_0_java.md) </span>

<span class="sect1"> [C++ ostream objects](upgrade_4_0_cxx.md) </span>

<span class="sect1"> [application-specific recovery](upgrade_4_0_asr.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_0_disk.md) </span>

<span class="sect1"> [4.0.14 Change Log](changelog_4_0_14.md) </span>

<span class="sect2"> [Major New Features:](changelog_4_0_14.md#idp51113768) </span>

<span class="sect2"> [General Environment Changes:](changelog_4_0_14.md#idp51101344) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_0_14.md#idp51103296) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_0_14.md#idp51105152) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_0_14.md#idp51109416) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_0_14.md#idp51112664) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_0_14.md#idp51113832) </span>

<span class="sect2"> [C++ API Changes:](changelog_4_0_14.md#idp51115760) </span>

<span class="sect2"> [Java API Changes:](changelog_4_0_14.md#idp51126328) </span>

<span class="sect2"> [Tcl API Changes:](changelog_4_0_14.md#idp51116840) </span>

<span class="sect2"> [RPC Client/Server Changes:](changelog_4_0_14.md#idp51117920) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_0_14.md#idp51118608) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_0_14.md#idp51118928) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_0_14.md#idp51103680) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_0_14.md#idp51122816) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_0_14.md#idp51109800) </span>

<span class="sect2"> [Utility Changes:](changelog_4_0_14.md#idp51113048) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_0_14.md#idp51125248) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_0_14.md#idp51126712) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 3.3 release interfaces to the Berkeley DB 4.0 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
