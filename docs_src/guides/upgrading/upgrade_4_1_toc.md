---
title: "Chapter 9. Upgrading Berkeley DB 4.0 applications to Berkeley DB 4.1"
api-name: "Chapter 9. Upgrading Berkeley DB 4.0 applications to Berkeley DB 4.1"
source: docs/upgrading/upgrade_4_1_toc.html
---
## Chapter 9. Upgrading Berkeley DB 4.0 applications to Berkeley DB 4.1

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_4_1_toc.md#upgrade_4_1_intro) </span>

<span class="sect1"> [DB_EXCL](upgrade_4_1_excl.md) </span>

<span class="sect1"> [DB-\>associate, DB-\>open, DB-\>remove, DB-\>rename](upgrade_4_1_fop.md) </span>

<span class="sect1"> [DB_ENV-\>log_register](upgrade_4_1_log_register.md) </span>

<span class="sect1"> [st_flushcommit](upgrade_4_1_log_stat.md) </span>

<span class="sect1"> [DB_CHECKPOINT, DB_CURLSN](upgrade_4_1_checkpoint.md) </span>

<span class="sect1"> [DB_INCOMPLETE](upgrade_4_1_incomplete.md) </span>

<span class="sect1"> [DB_ENV-\>memp_sync](upgrade_4_1_memp_sync.md) </span>

<span class="sect1"> [DB-\>stat.hash_nelem](upgrade_4_1_hash_nelem.md) </span>

<span class="sect1"> [Java exceptions](upgrade_4_1_java.md) </span>

<span class="sect1"> [C++ exceptions](upgrade_4_1_cxx.md) </span>

<span class="sect1"> [Application-specific logging and recovery](upgrade_4_1_app_dispatch.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_1_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.1.24 and 4.1.25 Change Log](changelog_4_1_24.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_1_24.md#idp50963888) </span>

<span class="sect2"> [Major New Features:](changelog_4_1_24.md#idp50959088) </span>

<span class="sect2"> [General Environment Changes:](changelog_4_1_24.md#idp50962280) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_1_24.md#idp50961984) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_1_24.md#idp50964272) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_1_24.md#idp50967400) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_1_24.md#idp50969240) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_1_24.md#idp50972088) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_1_24.md#idp50973928) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_1_24.md#idp50975768) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_1_24.md#idp50950328) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_1_24.md#idp50958680) </span>

<span class="sect2"> [Replication Changes:](changelog_4_1_24.md#idp50977144) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_1_24.md#idp50964336) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_1_24.md#idp50987264) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_1_24.md#idp50989192) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_1_24.md#idp50992072) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_1_24.md#idp50993160) </span>

<span class="sect2"> [Utility Changes:](changelog_4_1_24.md#idp50994744) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_1_24.md#idp50997648) </span>

<span class="sect1"> [Berkeley DB 4.1.25 Change Log](changelog_4_1_25.md) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 4.0 release interfaces to the Berkeley DB 4.1 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
