---
title: "Chapter 13. Upgrading Berkeley DB 4.7 applications to Berkeley DB 4.8"
api-name: "Chapter 13. Upgrading Berkeley DB 4.7 applications to Berkeley DB 4.8"
source: docs/installation/upgrade_4_8_toc.html
---
## Chapter 13. Upgrading Berkeley DB 4.7 applications to Berkeley DB 4.8

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_4_8_toc.md#upgrade_4_8_intro) </span>

<span class="sect1"> [Registering DPL Secondary Keys](upgrade_4_8_dpl.md) </span>

<span class="sect1"> [Minor Change in Behavior of DB_MPOOLFILE-\>get](upgrade_4_8_mpool.md) </span>

<span class="sect1"> [Dropped Support for fcntl System Calls](upgrade_4_8_fcntl.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_8_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.8.28 Change Log](changelog_4_8.md) </span>

<span class="sect2"> [Changes between 4.8.26 and 4.8.28:](changelog_4_8.md#idp1162104) </span>

<span class="sect2"> [Known bugs in 4.8](changelog_4_8.md#idp1184264) </span>

<span class="sect2"> [Changes between 4.8.24 and 4.8.26:](changelog_4_8.md#idp1139288) </span>

<span class="sect2"> [Changes between 4.8.21 and 4.8.24:](changelog_4_8.md#idp1091200) </span>

<span class="sect2"> [Changes between 4.7 and 4.8.21:](changelog_4_8.md#idp1199520) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_8.md#idp1200208) </span>

<span class="sect2"> [New Features:](changelog_4_8.md#idp981712) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_8.md#idp1130224) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_8.md#idp1209320) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_8.md#idp1209720) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_8.md#idp1218064) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_8.md#idp1215560) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_8.md#idp1226120) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_8.md#idp1163928) </span>

<span class="sect2"> [C-specific API Changes:](changelog_4_8.md#idp1138904) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_8.md#idp1218344) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_8.md#idp1238856) </span>

<span class="sect2"> [Direct Persistence Layer (DPL), Bindings and Collections API:](changelog_4_8.md#idp1232112) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_8.md#idp1232384) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_8.md#idp1244368) </span>

<span class="sect2"> [Replication Changes:](changelog_4_8.md#idp1245896) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_8.md#idp1242240) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_8.md#idp1247728) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_8.md#idp1241128) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_8.md#idp1258328) </span>

<span class="sect2"> [Mutex Subsystem Changes:](changelog_4_8.md#idp1258720) </span>

<span class="sect2"> [Test Suite Changes](changelog_4_8.md#idp1240832) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_8.md#idp1249776) </span>

<span class="sect2"> [Utility Changes:](changelog_4_8.md#idp1271664) </span>

<span class="sect2"> [Configuration, Documentation, Sample Application, Portability and Build Changes:](changelog_4_8.md#idp1274104) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 4.7 release interfaces to the Berkeley DB 4.8 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
