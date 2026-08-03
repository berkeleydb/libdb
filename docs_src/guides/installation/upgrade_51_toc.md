---
title: "Chapter 11.  Upgrading Berkeley DB 11.2.5.0 applications to Berkeley DB 11.2.5.1"
api-name: "Chapter 11.  Upgrading Berkeley DB 11.2.5.0 applications to Berkeley DB 11.2.5.1"
source: docs/installation/upgrade_51_toc.html
---
## Chapter 11.  Upgrading Berkeley DB 11.2.5.0 applications to Berkeley DB 11.2.5.1

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_51_toc.md#upgrade_51_intro) </span>

<span class="sect1"> [DPL Applications must be recompiled](upgrade_11gr2_51_dpl_recompile.md) </span>

<span class="sect1"> [Source Tree Rearranged](upgrade_11gr2_51_src_reorg.md) </span>

<span class="sect1"> [SQLite Interface Upgrade](upgrade_11gr2_51_sqlite_ver.md) </span>

<span class="sect1"> [Mod_db4 Support Discontinued](upgrade_11gr2_51_mod_db4_unsupp.md) </span>

<span class="sect1"> [Berkeley DB Library Version 11.2.5.1 Change Log](changelog_5_1.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes](changelog_5_1.md#idp1052992) </span>

<span class="sect2"> [New Features](changelog_5_1.md#idp953176) </span>

<span class="sect2"> [Database Environment Changes](changelog_5_1.md#idp1045336) </span>

<span class="sect2"> [Concurrent Data Store Changes](changelog_5_1.md#idp1059760) </span>

<span class="sect2"> [Access Method Changes](changelog_5_1.md#idp981016) </span>

<span class="sect2"> [API Changes](changelog_5_1.md#idp1049008) </span>

<span class="sect2"> [SQL-Specific API Changes](changelog_5_1.md#idp1055592) </span>

<span class="sect2"> [Tcl-Specific API Changes](changelog_5_1.md#idp1056952) </span>

<span class="sect2"> [Java-Specific API Changes](changelog_5_1.md#idp1052280) </span>

<span class="sect2"> [C#-Specific API Changes](changelog_5_1.md#idp987592) </span>

<span class="sect2"> [Direct Persistence Layer (DPL), Bindings and Collections API](changelog_5_1.md#idp1060648) </span>

<span class="sect2"> [Replication Changes](changelog_5_1.md#idp1070000) </span>

<span class="sect2"> [Locking Subsystem Changes](changelog_5_1.md#idp1080936) </span>

<span class="sect2"> [Logging Subsystem Changes](changelog_5_1.md#idp1092608) </span>

<span class="sect2"> [Memory Pool Subsystem Changes](changelog_5_1.md#idp1076376) </span>

<span class="sect2"> [Mutex Subsystem Changes](changelog_5_1.md#idp1080752) </span>

<span class="sect2"> [Transaction Subsystem Changes](changelog_5_1.md#idp1089584) </span>

<span class="sect2"> [Test Suite Changes](changelog_5_1.md#idp1067160) </span>

<span class="sect2"> [Utility Changes](changelog_5_1.md#idp1088000) </span>

<span class="sect2"> [Configuration, Documentation, Sample Apps, Portability, and Build Changes](changelog_5_1.md#idp1091312) </span>

<span class="sect2"> [Example Changes](changelog_5_1.md#idp1081576) </span>

<span class="sect2"> [Miscellaneous Bug Fixes](changelog_5_1.md#idp1102152) </span>

<span class="sect2"> [Deprecated Features](changelog_5_1.md#idp1100024) </span>

<span class="sect2"> [Known Bugs](changelog_5_1.md#idp1100672) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 11.2.5.0 library to the Berkeley DB 11.2.5.1 library (both of which belong to Berkeley DB 11<span class="emphasis">*g*</span> Release 2). This information does not describe how to upgrade Berkeley DB 1.85 release applications.

For information on the general process of upgrading Berkeley DB installations and upgrade instructions related to historical releases, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/upgrading/index.html" class="ulink" target="_top">Berkeley DB Upgrade Guide.</a>
