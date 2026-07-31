---
title: "Chapter 3. Upgrading Berkeley DB 4.6 applications to Berkeley DB 4.7"
api-name: "Chapter 3. Upgrading Berkeley DB 4.6 applications to Berkeley DB 4.7"
source: docs/upgrading/upgrade_4_7_toc.html
---
## Chapter 3. Upgrading Berkeley DB 4.6 applications to Berkeley DB 4.7

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_4_7_toc.md#upgrade_4_7_intro) </span>

<span class="sect1"> [Run-time configuration](upgrade_4_7_rtc.md) </span>

<span class="sect1"> [Replication API](upgrade_4_7_repapi.md) </span>

<span class="sect1"> [Tcl API](upgrade_4_7_tcl.md) </span>

<span class="sect1"> [DB_ENV-\>set_intermediate_dir](upgrade_4_7_interdir.md) </span>

<span class="sect1"> [Log configuration](upgrade_4_7_log.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_7_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.7.25 Change Log](changelog_4_7.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_7.md#idp50357648) </span>

<span class="sect2"> [New Features:](changelog_4_7.md#idp50378912) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_7.md#idp50380752) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_7.md#idp50382592) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_7.md#idp50381120) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_7.md#idp50391248) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_7.md#idp50365280) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_7.md#idp50355136) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_7.md#idp50346288) </span>

<span class="sect2"> [C-specific API Changes:](changelog_4_7.md#idp50346816) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_7.md#idp50347072) </span>

<span class="sect2"> [Direct Persistence Layer (DPL), Bindings and Collections API:](changelog_4_7.md#idp50347296) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_7.md#idp50386200) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_7.md#idp50395072) </span>

<span class="sect2"> [Replication Changes:](changelog_4_7.md#idp50395328) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_7.md#idp50391504) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_7.md#idp50357904) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_7.md#idp50397528) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_7.md#idp50385512) </span>

<span class="sect2"> [Mutex Subsystem Changes:](changelog_4_7.md#idp50386616) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_7.md#idp50391632) </span>

<span class="sect2"> [Utility Changes:](changelog_4_7.md#idp50396200) </span>

<span class="sect2"> [Configuration, Documentation, Sample Application, Portability and Build Changes:](changelog_4_7.md#idp50412288) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 4.6 release interfaces to the Berkeley DB 4.7 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
