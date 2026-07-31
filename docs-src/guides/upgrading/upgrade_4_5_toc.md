---
title: "Chapter 5. Upgrading Berkeley DB 4.4 applications to Berkeley DB 4.5"
api-name: "Chapter 5. Upgrading Berkeley DB 4.4 applications to Berkeley DB 4.5"
source: docs/upgrading/upgrade_4_5_toc.html
---
## Chapter 5. Upgrading Berkeley DB 4.4 applications to Berkeley DB 4.5

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_4_5_toc.md#upgrade_4_5_intro) </span>

<span class="sect1"> [deprecated interfaces](upgrade_4_5_deprecate.md) </span>

<span class="sect1"> [DB-\>set_isalive](upgrade_4_5_alive.md) </span>

<span class="sect1"> [DB_ENV-\>rep_elect](upgrade_4_5_elect.md) </span>

<span class="sect1"> [Replication method naming](upgrade_4_5_rep_set.md) </span>

<span class="sect1"> [Replication events](upgrade_4_5_rep_event.md) </span>

<span class="sect1"> [Memory Pool API](upgrade_4_5_memp.md) </span>

<span class="sect1"> [DB_ENV-\>set_paniccall](upgrade_4_5_paniccall.md) </span>

<span class="sect1"> [DB-\>set_pagesize](upgrade_4_5_pagesize.md) </span>

<span class="sect1"> [Collections API](upgrade_4_5_collect.md) </span>

<span class="sect1"> [--enable-pthread_self](upgrade_4_5_config.md) </span>

<span class="sect1"> [Recno backing text source files](upgrade_4_5_source.md) </span>

<span class="sect1"> [Application-specific logging](upgrade_4_5_applog.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_5_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.5.20 Change Log](changelog_4_5_20.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_5_20.md#idp50532016) </span>

<span class="sect2"> [New Features:](changelog_4_5_20.md#idp50511048) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_5_20.md#idp50513672) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_5_20.md#idp50516704) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_5_20.md#idp50520456) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_5_20.md#idp50542544) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_5_20.md#idp50536608) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_5_20.md#idp50533200) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_5_20.md#idp50539504) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_5_20.md#idp50539760) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_5_20.md#idp50541672) </span>

<span class="sect2"> [Java collections and bind API Changes:](changelog_4_5_20.md#idp50542632) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_5_20.md#idp50546176) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_5_20.md#idp50548752) </span>

<span class="sect2"> [Replication Changes:](changelog_4_5_20.md#idp50547824) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_5_20.md#idp50557816) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_5_20.md#idp50534496) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_5_20.md#idp50532216) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_5_20.md#idp50542056) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_5_20.md#idp50543016) </span>

<span class="sect2"> [Utility Changes:](changelog_4_5_20.md#idp50556608) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_5_20.md#idp50557880) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 4.4 release interfaces to the Berkeley DB 4.5 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
