---
title: "Chapter 6. Upgrading Berkeley DB 4.3 applications to Berkeley DB 4.4"
api-name: "Chapter 6. Upgrading Berkeley DB 4.3 applications to Berkeley DB 4.4"
source: docs/upgrading/upgrade_4_4_toc.html
---
## Chapter 6. Upgrading Berkeley DB 4.3 applications to Berkeley DB 4.4

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_4_4_toc.md#upgrade_4_4_intro) </span>

<span class="sect1"> [DB_AUTO_COMMIT](upgrade_4_4_autocommit.md) </span>

<span class="sect1"> [DB_DEGREE_2, DB_DIRTY_READ](upgrade_4_4_isolation.md) </span>

<span class="sect1"> [DB_JOINENV](upgrade_4_4_joinenv.md) </span>

<span class="sect1"> [mutexes](upgrade_4_4_mutex.md) </span>

<span class="sect1"> [DB_MPOOLFILE-\>set_clear_len](upgrade_4_4_clear.md) </span>

<span class="sect1"> [lock statistics](upgrade_4_4_lockstat.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_4_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.4.16 Change Log](changelog_4_4_16.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_4_16.md#idp50595920) </span>

<span class="sect2"> [New Features:](changelog_4_4_16.md#idp50583264) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_4_16.md#idp50583648) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_4_16.md#idp50567656) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_4_16.md#idp50591960) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_4_16.md#idp50592384) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_4_16.md#idp50595984) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_4_16.md#idp50597856) </span>

<span class="sect2"> [Recno Access Method Changes](changelog_4_4_16.md#idp50598936) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_4_16.md#idp50598072) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_4_16.md#idp50600424) </span>

<span class="sect2"> [Java collections and bind API Changes:](changelog_4_4_16.md#idp50621112) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_4_16.md#idp50604672) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_4_16.md#idp50589536) </span>

<span class="sect2"> [Replication Changes:](changelog_4_4_16.md#idp50610200) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_4_16.md#idp50594920) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_4_16.md#idp50614600) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_4_16.md#idp50614888) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_4_16.md#idp50635800) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_4_16.md#idp50617400) </span>

<span class="sect2"> [Utility Changes:](changelog_4_4_16.md#idp50617824) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_4_16.md#idp50621200) </span>

<span class="sect1"> [Berkeley DB 4.4.20 Change Log](changelog_4_4_20.md) </span>

<span class="sect2"> [Changes since Berkeley DB 4.4.16:](changelog_4_4_20.md#idp50624312) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 4.3 release interfaces to the Berkeley DB 4.4 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
