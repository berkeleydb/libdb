---
title: "Chapter 8. Upgrading Berkeley DB 4.1 applications to Berkeley DB 4.2"
api-name: "Chapter 8. Upgrading Berkeley DB 4.1 applications to Berkeley DB 4.2"
source: docs/upgrading/upgrade_4_2_toc.html
---
## Chapter 8. Upgrading Berkeley DB 4.1 applications to Berkeley DB 4.2

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_4_2_toc.md#upgrade_4_2_intro) </span>

<span class="sect1"> [Java](upgrade_4_2_java.md) </span>

<span class="sect1"> [Queue access method](upgrade_4_2_queue.md) </span>

<span class="sect1"> [DB_CHKSUM_SHA1](upgrade_4_2_cksum.md) </span>

<span class="sect1"> [DB_CLIENT](upgrade_4_2_client.md) </span>

<span class="sect1"> [DB-\>del](upgrade_4_2_del.md) </span>

<span class="sect1"> [DB-\>set_cache_priority](upgrade_4_2_priority.md) </span>

<span class="sect1"> [DB-\>verify](upgrade_4_2_verify.md) </span>

<span class="sect1"> [DB_LOCK_NOTGRANTED](upgrade_4_2_lockng.md) </span>

<span class="sect1"> [Replication](upgrade_4_2_repinit.md) </span>

<span class="sect2"> [Replication initialization](upgrade_4_2_repinit.md#idp50804696) </span>

<span class="sect2"> [Database methods and replication clients](upgrade_4_2_repinit.md#idp50772032) </span>

<span class="sect2"> [DB_ENV-\>rep_process_message()](upgrade_4_2_repinit.md#idp50779672) </span>

<span class="sect1"> [Client replication environments](upgrade_4_2_nosync.md) </span>

<span class="sect1"> [Tcl API](upgrade_4_2_tcl.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_2_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.2.52 Change Log](changelog_4_2_52.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_2_52.md#idp50822856) </span>

<span class="sect2"> [New Features:](changelog_4_2_52.md#idp50784344) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_2_52.md#idp50809288) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_2_52.md#idp50822104) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_2_52.md#idp50824288) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_2_52.md#idp50825368) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_2_52.md#idp50844704) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_2_52.md#idp50828568) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_2_52.md#idp50858440) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_2_52.md#idp50832248) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_2_52.md#idp50815840) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_2_52.md#idp50867864) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_2_52.md#idp50852544) </span>

<span class="sect2"> [Replication Changes:](changelog_4_2_52.md#idp50858528) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_2_52.md#idp50877816) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_2_52.md#idp50865088) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_2_52.md#idp50868008) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_2_52.md#idp50865504) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_2_52.md#idp50845064) </span>

<span class="sect2"> [Utility Changes:](changelog_4_2_52.md#idp50858944) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_2_52.md#idp50892568) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 4.1 release interfaces to the Berkeley DB 4.2 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
