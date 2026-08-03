---
title: "Chapter 7. Upgrading Berkeley DB 4.2 applications to Berkeley DB 4.3"
api-name: "Chapter 7. Upgrading Berkeley DB 4.2 applications to Berkeley DB 4.3"
source: docs/upgrading/upgrade_4_3_toc.html
---
## Chapter 7. Upgrading Berkeley DB 4.2 applications to Berkeley DB 4.3

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_4_3_toc.md#upgrade_4_3_intro) </span>

<span class="sect1"> [Java](upgrade_4_3_java.md) </span>

<span class="sect1"> [DB_ENV-\>set_errcall, DB-\>set_errcall](upgrade_4_3_err.md) </span>

<span class="sect1"> [DBcursor-\>c_put](upgrade_4_3_cput.md) </span>

<span class="sect1"> [DB-\>stat](upgrade_4_3_stat.md) </span>

<span class="sect1"> [DB_ENV-\>set_verbose](upgrade_4_3_verb.md) </span>

<span class="sect1"> [Logging](upgrade_4_3_log.md) </span>

<span class="sect1"> [DB_FILEOPEN](upgrade_4_3_fileopen.md) </span>

<span class="sect1"> [ENOMEM and DbMemoryException](upgrade_4_3_enomem.md) </span>

<span class="sect1"> [Replication](upgrade_4_3_repl.md) </span>

<span class="sect1"> [Run-time configuration](upgrade_4_3_rtc.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_3_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.3.29 Change Log](changelog_4_3_29.md) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_3_29.md#idp50694880) </span>

<span class="sect2"> [New Features:](changelog_4_3_29.md#idp50670248) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_3_29.md#idp50673968) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_3_29.md#idp50703424) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_3_29.md#idp50690848) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_3_29.md#idp50691272) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_3_29.md#idp50694944) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_3_29.md#idp50697104) </span>

<span class="sect2"> [Recno Access Method Changes](changelog_4_3_29.md#idp50720352) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_3_29.md#idp50700784) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_3_29.md#idp50670632) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_3_29.md#idp50702384) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_3_29.md#idp50703784) </span>

<span class="sect2"> [Replication Changes:](changelog_4_3_29.md#idp50685776) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_3_29.md#idp50733112) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_3_29.md#idp50712384) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_3_29.md#idp50740760) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_3_29.md#idp50695328) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_3_29.md#idp50720440) </span>

<span class="sect2"> [Utility Changes:](changelog_4_3_29.md#idp50724480) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_3_29.md#idp50724864) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 4.2 release interfaces to the Berkeley DB 4.3 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
