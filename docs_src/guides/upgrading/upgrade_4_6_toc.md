---
title: "Chapter 4. Upgrading Berkeley DB 4.5 applications to Berkeley DB 4.6"
api-name: "Chapter 4. Upgrading Berkeley DB 4.5 applications to Berkeley DB 4.6"
source: docs/upgrading/upgrade_4_6_toc.html
---
## Chapter 4. Upgrading Berkeley DB 4.5 applications to Berkeley DB 4.6

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_4_6_toc.md#upgrade_4_6_intro) </span>

<span class="sect1"> [C API cursor handle method names](upgrade_4_6_cursor.md) </span>

<span class="sect1"> [DB_MPOOLFILE-\>put](upgrade_4_6_memp_fput.md) </span>

<span class="sect1"> [B_MPOOLFILE-\>set](upgrade_4_6_memp_fset.md) </span>

<span class="sect1"> [Replication Events](upgrade_4_6_event.md) </span>

<span class="sect1"> [DB_REP_FULL_ELECTION](upgrade_4_6_full_election.md) </span>

<span class="sect1"> [Verbose Output](upgrade_4_6_verbose.md) </span>

<span class="sect1"> [DB_VERB_REPLICATION](upgrade_4_6_verb.md) </span>

<span class="sect1"> [Windows 9X](upgrade_4_6_win.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_4_6_disk.md) </span>

<span class="sect1"> [Berkeley DB 4.6.21 Change Log](changelog_4_6.md) </span>

<span class="sect2"> [4.6.21 Patches:](changelog_4_6.md#idp50449856) </span>

<span class="sect2"> [4.6.19 Patches](changelog_4_6.md#idp50370888) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_6.md#idp50361912) </span>

<span class="sect2"> [New Features:](changelog_4_6.md#idp50454856) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_6.md#idp50457960) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_6.md#idp50459800) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_6.md#idp50458344) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_6.md#idp50475672) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_6.md#idp50460536) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_6.md#idp50444272) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_6.md#idp50463616) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_6.md#idp50463872) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_6.md#idp50481800) </span>

<span class="sect2"> [Java collections and bind API Changes:](changelog_4_6.md#idp50464456) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_6.md#idp50464944) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_6.md#idp50465232) </span>

<span class="sect2"> [Replication Changes:](changelog_4_6.md#idp50486584) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_6.md#idp50466136) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_6.md#idp50465496) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_6.md#idp50451848) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_6.md#idp50452712) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_6.md#idp50468064) </span>

<span class="sect2"> [Utility Changes:](changelog_4_6.md#idp50475736) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_6.md#idp50479800) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 4.5 release interfaces to the Berkeley DB 4.6 release interfaces. This information does not describe how to upgrade Berkeley DB 1.85 release applications.
