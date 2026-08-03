---
title: "Chapter 15. Upgrading Berkeley DB 1.85 or 1.86 applications to Berkeley DB 2.0"
api-name: "Chapter 15. Upgrading Berkeley DB 1.85 or 1.86 applications to Berkeley DB 2.0"
source: docs/upgrading/upgrade_2_0_toc.html
---
## Chapter 15. Upgrading Berkeley DB 1.85 or 1.86 applications to Berkeley DB 2.0

**Table of Contents**

<span class="sect1"> [Introduction](upgrade_2_0_toc.md#upgrade_2_0_intro) </span>

<span class="sect1"> [System Integration](upgrade_2_0_system.md) </span>

<span class="sect1"> [Converting Applications](upgrade_2_0_convert.md) </span>

<span class="sect1"> [Upgrade Requirements](upgrade_2_0_disk.md) </span>

## Introduction

The following pages describe how to upgrade applications coded against the Berkeley DB 1.85 and 1.86 release interfaces to the Berkeley DB 2.0 release interfaces. They do not describe how to upgrade to the current Berkeley DB release interfaces.

It is not difficult to upgrade Berkeley DB 1.85 applications to use the Berkeley DB version 2 library. The Berkeley DB version 2 library has a Berkeley DB 1.85 compatibility API, which you can use by either recompiling your application's source code or by relinking its object files against the version 2 library. The underlying databases must be converted, however, as the Berkeley DB version 2 library has a different underlying database format.
