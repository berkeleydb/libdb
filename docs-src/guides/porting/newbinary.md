---
title: "Chapter 2. Creating a New Berkeley DB Binary"
api-name: "Chapter 2. Creating a New Berkeley DB Binary"
source: docs/porting/newbinary.html
---
## Chapter 2. Creating a New Berkeley DB Binary

**Table of Contents**

<span class="sect1"> [Creating a Base Build of Berkeley DB](newbinary.md#basebuild) </span>

<span class="sect1"> [Determining the Scope of the Modifications](modscope.md) </span>

<span class="sect2"> [Do Changes Need to be Made to the Operating System Functionality?](modscope.md#osfunc) </span>

<span class="sect2"> [Are Some Standard Functions Missing on the Target Platform?](modscope.md#standardfunc) </span>

<span class="sect2"> [How Will the Port Handle Shared Memory?](modscope.md#sharedmem) </span>

<span class="sect2"> [What Type of Mutexes Will the Port Use?](modscope.md#typemutex) </span>

<span class="sect2"> [Do Any Other Changes Need to be Made?](modscope.md#otherchanges) </span>

<span class="sect1"> [Building on the Target Platform](buildtarget.md) </span>

<span class="sect1"> [Source Code Layout](layout.md) </span>

Creating a new Berkeley DB executable on the target platform, involves:

1.  <a href="newbinary.md#basebuild" class="xref" title="Creating a Base Build of Berkeley DB">Creating a Base Build of Berkeley DB</a>

2.  <a href="modscope.md" class="xref" title="Determining the Scope of the Modifications">Determining the Scope of the Modifications</a>

3.  <a href="buildtarget.md" class="xref" title="Building on the Target Platform">Building on the Target Platform</a>

## Creating a Base Build of Berkeley DB

The simplest way to begin a port is to attempt to configure and build Berkeley DB on a UNIX or UNIX-like system. This gives you a list of the files that you needed to build Berkeley DB as well as the configuration files you can use as a starting point for building on your target port.

To create a base build of Berkeley DB, following the instructions in the *Berkeley DB Programmer's Reference Guide*:

1.  Download a Berkeley DB distribution from <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html</a>.

2.  Build Berkeley DB.

Berkeley DB uses the GNU autoconf tools for configuration on almost all of the platforms it supports. Specifically, the include file `db_config.h` configures the Berkeley DB build. The simplest way to begin a port is to configure and build Berkeley DB on a UNIX or UNIX-like system, and then take the `Makefile` and `db_config.h` file created by that configuration, and modify it by hand to reflect the needs of the new architecture. Unless you are already familiar with the GNU autoconf toolset, we do not recommend you take the time to integrate your changes back into the Berkeley DB autoconfiguration framework. Instead, send us context diffs of your changes and any new source files you created, and we can integrate the changes into our source tree.
