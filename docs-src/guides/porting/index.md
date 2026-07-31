---
title: "Porting Berkeley DB"
api-name: "Porting Berkeley DB"
source: docs/porting/index.html
---
# Porting Berkeley DB

**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

Other names may be trademarks of their respective owners.

To obtain a copy of this document's original source code, please submit a request to the Oracle Technology Network forum at: <a href="http://forums.oracle.com/forums/forum.jspa?forumID=271" class="ulink" target="_top">http://forums.oracle.com/forums/forum.jspa?forumID=271</a>

9/9/2013

------------------------------------------------------------------------

**Table of Contents**

<span class="preface"> [Preface](preface.md) </span>

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [Audience](audience.md) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

<span class="chapter"> [1. Introduction to Porting Berkeley DB](introduction.md) </span>

<span class="sect1"> [Types of Berkeley DB ports](introduction.md#porttypes) </span>

<span class="sect2"> [When Oracle Has Agreed to Support Berkeley DB on the New Platform](introduction.md#idp139688) </span>

<span class="sect2"> [When Oracle has Not Agreed to Support Berkeley DB on the New Platform](introduction.md#idp79768) </span>

<span class="sect1"> [Berkeley DB Porting Process](portprocess.md) </span>

<span class="chapter"> [2. Creating a New Berkeley DB Binary](newbinary.md) </span>

<span class="sect1"> [Creating a Base Build of Berkeley DB](newbinary.md#basebuild) </span>

<span class="sect1"> [Determining the Scope of the Modifications](modscope.md) </span>

<span class="sect2"> [Do Changes Need to be Made to the Operating System Functionality?](modscope.md#osfunc) </span>

<span class="sect2"> [Are Some Standard Functions Missing on the Target Platform?](modscope.md#standardfunc) </span>

<span class="sect2"> [How Will the Port Handle Shared Memory?](modscope.md#sharedmem) </span>

<span class="sect2"> [What Type of Mutexes Will the Port Use?](modscope.md#typemutex) </span>

<span class="sect2"> [Do Any Other Changes Need to be Made?](modscope.md#otherchanges) </span>

<span class="sect1"> [Building on the Target Platform](buildtarget.md) </span>

<span class="sect1"> [Source Code Layout](layout.md) </span>

<span class="chapter"> [3. Testing and Certifying the Port](testport.md) </span>

<span class="sect1"> [Types of Tests for Berkeley DB](testport.md#testtypes) </span>

<span class="sect1"> [Modifying the Tests](modifytest.md) </span>

<span class="sect1"> [Running the Tests](testrun.md) </span>

<span class="sect1"> [Reviewing the Results of the Tests](testreview.md) </span>

<span class="sect1"> [Integrating Changes into the Berkeley DB Source Code](sourceintegrate.md) </span>

<span class="sect1"> [Certifying a Port of Berkeley DB](certport.md) </span>
