---
title: "Chapter 1. Introduction to Porting Berkeley DB"
api-name: "Chapter 1. Introduction to Porting Berkeley DB"
source: docs/porting/introduction.html
---
## Chapter 1. Introduction to Porting Berkeley DB

**Table of Contents**

<span class="sect1"> [Types of Berkeley DB ports](introduction.md#porttypes) </span>

<span class="sect2"> [When Oracle Has Agreed to Support Berkeley DB on the New Platform](introduction.md#idp139688) </span>

<span class="sect2"> [When Oracle has Not Agreed to Support Berkeley DB on the New Platform](introduction.md#idp79768) </span>

<span class="sect1"> [Berkeley DB Porting Process](portprocess.md) </span>

Berkeley DB is an open source database product that supports a variety of platforms. When there is a need to run Berkeley DB on a platform that is currently not supported, DB is distributed in source code form that you can use as base source to port Berkeley DB to that platform.

Berkeley DB is designed to be as portable as possible, and has been ported to a wide variety of systems, from Wind River's Tornado system, to VMS, to Windows/NT and Windows/95, and most existing UNIX platforms. It runs on 16-bit, 32-bit, and 64-bit machines, little or big-endian. The difficulty of a port depends on how much of the ANSI C and POSIX 1003.1 standards the new architecture offers.

Before you begin actually porting Berkeley DB, you need an understanding of the:

- <a href="introduction.md#porttypes" class="xref" title="Types of Berkeley DB ports">Types of Berkeley DB ports</a>

- <a href="portprocess.md" class="xref" title="Berkeley DB Porting Process">Berkeley DB Porting Process</a>

## Types of Berkeley DB ports

<span class="sect2"> [When Oracle Has Agreed to Support Berkeley DB on the New Platform](introduction.md#idp139688) </span>

<span class="sect2"> [When Oracle has Not Agreed to Support Berkeley DB on the New Platform](introduction.md#idp79768) </span>

There are several types of Berkeley DB ports:

- Ports developed and supported by Oracle

- Ports developed by a customer or a partner, but which Oracle has agreed to support.

- Ports developed, maintained, and supported by a customer or partner.

For a port developed by a customer or a partner, the general steps for porting Berkeley DB to a new platform are the same whether or not Oracle has agreed to support Berkeley DB on the new platform. For example, after you complete the port you send it to Berkeley DB as described in <a href="sourceintegrate.md" class="xref" title="Integrating Changes into the Berkeley DB Source Code">Integrating Changes into the Berkeley DB Source Code</a>. However, there are some differences.

### When Oracle Has Agreed to Support Berkeley DB on the New Platform

When porting Berkeley DB to a platform that Oracle has agreed to support, you need to have Berkeley DB engineering review your port at various points. These review points are discussed more fully in <a href="sourceintegrate.md" class="xref" title="Integrating Changes into the Berkeley DB Source Code">Integrating Changes into the Berkeley DB Source Code</a>, <a href="modifytest.md" class="xref" title="Modifying the Tests">Modifying the Tests</a>, and <a href="testreview.md" class="xref" title="Reviewing the Results of the Tests">Reviewing the Results of the Tests</a>.

It is up to you to submit the results of the tests (test_micro, test_mutex, and, if possible, the entire tcl test suit) for review by Oracle Berkelely DB engineering in order for Oracle to consider providing support for Berkeley DB on a new platform.

You must also assign copyrights for your changes to any part of Berkeley DB to "Oracle Corporation" and attest to the fact that you are not infringing on any software patents for the changes to be included in the general Berekely DB distribution.

Once the port is certified, Oracle provides support for Berkeley DB on the new platform in the same manner that it does for Berkeley DB running on other established platforms.

### When Oracle has Not Agreed to Support Berkeley DB on the New Platform

When Oracle has <span class="emphasis">*not*</span> agreed to support Berkeley DB on the new platform, the customer or partner assume the responsibility of front-line support. When it is determined that there is a problem in the code that was not modified by the customer or partner, then Berkeley DB engineering provides support to the customer or vendor who implemented the port, However, in these cases, Oracle needs access to the platform and hardware for diagnosing, debugging, and testing.
