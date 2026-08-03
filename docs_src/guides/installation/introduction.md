---
title: "Chapter 1. Introduction"
api-name: "Chapter 1. Introduction"
source: docs/installation/introduction.html
---
## Chapter 1. Introduction

**Table of Contents**

<span class="sect1"> [Installation Overview](introduction.md#install-overview) </span>

<span class="sect1"> [Supported Platforms](ch01s02.md) </span>

Welcome to the Berkeley DB. This manual describes how to configure, build and install Berkeley DB. Installation of DB for all of the platforms it officially supports is described in this manual. Upgrade instructions and release notes for newer versions of this product are described here. For infomation on upgrading from historical versions, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/upgrading/index.html" class="ulink" target="_top">Berkeley DB Upgrade Guide.</a>

Note that some operating systems and distributions might provide DB, either by default or as part of an installation option. If so, those platforms will have installation instructions for DB specific to them. In this situation, you should see the documentation for your operating system or distribution provider for information on how to get DB on your platform.

## Installation Overview

Berkeley DB is an open-source product, and as such it is usually offered in source-code format. This means that placing DB on your platform requires you to configure the build scripts, compile it, and then install the product onto your host system. The exception to this are Microsoft Windows platforms for which a binary installer is available. Note that for Windows platforms, you can still compile the product from source if you desire.

For \*nix systems, including the BSD and Linux systems, the usual `configure`, `make` and `make install` installation process is used to place DB on your platform.

For information on building and installing Berkeley DB on:

- Microsoft Windows, see <a href="build_win.md" class="xref" title="Chapter 5.  Building Berkeley DB for Windows">Building Berkeley DB for Windows</a> or <a href="build_wince.md" class="xref" title="Chapter 6.  Building Berkeley DB for Windows Mobile">Building Berkeley DB for Windows Mobile</a> .

- Unix/POSIX — including Linux, BSD, Apple iOS (known as iPhone OS previously), and Mac OS X — see <a href="build_unix.md" class="xref" title="Chapter 7.  Building Berkeley DB for UNIX/POSIX">Building Berkeley DB for UNIX/POSIX</a> .

- VxWorks, see <a href="build_vxworks.md" class="xref" title="Chapter 8.  Building Berkeley DB for VxWorks">Building Berkeley DB for VxWorks</a> .
