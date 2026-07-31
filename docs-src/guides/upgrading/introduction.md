---
title: "Chapter 1. Introduction"
api-name: "Chapter 1. Introduction"
source: docs/upgrading/introduction.html
---
## Chapter 1. Introduction

**Table of Contents**

<span class="sect1"> [Library version information](introduction.md#upgrade_version) </span>

This manual describes how to upgrade from historical versions of Berkeley DB (Berkeley DB 4.7 and older). For information on upgrading newer releases of the product (anything newer than DB 4.7), and on building and installing Berkeley DB on all of the platforms it officially supports, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/installation/index.html" class="ulink" target="_top">Berkeley DB Installation and Build Guide.</a>

## Library version information

Each release of the Berkeley DB library has a major version number, a minor version number, and a patch number.

The major version number changes only when major portions of the Berkeley DB functionality have been changed. In this case, it may be necessary to significantly modify applications in order to upgrade them to use the new version of the library.

The minor version number changes when Berkeley DB interfaces have changed, and the new release is not entirely backward-compatible with previous releases. To upgrade applications to the new version, they must be recompiled and potentially, minor modifications made (for example, the order of arguments to a function might have changed).

The patch number changes on each release. If only the patch number has changed in a release, applications do not need to be recompiled, and they can be upgraded to the new version by installing the new version of a shared library or by relinking the application to the new version of a static library.

Internal Berkeley DB interfaces may change at any time and during any release, without warning. This means that the library must be entirely recompiled and reinstalled when upgrading to new releases of the library because there is no guarantee that modules from the current version of the library will interact correctly with modules from a previous release.

To retrieve the Berkeley DB version information, applications should use the <a href="../../api/c/envversion.md" class="olink">DB_ENV-&gt;version()</a> function. In addition to the previous information, the <a href="../../api/c/envversion.md" class="olink">DB_ENV-&gt;version()</a> function returns a string encapsulating the version information, suitable for display to a user.
