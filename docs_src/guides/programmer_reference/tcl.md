---
title: "Chapter 21.  Berkeley DB Extensions: Tcl"
api-name: "Chapter 21.  Berkeley DB Extensions: Tcl"
source: docs/programmer_reference/tcl.html
---
## Chapter 21.  Berkeley DB Extensions: Tcl

**Table of Contents**

<span class="sect1"> [Loading Berkeley DB with Tcl](tcl.md#tcl_intro) </span>

<span class="sect2"> [Installing as a Tcl Package](tcl.md#idp53366464) </span>

<span class="sect2"> [Loading Berkeley DB with Tcl](tcl.md#idp53356912) </span>

<span class="sect1"> [Using Berkeley DB with Tcl](tcl_using.md) </span>

<span class="sect1"> [Tcl API programming notes](tcl_program.md) </span>

<span class="sect1"> [Tcl error handling](tcl_error.md) </span>

<span class="sect1"> [Tcl FAQ](tcl_faq.md) </span>

## Loading Berkeley DB with Tcl

<span class="sect2"> [Installing as a Tcl Package](tcl.md#idp53366464) </span>

<span class="sect2"> [Loading Berkeley DB with Tcl](tcl.md#idp53356912) </span>

Berkeley DB includes a dynamically loadable Tcl API, which requires that Tcl/Tk 8.5 or later already be installed on your system. You can download a copy of Tcl from the <a href="http://www.tcl.tk" class="ulink" target="_top">Tcl Developer Xchange</a> Web site.

This document assumes that you already configured Berkeley DB for Tcl support, and you have built and installed everything where you want it to be. If you have not done so, see <a href="../../guides/installation/build_unix_conf.md" class="olink">Configuring Berkeley DB</a> or <a href="../../guides/installation/build_win_tcl.md" class="olink">Building the Tcl API</a> in the Berkeley DB Installation and Build Guide for more information.

### Installing as a Tcl Package

Once enabled, the Berkeley DB shared library for Tcl is automatically installed as part of the standard installation process. However, if you want to be able to dynamically load it as a Tcl package into your script, there are several steps that must be performed:

1.  Run the Tcl shell in the install directory.
2.  Append this directory to your auto_path variable.
3.  Run the pkg_mkIndex proc, giving the name of the Berkeley DB Tcl library.

For example:

``` c
# tclsh8.5
% lappend auto_path /usr/local/BerkeleyDB.5.2/lib
% pkg_mkIndex /usr/local/BerkeleyDB.5.2/lib libdb_tcl-5.2.so
```

Note that your Tcl and Berkeley DB version numbers may differ from the example, and so your tclsh and library names may be different.

### Loading Berkeley DB with Tcl

The Berkeley DB package may be loaded into the user's interactive Tcl script (or wish session) via the **load** command. For example:

``` c
load /usr/local/BerkeleyDB.5.2/lib/libdb_tcl-5.2.so
```

Note that your Berkeley DB version numbers may differ from the example, and so the library name may be different.

If you installed your library to run as a Tcl package, Tcl application scripts should use the **package** command to indicate to the Tcl interpreter that it needs the Berkeley DB package and where to find it. For example:

``` c
lappend auto_path "/usr/local/BerkeleyDB.5.2/lib"
package require Db_tcl
```

No matter which way the library gets loaded, it creates a command named **berkdb**. All the Berkeley DB functionality is accessed via this command and additional commands it creates on behalf of the application. A simple test to determine whether everything is loaded and ready is to display the library version, as follows:

``` c
berkdb version -string
```

This should return you the Berkeley DB version in a string format.
