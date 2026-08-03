---
title: "Building a small memory footprint library"
api-name: "Building a small memory footprint library"
source: docs/installation/build_unix_small.html
---
## Building a small memory footprint library

There are a set of configuration options to assist you in building a small memory footprint library. These configuration options turn off specific functionality in the Berkeley DB library, reducing the code size. These configuration options include:

<span class="term"> `--enable-smallbuild` </span>  
Equivalent to individually specifying all of the following configuration options. In addition, when compiling building with the GNU gcc compiler, this option uses the `-Os` compiler build flag instead of the default `-O3`.

<span class="term"> `--with-cryptography=no` </span>  
Builds Berkeley DB without support for cryptography.

<span class="term"> `--disable-hash` </span>  
Builds Berkeley DB without support for the Hash access method.

<span class="term"> `--disable-heap` </span>  
Builds Berkeley DB without support for the Heap access method.

<span class="term"> `--disable-queue` </span>  
Builds Berkeley DB without support for the Queue access method.

<span class="term"> `--disable-replication` </span>  
Builds Berkeley DB without support for the database environment replication.

<span class="term"> `--disable-statistics` </span>  
Builds Berkeley DB without support for the statistics interfaces.

<span class="term"> `--disable-verify` </span>  
Builds Berkeley DB without support for database verification.

<span class="term"> `--enable-stripped_messages` </span>  
Strips message text from the error messages issued by Berkeley DB. This can reduce the size of the library by roughly another 22KB.

If your library has stripped messages, you can get an idea of what text should be issued for a given error message by using the <a href="../articles/mssgtxt/index.html" class="olink">Message Reference for Stripped Libraries</a> guide.

### Note

`--disable-cryptography` and `--enable-cryptography` are deprecated in the Berkeley DB 11gR2 release. Use `--with-cryptography=no` and `--with-cryptography=yes` instead.

The following configuration options will increase the size of the Berkeley DB library dramatically and are only useful when debugging applications:

<span class="term"> <a href="build_unix_conf.md#build_unix_conf.--enable-debug" class="link">--enable-debug</a> </span>  
Build Berkeley DB with symbols for debugging.

<span class="term"> <a href="build_unix_conf.md#build_unix_conf.--enable-debug_rop" class="link">--enable-debug_rop</a> </span>  
Build Berkeley DB with read-operation logging.

<span class="term"> <a href="build_unix_conf.md#build_unix_conf.--enable-debug_wop" class="link">--enable-debug_wop</a> </span>  
Build Berkeley DB with write-operation logging.

<span class="term"> <a href="build_unix_conf.md#build_unix_conf.--enable-diagnostic" class="link">--enable-diagnostic</a> </span>  
Build Berkeley DB with run-time debugging checks.

In addition, static libraries are usually smaller than shared libraries. By default Berkeley DB will build both shared and static libraries. To build only a static library, configure Berkeley DB with the <a href="build_unix_conf.md" class="xref" title="Configuring Berkeley DB">Configuring Berkeley DB</a> option.

The size of the Berkeley DB library varies depending on the compiler, machine architecture, and configuration options. As an estimate, production Berkeley DB libraries built with GNU gcc version 4.X compilers have footprints in the range of 600KB to 1.4MB on 32-bit x86 architectures, and in the range of 700KB to 1.6MB on 64-bit x86 architectures.

For assistance in further reducing the size of the Berkeley DB library, or in building small memory footprint libraries on other systems, please contact Berkeley DB support.
