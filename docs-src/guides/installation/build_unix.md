---
title: "Chapter 7.  Building Berkeley DB for UNIX/POSIX"
api-name: "Chapter 7.  Building Berkeley DB for UNIX/POSIX"
source: docs/installation/build_unix.html
---
## Chapter 7.  Building Berkeley DB for UNIX/POSIX

**Table of Contents**

<span class="sect1"> [Building for UNIX/POSIX](build_unix.md#build_unix_intro) </span>

<span class="sect2"> [Building the Berkeley DB SQL Interface](build_unix.md#build_unix_sqlinter) </span>

<span class="sect1"> [Configuring Berkeley DB](build_unix_conf.md) </span>

<span class="sect1"> [Configuring the SQL Interface](build_unix_sql.md) </span>

<span class="sect2"> [Changing Compile Options](build_unix_sql.md#config_sql) </span>

<span class="sect2"> [Enabling Extensions](build_unix_sql.md#idp500824) </span>

<span class="sect2"> [Building the JDBC Driver](build_unix_sql.md#build_unix_jdbc) </span>

<span class="sect2"> [Using the JDBC Driver](build_unix_sql.md#idp571856) </span>

<span class="sect2"> [Building the ODBC Driver](build_unix_sql.md#idp593744) </span>

<span class="sect2"> [Building the BFILE extension](build_unix_sql.md#bfile) </span>

<span class="sect1"> [Building a small memory footprint library](build_unix_small.md) </span>

<span class="sect1"> [Changing compile or load options](build_unix_flags.md) </span>

<span class="sect1"> [Cross-Compiling on Unix](cross_compile_unix.md) </span>

<span class="sect1"> [Installing Berkeley DB](build_unix_install.md) </span>

<span class="sect1"> [Dynamic shared libraries](build_unix_shlib.md) </span>

<span class="sect1"> [Running the test suite under UNIX](build_unix_test.md) </span>

<span class="sect2"> [Building SQL Test Suite on Unix](build_unix_test.md#build_unix_test_sql) </span>

<span class="sect1"> [Architecture independent FAQ](build_unix_notes.md) </span>

<span class="sect1"> [AIX](build_unix_aix.md) </span>

<span class="sect1"> [FreeBSD](build_unix_freebsd.md) </span>

<span class="sect1"> [Apple iOS (iPhone OS)](build_unix_iphone.md) </span>

<span class="sect1"> [IRIX](build_unix_irix.md) </span>

<span class="sect1"> [Linux](build_unix_linux.md) </span>

<span class="sect1"> [Mac OS X](build_unix_macosx.md) </span>

<span class="sect1"> [QNX](build_unix_qnx.md) </span>

<span class="sect1"> [SCO](build_unix_sco.md) </span>

<span class="sect1"> [Solaris](build_unix_solaris.md) </span>

<span class="sect1"> [SunOS](build_unix_sunos.md) </span>

## Building for UNIX/POSIX

<span class="sect2"> [Building the Berkeley DB SQL Interface](build_unix.md#build_unix_sqlinter) </span>

The Berkeley DB distribution builds up to four separate libraries: the base C API Berkeley DB library and the optional C++, Java, and Tcl API libraries. For portability reasons, each library is standalone and contains the full Berkeley DB support necessary to build applications; that is, the C++ API Berkeley DB library does not require any other Berkeley DB libraries to build and run C++ applications.

Building for Linux, Apple iOS (known as iPhone OS previously), Mac OS X or the QNX Neutrino release is the same as building for a conventional UNIX platform.

The Berkeley DB distribution uses the Free Software Foundation's <a href="http://www.gnu.org/software/autoconf/autoconf.html" class="ulink" target="_top">autoconf</a> and <a href="http://www.gnu.org/software/libtool/libtool.html" class="ulink" target="_top">libtool</a> tools to build on UNIX platforms. In general, the standard configuration and installation options for these tools apply to the Berkeley DB distribution.

To perform a standard UNIX build of Berkeley DB, change to the **build_unix** directory and then enter the following two commands:

``` c
../dist/configure
make
```

This will build the Berkeley DB library.

To install the Berkeley DB library, enter the following command:

``` c
make install
```

To rebuild Berkeley DB, enter:

``` c
make clean
make
```

If you change your mind about how Berkeley DB is to be configured, you must start from scratch by entering the following command:

``` c
make realclean
../dist/configure
make
```

To uninstall Berkeley DB, enter:

``` c
make uninstall
```

To build multiple UNIX versions of Berkeley DB in the same source tree, create a new directory at the same level as the build_unix directory, and then configure and build in that directory as described previously.

### Building the Berkeley DB SQL Interface

To perform a standard UNIX build of the Berkeley DB SQL interface, go to the **build_unix** directory and then enter the following two commands:

``` c
../dist/configure --enable-sql
make 
```

This creates a library, `libdb_sql`, and a command line tool, `dbsql`. You can create and manipulate SQL databases using the `dbsql` shell.

You can optionally provide the `--enable-sql_compat` argument to the `configure` script. In addition to creating `libdb_sql` and `dbsql` this causes a thin wrapper library called `libsqlite3` and a command line tool called `sqlite3` to be built. This library can be used as a drop-in replacement for SQLite. The `sqlite3` command line tool is identical to the `dbsql` executable but is named so that existing scripts for SQLite can easily work with Berkeley DB.

``` c
../dist/configure --enable-sql_compat
make 
```

There are several arguments you can specify when configuring the Berkeley DB SQL Interface. See <a href="build_unix_sql.md" class="xref" title="Configuring the SQL Interface">Configuring the SQL Interface</a> for more information.
