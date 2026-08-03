---
title: "Configuring Berkeley DB"
api-name: "Configuring Berkeley DB"
source: docs/installation/build_unix_conf.html
---
## Configuring Berkeley DB

There are several arguments you can specify when configuring Berkeley DB. Although only the Berkeley DB-specific ones are described here, most of the standard GNU autoconf arguments are available and supported. To see a complete list of possible arguments, specify the --help flag to the configure program.

The Berkeley DB specific arguments are as follows:

- **--disable-largefile**

  Some systems, notably versions of Solaris, require special compile-time options in order to create files larger than 2^32 bytes. These options are automatically enabled when Berkeley DB is compiled. For this reason, binaries built on current versions of these systems may not run on earlier versions of the system because the library and system calls necessary for large files are not available. To disable building with these compile-time options, enter --disable-largefile as an argument to configure.

- **--disable-shared, --disable-static**

  On systems supporting shared libraries, Berkeley DB builds both static and shared libraries by default. (Shared libraries are built using <a href="http://www.gnu.org/software/libtool/libtool.html" class="ulink" target="_top">the GNU Project's Libtool</a> distribution, which supports shared library builds on many (although not all) systems.) To not build shared libraries, configure using the --disable-shared argument. To not build static libraries, configure using the --disable-static argument.

- **--disable-heap**

  Disables the Heap access method so that it cannot be used by Berkeley DB applications.

- **--enable-compat185**

  To compile or load Berkeley DB 1.85 applications against this release of the Berkeley DB library, enter --enable-compat185 as an argument to configure. This will include Berkeley DB 1.85 API compatibility code in the library.

- **--enable-cxx**

  To build the Berkeley DB C++ API, enter --enable-cxx as an argument to configure.

- **--enable-debug**

  To build Berkeley DB with **-g** as a compiler flag and with **DEBUG** \#defined during compilation, enter --enable-debug as an argument to configure. This will create a Berkeley DB library and utilities with debugging symbols, as well as load various routines that can be called from a debugger to display pages, cursor queues, and so forth. If installed, the utilities will not be stripped. This argument should not be specified when configuring to build production binaries.

- **--enable-debug_rop**

  To build Berkeley DB to output log records for read operations, enter --enable-debug_rop as an argument to configure. This argument should not be specified when configuring to build production binaries.

- **--enable-debug_wop**

  To build Berkeley DB to output log records for write operations, enter --enable-debug_wop as an argument to configure. This argument should not be specified when configuring to build production binaries.

- **--enable-diagnostic**

  To build Berkeley DB with run-time debugging checks, enter --enable-diagnostic as an argument to configure. This causes a number of additional checks to be performed when Berkeley DB is running, and also causes some failures to trigger process abort rather than returning errors to the application. Applications built using this argument should not share database environments with applications built without this argument. This argument should not be specified when configuring to build production binaries.

- **--enable-dump185**

  To convert Berkeley DB 1.85 (or earlier) databases to this release of Berkeley DB, enter --enable-dump185 as an argument to configure. This will build the <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility, which can dump Berkeley DB 1.85 and 1.86 databases in a format readable by the Berkeley DB <a href="../../api/c/db_load.md" class="olink">db_load</a> utility.

  The system libraries with which you are loading the <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility must already contain the Berkeley DB 1.85 library routines for this to work because the Berkeley DB distribution does not include them. If you are using a non-standard library for the Berkeley DB 1.85 library routines, you will have to change the Makefile that the configuration step creates to load the <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility with that library.

- **--enable-java**

  To build the Berkeley DB Java API, enter --enable-java as an argument to configure. To build Java, you must also build with shared libraries. Before configuring, you must set your PATH environment variable to include javac. Note that it is not sufficient to include a symbolic link to javac in your PATH because the configuration process uses the location of javac to determine the location of the Java include files (for example, jni.h). On some systems, additional include directories may be needed to process jni.h; see <a href="build_unix_flags.md" class="xref" title="Changing compile or load options">Changing compile or load options</a> for more information.

- **--enable-posixmutexes**

  To force Berkeley DB to use the POSIX pthread mutex interfaces for underlying mutex support, enter --enable-posixmutexes as an argument to configure. This is rarely necessary: POSIX mutexes will be selected automatically on systems where they are the preferred implementation.

  The --enable-posixmutexes configuration argument is normally used in two ways: First, when there are multiple mutex implementations available and the POSIX mutex implementation is not the preferred one (for example, on Solaris where the LWP mutexes are used by default). Second, by default the Berkeley DB library will only select the POSIX mutex implementation if it supports mutexes shared between multiple processes, as described for the pthread_condattr_setpshared and pthread_mutexattr_setpshared interfaces. The --enable-posixmutexes configuration argument can be used to force the selection of POSIX mutexes in this case, which can improve application performance significantly when the alternative mutex implementation is a non-blocking one (for example test-and-set assembly instructions). However, configuring to use POSIX mutexes when the implementation does not have inter-process support will only allow the creation of private database environments, that is, environments where the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag is specified to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method.

  Specifying the --enable-posixmutexes configuration argument may require that applications and Berkeley DB be linked with the -lpthread library.

- **--enable-pthread_api**

  To configure Berkeley DB for a POSIX pthreads application (with the exception that POSIX pthread mutexes may not be selected as the underlying mutex implementation for the build), enter --enable-pthread_api as an argument to configure. The build will include the Berkeley DB replication manager interfaces and will use the POSIX standard pthread_self and pthread_yield functions to identify threads of control and yield the processor. The --enable-pthread_api argument requires POSIX pthread support already be installed on your system.

  Specifying the --enable-pthread_api configuration argument may require that applications and Berkeley DB be linked with the -lpthread library.

- **--enable-sql**

  To build the command tool dbsql, enter --enable-sql as an argument to configure. The dbsql utility provides access to the Berkeley DB SQL interface. See <a href="build_unix_sql.md" class="xref" title="Configuring the SQL Interface">Configuring the SQL Interface</a> for more information.

- **--enable-sql_compat**

  To build the command tool sqlite3, enter --enable-sql_compat as an argument to configure. Sqlite3 is a command line tool that enables you to manually enter and execute SQL commands. It is identical to the dbsql executable but named so that existing scripts for SQLite can easily work with Berkeley DB. See <a href="build_unix_sql.md" class="xref" title="Configuring the SQL Interface">Configuring the SQL Interface</a> for more information.

- **--enable-sql_codegen**

  To build the command line tool db_sql_codegen, enter --enable-sql_codegen as an argument to configure. The db_sql_codegen utility translates a schema description written in a SQL Data Definition Language dialect into C code that implements the schema using Berkeley DB.

- **--enable-smallbuild**

  To build a small memory footprint version of the Berkeley DB library, enter --enable-smallbuild as an argument to configure. The --enable-smallbuild argument is equivalent to individually specifying --with-cryptography=no, --disable-hash, --disable-queue, --disable-replication, --disable-statistics and --disable-verify, turning off cryptography support, the Hash and Queue access methods, database environment replication support and database and log verification support. See <a href="build_unix_small.md" class="xref" title="Building a small memory footprint library">Building a small memory footprint library</a> for more information.

- **--enable-stl**

  To build the Berkeley DB C++ STL API, enter --enable-stl as an argument to configure. Setting this argument implies that --enable-cxx is set, and the Berkeley DB C++ API will be built too.

  There will be a libdb_stl-X.X.a and libdb_stl-X.X.so built, which are the static and shared library you should link your application with in order to make use of Berkeley DB via its STL API.

  If your compiler is not ISO C++ compliant, the configure may fail with this argument specified because the STL API requires standard C++ template features. In this case, you will need a standard C++ compiler. So far gcc is the best choice, we have tested and found that gcc-3.4.4 and all its newer versions can build the Berkeley DB C++ STL API successfully.

  For information on db_stl supported compilers, see the <a href="../../guides/programmer_reference/stl.md#stl_intro_portability" class="olink">Portability section</a> in the <span class="emphasis">*Programmer's Reference Guide*</span>.

  And you need to include the STL API header files in your application code. If you are using the Berkeley DB source tree, the header files are in \<Berkeley DB Source Root \>/stl directory; If you are using the installed version, these header files are in \< Berkeley DB Installed Directory\>/include, as well as the db.h and db_cxx.h header files.

- **--enable-tcl**

  To build the Berkeley DB Tcl API, enter --enable-tcl as an argument to configure. This configuration argument expects to find Tcl's tclConfig.sh file in the `/usr/local/lib` directory. See the --with-tcl argument for instructions on specifying a non-standard location for the Tcl installation. See <a href="../../guides/programmer_reference/tcl.md#tcl_intro" class="olink">Loading Berkeley DB with Tcl</a> for information on sites from which you can download Tcl and which Tcl versions are compatible with Berkeley DB. To build Tcl, you must also build with shared libraries.

- **--enable-test**

  To build the Berkeley DB test suite, enter --enable-test as an argument to configure. To run the Berkeley DB test suite, you must also build the Tcl API. This argument should not be specified when configuring to build production binaries.

- **--enable-uimutexes**

  To force Berkeley DB to use the UNIX International (UI) mutex interfaces for underlying mutex support, enter --enable-uimutexes as an argument to configure. This is rarely necessary: UI mutexes will be selected automatically on systems where they are the preferred implementation.

  The --enable-uimutexes configuration argument is normally used when there are multiple mutex implementations available and the UI mutex implementation is not the preferred one (for example, on Solaris where the LWP mutexes are used by default).

  Specifying the --enable-uimutexes configuration argument may require that applications and Berkeley DB be linked with the -lthread library.

- **--enable-umrw**

  Rational Software's Purify product and other run-time tools complain about uninitialized reads/writes of structure fields whose only purpose is padding, as well as when heap memory that was never initialized is written to disk. Specify the --enable-umrw argument during configuration to mask these errors. This argument should not be specified when configuring to build production binaries.

- **--enable-dtrace** \[**--enable-perfmon-statistics**\]

  To build Berkeley DB with performance event monitoring probes add --enable-dtrace to the configuration options. Both native DTrace (on Solaris and Mac OS X) and the Statically Defined Tracing compatibility layer in Linux SystemTap version 1.1 or better are supported. That compatibility package may be called systemtap-sdt-devel; it includes `sys/sdt.h`.

  If --enable-perfmon-statistics is combined with --enable-dtrace then additional probes are defined for the tracking variables from which DB's statistics are obtained. They allow DTrace and SystemTap access to these values when they are updated, are the basis of the statistics as displayed db_stat and the API functions that return statistics.

  The --enable-dtrace option may not be specified at the same time as --disable-statistics.

  For information on Berkeley DB Performance Event Monitoring, see the <a href="../../guides/programmer_reference/program_perfmon.md" class="olink">Performance Event Monitoring</a> section in the <span class="emphasis">*Programmer's Reference Guide*</span>.

- **--enable-localization**

  Enable localized error message text, if available. This option should not be used when `--enable-stripped_messages` is in use.

- **--enable-stripped_messages**

  Causes all error messages to be stripped of their textual information. Instead, only error return codes are used. This option should not be used when `--enable-localization` is in use. Use of this build option can reduce your library foot print by up to 44KB (.so) or 50KB (.a).

  If you use this configuration option, you can get an idea of what text should be issued for a given error message by using the <a href="../articles/mssgtxt/index.html" class="olink">Message Reference for Stripped Libraries</a> guide.

- **--with-cryptography**

  To build Berkeley DB with support for cryptography, enter --with-cryptography=yes as an argument to configure.

  To build Berkeley DB without support for cryptography, enter --with-cryptography=no as an argument to configure.

  To build Berkeley DB with support for cryptography using Intel's Performance Primitive (IPP) library, enter --with-cryptography=ipp as an argument to configure. Additionally, set the following arguments:

  -L/path/to/ipp/sharedlib to LDFLAGS

  -I/path/to/ipp/include to CPPFLAGS

  -lippcpem64t -lpthread to LIBS

  An example configuration command for IPP encryption is as follows:

  ``` c
   ../dist/configure -with-cryptography=ipp 
           CPPFLAGS="-I/opt/intel/ipp/6.1.3.055/em64t/include" 
           LDFLAGS="-L/opt/intel/ipp/6.1.3.055/em64t/sharedlib" 
           LIBS="-lippcpem64t -lpthread"
  ```

  See the <a href="http://software.intel.com/en-us/articles/intel-integrated-performance-primitives-documentation/" class="ulink" target="_top">Intel Documenation</a> for specific instructions on configuring environment variables.

  Note: The --with-cryptography=ipp argument works only on Linux.

- **--with-mutex=MUTEX**

  To force Berkeley DB to use a specific mutex implementation, configure with --with-mutex=MUTEX, where MUTEX is the mutex implementation you want. For example, --with-mutex=x86/gcc-assembly will configure Berkeley DB to use the x86 GNU gcc compiler based test-and-set assembly mutexes. This is rarely necessary and should be done only when the default configuration selects the wrong mutex implementation. A list of available mutex implementations can be found in the distribution file `dist/aclocal/mutex.m4`.

- **--with-tcl=DIR**

  To build the Berkeley DB Tcl API, enter --with-tcl=DIR, replacing DIR with the directory in which the Tcl tclConfig.sh file may be found. See <a href="../../guides/programmer_reference/tcl.md#tcl_intro" class="olink">Loading Berkeley DB with Tcl</a> for information on sites from which you can download Tcl and which Tcl versions are compatible with Berkeley DB. To build Tcl, you must also build with shared libraries.

- **--with-uniquename=NAME**

  To build Berkeley DB with unique symbol names (in order to avoid conflicts with other application modules or libraries), enter --with-uniquename=NAME, replacing NAME with a string that to be appended to every Berkeley DB symbol. If "=NAME" is not specified, a default value of "\_MAJORMINOR" is used, where MAJORMINOR is the major and minor release numbers of the Berkeley DB release. See <a href="install_multiple.md" class="xref" title="Building with multiple versions of Berkeley DB">Building with multiple versions of Berkeley DB</a> for more information.
