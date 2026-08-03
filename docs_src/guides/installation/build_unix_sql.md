---
title: "Configuring the SQL Interface"
api-name: "Configuring the SQL Interface"
source: docs/installation/build_unix_sql.html
---
## Configuring the SQL Interface

<span class="sect2"> [Changing Compile Options](build_unix_sql.md#config_sql) </span>

<span class="sect2"> [Enabling Extensions](build_unix_sql.md#idp500824) </span>

<span class="sect2"> [Building the JDBC Driver](build_unix_sql.md#build_unix_jdbc) </span>

<span class="sect2"> [Using the JDBC Driver](build_unix_sql.md#idp571856) </span>

<span class="sect2"> [Building the ODBC Driver](build_unix_sql.md#idp593744) </span>

<span class="sect2"> [Building the BFILE extension](build_unix_sql.md#bfile) </span>

There are a set of options you can provide to <span class="command">**configure**</span> in order to control how the Berkeley DB SQL interface is built. These configuration options include:

<span class="term">--disable-log-checksum</span>  
Disables checksums in log records. This provides a boost to performance at the risk of log files having undetectable corruption that could prevent proper data recovery in case of database corruption.

Note that while this option is meant for use with the SQL interface, it will also disable checksum for the non-SQL interfaces.

<span class="term">--enable-sql</span>  
Causes the <span class="command">**dbsql**</span> command line interpreter to be built. Along with <span class="command">**dbsql**</span>, this argument also builds the libdb_sqlXX.{so\|la} library, a C API library that mirrors the SQLite C API.

<span class="term">--enable-sql_compat</span>  
Causes the <span class="command">**sqlite3**</span> command line tool to be built. This tool is identical to the <span class="command">**dbsql**</span> command line tool, except that it has the same name as the command line tool that comes with standard SQLite.

In addition, the libsqlite3.{so\|la} C API library is built if this option is specified. This library is identical to the libdb_sqlXX.{so\|la} library that is normally built for Berkeley DB's sql interface, except that it has the same name as the library which is built for standard SQLite.

### Warning

Use this compatibility option with <span class="emphasis">*extreme*</span> care. Standard SQLite is used by many programs and utilities on many different platforms. Some platforms, such as Mac OS X, come with standard SQLite built in because default applications for the platform use that library.

**Use of this option on platforms where standard SQLite is in production use can cause unexpected runtime errors either for your own application, or for applications and utilities commonly found on the platform, depending on which library is found first in the platform's library search path.**

Use this option <span class="emphasis">*only*</span> if you know exactly what you are doing.

This option is provided so that there is an easy upgrade path for legacy SQLite tools and scripts that want to use BDB SQL without rewriting the tool or script. However, data contained in standard SQLite databases must be manually migrated from the old database to your BDB SQL database even if you use this option. See the *Berkeley DB Getting Started with the SQL APIs* guide for information on migrating data from standard SQLite to BDB SQL databases.

Note that in addition to the renamed command line tool and library, this option also causes versions of the command line tool and library to be built that use the normal BDB SQLite names (<span class="command">**dbsql**</span> and libdb_sqlXX.{so\|la}).

<span class="term">--enable-test</span>  
Cause the Berkeley DB SQL interface test suite to be built. This argument can also be used with either `--enable-sql` or `--enable-sql_compat` to build the SQLite Tcl test runner.

<span class="term">--enable-jdbc</span>  
Causes the JDBC driver to be built. Setting this option implies that `--enable-sql` is set, which means that the Berkeley DB SQL API will be built too.

The following configuration options are useful when debugging applications:

<span class="term"> <a href="build_unix_conf.md#build_unix_conf.--enable-debug" class="link">--enable-debug</a> </span>  
Builds the Berkeley DB SQL interface with debug symbols.

<span class="term"> <a href="build_unix_conf.md#build_unix_conf.--enable-diagnostic" class="link">--enable-diagnostic</a> </span>  
Builds the Berkeley DB SQL interface with run-time debugging checks.

Any arguments that you can provide to the standard SQLite configure script can also be supplied when configuring Berkeley DB SQL interface.

### Changing Compile Options

There are several configuration options you can specify as an argument to the configure script using the standard environment variable, CFLAGS.

<span class="term">BDBSQL_DEFAULT_PAGE_SIZE</span>  
To set the default page size when you create a database, specify the BDBSQL_DEFAULT_PAGE_SIZE flag. The value assigned must be a 0, 512, 1024, 2048, 4096, 8192 16384, 32768, or 65536. The default value is 4096. If the value is set to zero, Berkeley DB queries the file system to determine the best page size, and the value of SQLITE_DEFAULT_PAGE_SIZE is used to calculate the cache size, as the cache size is specified as a number of pages.

<span class="term">BDBSQL_FILE_PER_TABLE</span>  
To generate each table in a separate file, rather than as subdatabases in a single file, specify the BDBSQL_FILE_PER_TABLE flag. When this option is enabled, the SQL database name is used as a directory name. This directory contains one file for the metadata and one file each for every table created by the SQL API. Note that adding or deleting files from the database directory may corrupt your database. To backup the metadata (schema), make a copy of the `metadata` and `table00001` files from the database directory. Make a new copy whenever the schema is changed.

<span class="term">BDBSQL_LOG_REGIONMAX</span>  
To configure the log region size for the underlying storage engine, specify the BDBSQL_LOG_REGIONMAX flag. For more information, see <a href="../../api/c/envget_lg_regionmax.md" class="olink">DB_ENV-&gt;get_lg_regionmax()</a>.

<span class="term">BDBSQL_OMIT_LEAKCHECK </span>  
For Berkeley DB to use the default system allocation routines rather than the SQLite allocation routines, specify the BDBSQL_OMIT_LEAKCHECK flag.

<span class="term">BDBSQL_OMIT_LOG_REMOVE </span>  
Berkeley DB automatically removes log files that are not required any more, that is, files that are older than the most recent checkpoint. To disable this functionality, specify the BDBSQL_OMIT_LOG_REMOVE flag. It is necessary to provide this flag if you are using replication with Berkeley DB SQL.

<span class="term">BDBSQL_OMIT_SHARING </span>  
To create a private environment rather than a shared environment, specify the BDBSQL_OMIT_SHARING flag. That is, the cache and other region files will be created in memory rather than using file backed shared memory. For more information, see the DB_PRIVATE flag of <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>.

<span class="term">BDBSQL_SINGLE_THREAD </span>  
To disable locking and thread safe connections, specify the BDBSQL_SINGLE_THREAD flag. If an application is going to use Berkeley DB from a single thread and a single process, enabling this flag can deliver significant performance advantages.

<span class="term">SQLITE_DEFAULT_CACHE_SIZE </span>  
SQLite provides an in-memory cache which you size according to the maximum number of database pages that you want to hold in memory at any given time. Berkeley DB's in-memory cache feature performs the same function as SQLite. To specify the suggested maximum number of pages of disk cache that will be allocated per open database file specify the SQLITE_DEFAULT_CACHE_SIZE flag. Default value is 2000 pages. For more information, see the SQLite documentation on <a href="http://www.sqlite.org/pragma.html#pragma_default_cache_size" class="ulink" target="_top">PRAGMA default_cache_size</a>.

<span class="term">SQLITE_DEFAULT_JOURNAL_SIZE_LIMIT </span>  
For SQLite, this pragma identifies the maximum size that the journal file is allowed to be. Berkeley DB does not have a journal file, but it writes and uses log files. A new log file is created when the current log file has reached the defined maximum size. To define this maximum size for a log file, specify the SQLITE_DEFAULT_JOURNAL_SIZE_LIMIT flag. Default value is 10 MB for the Berkeley DB SQL interface.

### Enabling Extensions

The Berkeley DB SQL API provides extensions such as full text search and R-Tree index. By default, these extensions are disabled. To enable an extension in the Berkeley DB SQL interface, specify the related option as an argument to the configure script using the standard environment variable, CPPFLAGS.

<span class="term">SQLITE_ENABLE_FTS3</span>  
Enable building the Berkeley DB full text search layer

<span class="term">SQLITE_ENABLE_RTREE</span>  
Enables the Berkeley DB R-Tree layer.

See the SQLite Documentation for more information on <a href="http://www.sqlite.org/fts3.html" class="ulink" target="_top">full text search</a> and <a href="http://www.sqlite.org/rtree.html" class="ulink" target="_top">R-Tree</a>.

### Building the JDBC Driver

This section describes how to build the JDBC driver code using `autoconf`, which is the only method supported and tested by the Berkeley DB team.

To build the JDBC driver, you must have Sun Java Development Kit 1.1 or above installed.

``` c
cd build_unix
    ../dist/configure --enable-jdbc --prefix=<install path>
    make install
```

You can test the build by entering the following commands from the `build_unix/jdbc` directory:

|                                                                  |
|------------------------------------------------------------------|
| javac -classpath ./sqlite.jar test3.java                         |
| java -Djava.library.path=./.libs -classpath ./sqlite.jar:. test3 |

### Using the JDBC Driver

This section describes how to download, build, and run sample programs using the built JDBC driver.

#### Downloading JDBC Sample Code

The download link for JDBC sample code is available on the <a href="http://www.oracle.com/technetwork/java/index-139949.html" class="ulink" target="_top">Oracle Technology Network (OTN)</a> page. You can identify the link by the "JDBC programming examples from all three editions (ZIP format)" text beside it.

#### Modifying Sample Code

Before running the example code, do the following:

1.  Unzip the file containing the sample code to a new directory (for example, jdbc_ex).

2.  Substitute `jdbc:sqlite:/<db-file-name>` for the generic JDBC URL that appears in the code. That is, put `jdbc:sqlite:/<db-file-name>` between the quotation marks in the line:

    `String url = "jdbc:mySubprotocol:myDataSource";`

    Note: The \<db-file-name\> can either be an absolute path name like `"/jdbc_ex_db/myDataSource"`, or a relative path-file-name like `"../jdbc_ex_db/myDataSource"`, or a file name, like `"myDataSource"`, in which case the database file will be stored at the current directory.

3.  Substitute `SQLite.JDBCDriver` for `myDriver.ClassName` in the line: `Class.forName("myDriver.ClassName");`

4.  Optionally substitute the username and password you use for your database in the following: `"myLogin", "myPassword"`.

5.  If your JDK version is above 1.5, change the variable name `enum` in `OutputApplet.java` to some other variable name because, as of JDK release 5 `enum` is a keyword and can not be used as an identifier.

#### Building and Running the JDBC Sample code

See <a href="build_unix_sql.md#build_unix_jdbc" class="xref" title="Building the JDBC Driver">Building the JDBC Driver</a> for instructions on building the JDBC driver.

To build and run the JDBC examples do the following:

1.  Copy `build_unix/jdbc/sqlite.jar` and `build_unix/jdbc/.libs/libsqlite_jni.so` to the `jdbc_ex` directory.

2.  In the `jdbc_ex` directory, run the following commands:

    ``` c
        $ javac -classpath ./sqlite.jar *.java
        $ java -classpath .:sqlite.jar -Djava.library.path=. \
        <ClassName, eg. CreateCoffees>
        
    ```

3.  After you run the CreateCoffees example, use the `dbsql` executable to open the `myDataSource` database file and check if the table `COFFEES` has been successfully created in the database.

    ``` c
        $ dbsql myDataSourcedbsql> .tables
        COFFEES
        dbsql> .dump
        PRAGMA foreign_keys=OFF;
        BEGIN TRANSACTION;
        CREATE TABLE COFFEES (COF_NAME varchar(32),\
        SUP_ID int, PRICE float, SALES int, TOTAL int);
        COMMIT;
        dbsql>
        
    ```

4.  Repeat step 3 to run other examples.

    Note: Some examples, such as AutoGenKeys, are not yet supported by BDB JDBC driver. The `SQLFeatureNotSupportedException` is displayed for those unsupported examples.

### Building the ODBC Driver

This section describes how to build the ODBC driver.

#### Configuring Your System

To configure your system prior to building the ODBC driver, do the following:

1.  Download and install the latest <a href="http://www.unixodbc.org" class="ulink" target="_top">unixODBC</a> if ODBC is not already installed on your system.
2.  Configure the ODBC server to work with SQLite databases. Follow <a href="http://www.ch-werner.de/sqliteodbc/html/index.html" class="ulink" target="_top">these instructions</a> from Christian Werner.

#### Building the Library

To build the library, do the following:

``` c
    $ cd db-5.3.XX/build_unix
    $ CFLAGS="-fPIC" ../dist/configure --enable-sql_compat --disable-shared
    $ make
    $ cd ../lang/sql/odbc
    $ CFLAGS="-DHAVE_ERRNO_H -I../../../build_unix -I../../../src/dbinc \
             -I../sqlite/src" LDFLAGS="../../../build_unix/libdb-5.3.a" \ 
             ./configure --with-sqlite3=../generated
    $ make
    
```

The `libsqlite3odbc.so` library containing a statically linked version of Berkeley DB SQL is now built.

NOTE: The final `make` command above is known to generate a warning when using GCC. The warning states: `Warning: Linking the shared library libsqlite3odbc.la against the static library ../../build_unix/libdb-5.3.a is not portable!`. It is generally safe to ignore the warning when using the generated library.

#### Testing the ODBC Driver

The steps to verify that the installed driver works are as follows:

1.  Alter the `/etc/odbcinst.ini` and `~/.odbc.ini` configuration files to refer to the libsqlite3odbc.so file built above.

2.  Create a data source, and launch a data source viewer application by doing the following:

    ``` c
    $ mkdir ~/databases
        $ cd ~/databases
        $ /path/to/Berkeley DB/build_unix/sqlite3 mytest.db
        dbsql> CREATE TABLE t1(x);
        dbsql> .quit;
        $ DataManager 
    ```

    The final step opens a GUI application that displays ODBC data sources on a system. You should be able to find the `mytest.db` data source just created.

### Building the BFILE extension

The BFILE extension allows you to store binary files outside of the database, but still operate upon them as if they were stored within the database. To enable this extension, use the `--enable-load-extension` configuration flag. For example:

``` c
$ cd <db>/build_unix
$ export DBSQL_DIR=$PWD/../install
$ ../dist/configure --enable-sql --enable-load-extension \
     --prefix=$DBSQL_DIR && make && make install
$ cd ../lang/sql/sqlite/ext/bfile/build
$ make && make install
```

BFILE extensions are only supported for Unix platforms.

Note that the extension support has two interfaces: SQL expressions and a C-functions API. By default, the SQL expressions are built when you use `--enable-load_extension`. To use the C-functions API, edit `<db>/lang/sql/ext/bfile/build/Makefile` and set `ENABLE_BFILE_CAPI` to `1`.

Once you have enabled the extension and built the library, you can run the included example:

``` c
$ cd lang/sql/sqlite/ext/bfile/build
$ export LD_LIBRARY_PATH=$PWD:$DBSQL_DIR/lib
$ ./bfile_example_sql     # for SQL expressions interface
$ ./bfile_example_capi    # for C-functions API
```

For more information on using the BFILE extension, see the *Berkeley DB Getting Started with the SQL APIs* guide.
