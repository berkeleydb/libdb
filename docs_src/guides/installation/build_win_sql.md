---
title: "Building the SQL API"
api-name: "Building the SQL API"
source: docs/installation/build_win_sql.html
---
## Building the SQL API

<span class="sect2"> [Binary Compatibility With SQLite](build_win_sql.md#idp290248) </span>

<span class="sect2"> [Setting Preprocessor Flags](build_win_sql.md#idp276576) </span>

<span class="sect2"> [Enabling Extensions](build_win_sql.md#idp288280) </span>

<span class="sect2"> [Disabling Log Checksums](build_win_sql.md#win-disablechecksums) </span>

<span class="sect2"> [Building the JDBC Driver](build_win_sql.md#build_jdbc) </span>

<span class="sect2"> [Using the JDBC Driver](build_win_sql.md#idp266616) </span>

<span class="sect2"> [Building the ODBC Driver](build_win_sql.md#idp305704) </span>

<span class="sect2"> [Using the ADO.NET Driver](build_win_sql.md#idp320888) </span>

SQL support is built as part of the default build on Windows. For information on the build instructions, see <a href="build_win.md" class="xref" title="Chapter 5.  Building Berkeley DB for Windows">Building Berkeley DB for Windows</a> .

The SQL library is built as `libdb_sql53.dll` in the Release mode or `libdb_sql53d.dll` in the Debug mode. An SQL command line interpreter called `dbsql.exe` is also built.

### Binary Compatibility With SQLite

`libdb_sql53.dll` is compatible with `sqlite3.dll`. You can copy `libdb_sql53.dll` to `sqlite3.dll` and `dbsql.exe` to `sqlite3.exe`, and use these applications as a replacement for the standard SQLite binaries with the same names. However, if you want to do this, then any legacy data in use by those tools must be migrated from the standard SQLite database to a Berkeley DB SQL database <span class="emphasis">*before*</span> you replace the standard SQLite dll and executable with the Berkeley DB equivalent. For information on migrating data from standard SQLite databases to a Berkeley DB SQL database, see the *Berkeley DB Getting Started with the SQL APIs* guide.

### Warning

Rename your dlls and executables to the standard SQLite names with <span class="emphasis">*extreme*</span> care. Doing this will cause all existing tools to break that currently have data stored in a standard SQLite database.

<span class="emphasis"> *For best results, rename your dlls and command line tool to use the standard SQLite names only if you know there are no other tools on your production platform that rely on standard SQLite.* </span>

### Setting Preprocessor Flags

By default, Berkeley DB SQL generates each table as a subdatabase in a single file. To generate each table in a separate file, specify <span class="emphasis">*BDBSQL_FILE_PER_TABLE*</span> in <span class="emphasis">*Preprocessor Definitions* </span>of the `db_sql` project.

When this option is enabled, the SQL database name is used as a directory name. This directory contains one file for the metadata and one file each for every table created by the SQL API. Do not add or delete files from the database directory. Adding or deleting files may corrupt the database. To backup just the metadata (schema), make a copy of the `metadata` and `table00001` files from the database directory. Make a new copy whenever the schema is changed.

### Enabling Extensions

The Berkeley DB SQL API provides extensions such as full text search and R-Tree index. To enable these extensions, do the following:

1.  Open the Berkeley DB solution in Visual Studio.
2.  Specify <span class="emphasis">*SQLITE_ENABLE_FTS3*</span> or <span class="emphasis">*SQLITE_ENABLE_RTREE*</span> in <span class="emphasis">*Preprocessor Definitions*</span> of the `db_sql` project.
3.  Re-build the `db_sql` project.

See the SQLite Documentation for more information on <a href="http://www.sqlite.org/fts3.html" class="ulink" target="_top">full text search</a> and <a href="http://www.sqlite.org/rtree.html" class="ulink" target="_top">R-Tree</a>.

### Disabling Log Checksums

You can disable checksums in log records so as to provide a boost to database performance. However, this comes at the risk of having undetectable log file corruption that could prevent data recovery in the event of database corruption.

### Note

Note that disabling log record checksums is meant to only be used with the SQL interface. However, disabling checksums for the SQL interface also disables checksums for the non-SQL interfaces.

To disable log checksums, before you build the library edit the `build_windows/db_config.h` file, and delete the following line:

``` c
#define HAVE_LOG_CHECKSUM 1
```

### Building the JDBC Driver

This section describes the steps to build the JDBC driver.

1.  Configure your build environment. For information on how to configure to build Java applications, see <a href="build_win_java.md" class="xref" title="Building the Java API">Building the Java API</a>.

2.  Build the SQL project in Debug mode.

3.  Open Berkeley_DB.sln or Berkeley_DB_vs2010.sln in Visual Studio. If you are using Java 1.6, do the following:

    - In the Solution Explorer, right-click the `db_sql_jdbc` project and select <span class="emphasis">*properties*</span>.

    - In the <span class="emphasis">*Configuration Properties -\> Build Events -\> Pre-Build Event*</span> section, alter the command to refer to `JDBC2z` instead of `JDBC2x`.

    If you are using Java 1.7, do the following:

    - In the Solution Explorer, right-click the `db_sql_jdbc` project and select <span class="emphasis">*properties*</span>.

    - In the <span class="emphasis">*Configuration Properties -\> Build Events -\> Pre-Build Event*</span> section, alter the command to refer to `JDBC2z1` instead of `JDBC2x`. Also, remove the option of "-target 1.5".

    - Go to `db\lang\sql\jdbc\SQLite`, and replace `JDBCDriver.java` with `JDBCDriver.java17`, and replace `JDBCDataSource.java` with `JDBCDataSource.java17`.

4.  Build the `db_sql_jdbc` project in Visual Studio.

You can test the build by entering the following commands from the `db\build_windows\Win32\Debug` directory:

|                                                          |
|----------------------------------------------------------|
| javac -cp ".;jdbc.jar" -d . ..\\.\\.\sql\jdbc\test3.java |
| java -cp ".;jdbc.jar" test3                              |

### Using the JDBC Driver

This section describes the steps to download, build, and run sample programs using the built JDBC driver.

#### Downloading JDBC Sample Code

The download link for JDBC sample code is available on the <a href="http://www.oracle.com/technetwork/java/index-139949.html" class="ulink" target="_top">Oracle Technology Network (OTN)</a> . You can identify the link by the "JDBC programming examples from all three editions (ZIP format)" text beside it.

#### Modifying Sample Code

Before running the sample code, do the following:

1.  Unzip the file containing the sample code to a new directory (for example, jdbc_ex).

2.  Substitute `jdbc:sqlite:/<db-file-name>` for the generic JDBC URL that appears in the code. That is, put `jdbc:sqlite:/<db-file-name>` between the quotation marks in the line:

    ` String url = "jdbc:mySubprotocol:myDataSource";`

    Note: The \<db-file-name\> can either be an absolute path name like `"D:\\jdbc_ex_db\\myDataSource"`, or a relative path-file-name like `"..\\jdbc_ex_db\myDataSource"`, or a file name, like `"myDataSource"`, in which the database file will be stored at the current directory.

3.  Substitute `SQLite.JDBCDriver` for `myDriver.ClassName` in the line: `Class.forName("myDriver.ClassName");`

4.  Substitute the username and password you use for your database in the following: `"myLogin", "myPassword"`.

    This is optional.

5.  If your JDK version is above 1.5, change the variable name `enum` in `OutputApplet.java` to some other variable name because, as of JDK release 5 `enum` is a keyword and can not be used as an identifier.

#### Building and Running the JDBC Sample code

See <a href="build_win_sql.md#build_jdbc" class="xref" title="Building the JDBC Driver">Building the JDBC Driver</a> for instructions about building JDBC driver.

To build and run the JDBC examples do the following:

1.  In the `db\build_windows\Win32\Debug` directory, run following commands:

    ``` c
        $ javac -classpath ".;jdbc.jar" -d . \path\to\jdbc_ex\*.java
        $ java  -classpath ".;jdbc.jar" <ClassName, eg. CreateCoffees>
        
    ```

2.  After you run the CreateCoffees example, use the `dbsql` executable to open the `myDataSource` database file and check if the table `COFFEES` has been successfully created in the database.

    ``` c
        $ dbsql myDataSourcedbsql> .tables
        COFFEES
        dbsql> .dump
        PRAGMA foreign_keys=OFF;
        BEGIN TRANSACTION;
        CREATE TABLE COFFEES (COF_NAME varchar(32),/
        SUP_ID int, PRICE float, SALES int, TOTAL int);
        COMMIT;
        dbsql>
        
    ```

3.  Repeat step 2 to run other examples.

    Note: Some examples like AutoGenKeys are not yet supported by BDB JDBC driver. The `SQLFeatureNotSupportedException` is displayed for those unsupported examples.

### Building the ODBC Driver

This section describes the steps required to build the ODBC driver.

#### Configuring Your System

To configure your system prior to building the ODBC driver, do the following:

1.  Download and install the latest SQLite ODBC driver Windows installer package for <a href="http://www.ch-werner.de/sqliteodbc/sqliteodbc.exe" class="ulink" target="_top">32 bit Windows</a> or <a href="http://www.ch-werner.de/sqliteodbc/sqliteodbc_w64.exe" class="ulink" target="_top">64 bit Windows</a>.
2.  Download and install the latest <a href="http://www.microsoft.com/download/en/details.aspx?id=21995" class="ulink" target="_top">Microsoft Data Access Components (MDAC) SDK</a> . The MDAC SDK is only required for testing the installation.

#### Building the Library

1.  Build the SQL project in Release mode. See <a href="build_win_sql.md" class="xref" title="Building the SQL API">Building the SQL API</a>.
2.  Open Visual Studio.
3.  Load the Berkeley_DB solution file into Visual Studio.
4.  Set the build target to <span class="emphasis">*Release*</span>
5.  Build the solution.
6.  Select <span class="emphasis">*File*</span> -\> <span class="emphasis">*Add*</span> -\> <span class="emphasis">*Existing Project*</span>.
7.  Select `build_windows`.
8.  Select the appropriate directory for your compiler: `VS8` or `VS10`.
9.  Select `db_sql_odbc.vcproj` and add it to the Berkeley_DB solution. This adds the `db_sql_odbc` Visual Studio project to the Berkeley_DB solution file.
10. Build the `db_sql_odbc` project. This can be done by right-clicking the `db_sql_odbc` project in the project explorer panel, and selecting `build`.

The `sqlite3odbc.dll`, `libdb_sql53.dll` and `libdb53.dll` files are now built.

#### Installing the Library

Copy the dll files built in the <span class="emphasis">*Building the Library*</span> section to the Windows system folder.

The Windows system folder is different on different systems, but is often `C:\WINDOWS\System32`.

#### Testing the ODBC Install

The steps to verify that the installed driver works are as follows:

1.  Open the Unicode ODBCTest application. On Windows XP: <span class="emphasis">*Windows start*</span> -\> <span class="emphasis">*Microsoft Data Access SDK 2.8*</span> -\> <span class="emphasis">*ODBCTest (Unicode, x86).*</span>
2.  Select the <span class="emphasis">*Conn*</span> -\> <span class="emphasis">*Full Connect...*</span> menu item.
3.  Select `SQLite3 Datasource` and click `OK`.
4.  Select the <span class="emphasis">*Stmt*</span> -\> <span class="emphasis">*SQLExecDirect...*</span> menu item.
5.  Enter `CREATE TABLE t1(x);` in the `Statement` text box and click `OK`.
6.  Verify that no error messages were output to the error window.

### Using the ADO.NET Driver

Go to the <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html" class="ulink" target="_top">Oracle Berkeley DB download page</a>, and download the ADO.NET package. Build the package by following the <a href="../../guides/bdb-sql/buildinstall.md#ado_net" class="olink">instructions</a> included in the package.
