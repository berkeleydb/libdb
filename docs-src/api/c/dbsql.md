---
title: "dbsql"
api-name: "dbsql"
source: docs/api_reference/C/dbsql.html
---
## dbsql

``` c
dbsql [OPTIONS] FILENAME SQL 
```

`dbsql` is a command line tool that provides access to the Berkeley DB SQL interface.

To build this tool, run the configure script with the `--enable-sql `option when you are building the Berkeley DB SQL interface. For more information on building this tool, see <a href="../../installation/build_unix.html#build_unix_intro" class="olink">"Building for UNIX/POSIX"</a>.

FILENAME is the name of a Berkeley DB database file created with the SQL interface. A new database is created if the file does not exist. The options are as follows:

- **-init filename**

  Reads/processes named file.

- **-echo**

  Prints commands before execution.

- **-\[no\]header**

  Turns headers on or off.

- **-bail**

  Stops after hitting an error.

- **-interactive**

  Forces interactive I/O.

- **-batch**

  Forces batch I/O.

- **-column**

  Sets output mode to column.

- **-csv**

  Sets output mode to csv.

- **-html**

  Sets output mode to HTML.

- **-line**

  Sets output mode to line.

- **-list**

  Sets output mode to list.

- **-separator 'x'**

  Sets output field separator (\|).

- **-nullvalue 'text'**

  Sets text string for NULL values.

- **-version**

  Shows SQLite version.

The `dbsql` executable provides the same interface as the `sqlite3` executable that is part of SQLite. For more information on how to use dbsql see the <a href="http://www.sqlite.org/sqlite.html" class="ulink" target="_top">SQLite Documentation page.</a>

### Command Line Features Unique to dbsql

This section describes pre-defined query statements that can be executed from the `dbsql` command line. These queries take the form of:

``` c
 .stat ITEM
```

where `ITEM` is an optional parameter that indicates what statistics to print. If `ITEM` is not specified, then this command prints statistics for the Berkeley DB environment, followed by statistics for all tables and indexes within the database.

If `ITEM` is the name of a table or index, then this command prints statistics for the table or index using the <a href="dbstat_print.md" class="xref" title="DB-&gt;stat_print()">DB-&gt;stat_print()</a> method.

Otherwise, `ITEM` can be one of several keywords. They are:

- `:env:`

  ``` c
  dbsql> .stat :env:
  ```

  Causes this command to print statistics for the Berkeley DB environment using the <a href="envstat.md" class="xref" title="DB_ENV-&gt;stat_print()">DB_ENV-&gt;stat_print()</a>. method.

- `:rep:`

  ``` c
  dbsql> .stat :rep:
  ```

  Causes this command to print a summary of replication statistics.
