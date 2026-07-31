---
title: "Syncing with Oracle Databases"
api-name: "Syncing with Oracle Databases"
source: docs/bdb-sql/sync.html
---
## Syncing with Oracle Databases

<span class="sect2"> [Syncing on Unix Platforms](sync.md#syncunix) </span>

<span class="sect2"> [Syncing on Windows Platforms](sync.md#syncwin) </span>

<span class="sect2"> [Syncing on Windows Mobile Platforms](sync.md#syncwinmobile) </span>

Oracle's SQLite Mobile Client product allows you to synchronize a SQLite database with a back-end Oracle database. Because the BDB SQL interface is a drop-in replacement for SQLite, this means you can synchronize a Berkeley DB database with an Oracle back-end as well.

### Note

Berkeley DB SQL databases are not compatible with SQLite databases. In order for sync to work, you must remove any currently existing SQLite databases.

### Syncing on Unix Platforms

For Unix platforms, the easiest way to use Oracle's SQLite Mobile Client is to build the BDB SQL interface with the compatibility option. That is, specify both `--enable-sql` and `--enable-sql-compat` when you configure your Berkeley DB installation. This causes libraries with the exact same name as the SQLite libraries to be created when you build Berkeley DB.

Having done that, you must then change your platform's library search path so that it finds the Berkeley DB libraries <span class="emphasis">*before*</span> any installed SQLite libraries. On many (but not all) Unix platforms, you do this by modifying the `LD_LIBRARY_PATH` environment variable. See your operating system documentation for information on how to change your search path for dynamically linked libraries.

Once you have properly configured and built your Berkeley DB installation, and you have properly configured your operating system, you can use the Oracle SQLite Mobile Client in exactly the same way as you would if you were using standard SQLite libraries and databases with it. See the <a href="http://download.oracle.com/docs/cd/E12095_01/nav/portal_booklist.htm" class="ulink" target="_top">Oracle Database Lite</a> documentation for information on using SQLite Mobile Client.

For information on building the BDB SQL interface, see the <a href="../../guides/installation/build_unix_sql.md" class="olink">Configuring the SQL Interface</a> section in the *Berkeley DB Installation and Build Guide*.

### Syncing on Windows Platforms

For Windows platforms, you use Oracle's SQLite Mobile Client by building the BDB SQL interface in the same way as you normally do. See the <a href="../../guides/installation/build_win.md" class="olink">Building Berkeley DB for Windows</a> chapter in the *Berkeley DB Installation and Build Guide* for more information.

Once you have built the product, rename the Berkeley DB SQL dlls so that they are named identically to the standard SQLite dlls (sqlite3.dll). Install the renamed Berkeley DB SQL dll along with the main Berkeley DB dll (libdb5x.dll) in the same directory as the SQLite dlls. See the <a href="../../guides/installation/build_win_sql.md" class="olink">Building the SQL API</a> section for details.

Finally, configure your Windows PATH environment variable so that it finds your Berkeley DB dlls before it finds any standard SQLite dlls that might be installed on your system.

Once you have built your Berkeley DB installation and renamed your dlls, and you have properly configured your operating system, you can use the Oracle SQLite Mobile Client in exactly the same way as you would if you were using standard SQLite libraries and databases with it. See the <a href="http://download.oracle.com/docs/cd/E12095_01/nav/portal_booklist.htm" class="ulink" target="_top">Oracle Database Lite</a> documentation for information on using SQLite Mobile Client.

### Syncing on Windows Mobile Platforms

For Windows Mobile platforms, you use Oracle's SQLite Mobile Client by building the BDB SQL interface in the same way as you normally do. See the <a href="../../guides/installation/build_wince.md" class="olink">Building Berkeley DB for Windows Mobile</a> chapter in the *Berkeley DB Installation and Build Guide* for more information.

Once you have built the product, rename the Berkeley DB SQL dll to `sqlite3.dll`. Then, copy the dll to the `\Windows` path on the phone. Note that you only need the new `sqlite3.dll`; you do not need any of the other Berkeley DB dlls.

Once you have built your Berkeley DB installation and renamed your dlls, and you have properly configured your operating system, you can use the Oracle SQLite Mobile Client in exactly the same way as you would if you were using standard SQLite libraries and databases with it. See the <a href="http://download.oracle.com/docs/cd/E12095_01/nav/portal_booklist.htm" class="ulink" target="_top">Oracle Database Lite</a> documentation for information on using SQLite Mobile Client.
