---
title: "Data Migration"
api-name: "Data Migration"
source: docs/bdb-sql/datamigration.html
---
## Data Migration

<span class="sect2"> [Migration Using the Shells](datamigration.md#shellmigrate) </span>

If you have a database created by SQLite, you can migrate it to a Berkeley DB database for use with the BDB SQL interface. For production applications, you should do this only when your application is shutdown.

All data and schema supported by SQLite can be migrated to a Berkeley DB database.

### Migration Using the Shells

To migrate your data from SQLite to a Berkeley DB database:

1.  Make sure your application is shutdown.

2.  Open the SQLite database within the <span class="command">**sqlite3**</span> shell.

3.  Execute the `.output` command to specify the location where you want to dump data.

4.  Dump the database using the SQLite `.dump` command.

5.  Close the <span class="command">**sqlite3**</span> shell and open the Berkeley DB <a href="../../api/c/dbsql.md" class="olink">dbsql</a> shell.

6.  Load the dumped data using the `.read` command.

Note that you can migrate in the reverse direction as well. Dump the Berkeley DB database by calling `.dump` from within the Berkeley DB <a href="../../api/c/dbsql.md" class="olink">dbsql</a> shell, and load it into SQLite by `.read` from within SQLite's <span class="command">**sqlite3**</span> shell.
