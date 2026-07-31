---
title: "db_dump"
api-name: "db_dump"
source: docs/api_reference/C/db_dump.html
---
## db_dump

``` c
db_dump [-klNpRrV] [-b blob_dir] [-d ahr]
    [-f output] [-h home] [-P password] [-s database] [-D bytes] file

db_dump [-kNpV] [-d ahr] [-f output] [-h home] -m database

db_dump185 [-p] [-f output] file  
```

The <span class="command">**db_dump**</span> utility reads the database file **file** and writes it to the standard output using a portable flat-text format understood by the <a href="db_load.md" class="xref" title="db_load">db_load</a> utility. The **file** argument must be a file produced using the Berkeley DB library functions.

The <span class="command">**db_dump185**</span> utility is similar to the <span class="command">**db_dump**</span> utility, except that it reads databases in the format used by Berkeley DB versions 1.85 and 1.86.

The options are as follows:

- **-b**

  Specifies the directory where BLOB data is stored for the database you are dumping.

- **-d**

  Dump the specified database in a format helpful for debugging the Berkeley DB library routines.

  - **a**

    Display all information. See also the **-D** option.

  - **h**

    Display only page headers.

  - **r**

    Do not display the free-list or pages on the free list. This mode is used by the recovery tests.

  **The output format of the ****-d** option is not standard and may change, without notice, between releases of the Berkeley DB library.****

- **-D**

  Specifies the maximum number of bytes to dump for each key/data item found in the specified database. This option is only valid when **-da** is also specified. This option overrides the value set for the "set_data_len" parameter in your DB_CONFIG file, if any.

- **-f**

  Write to the specified file instead of to the standard output.

- **-h**

  Specify a home directory for the database environment; by default, the current working directory is used.

- **-k**

  Dump record numbers from Queue and Recno databases as keys.

- **-l**

  List the databases stored in the file.

- **-m**

  Specify a named in-memory database to dump. In this case the **file** argument must be omitted.

- **-N**

  Do not acquire shared region mutexes while running. Other problems, such as potentially fatal errors in Berkeley DB, will be ignored as well. This option is intended only for debugging errors, and should not be used under any other circumstances.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-p**

  If characters in either the key or data items are printing characters (as defined by **isprint**(3)), use printing characters in **file** to represent them. This option permits users to use standard text editors and tools to modify the contents of databases.

  Note: different systems may have different notions about what characters are considered <span class="emphasis">*printing characters*</span>, and databases dumped in this manner may be less portable to external systems.

- **-R**

  Aggressively salvage data from a possibly corrupt file. The **-R** flag differs from the **-r** option in that it will return all possible data from the file at the risk of also returning already deleted or otherwise nonsensical items. Data dumped in this fashion will almost certainly have to be edited by hand or other means before the data is ready for reload into another database

  Note that this option causes the utility to verify the integrity of the database before performing the database dump. If this verification fails, the utility will exit with error return `DB_VERIFY_BAD` even though the database is successfully dumped. If you are dumping a database known to be corrupt, you can safely ignore a `DB_VERIFY_BAD` error return.

- **-r**

  Salvage data from a possibly corrupt file. When used on a uncorrupted database, this option should return equivalent data to a normal dump, but most likely in a different order.

  Note that this option causes the utility to verify the integrity of the database before performing the database dump. If this verification fails, the utility will exit with error return `DB_VERIFY_BAD` even though the database is successfully dumped. If you are dumping a database known to be corrupt, you can safely ignore a `DB_VERIFY_BAD` error return.

- **-s**

  Specify a single database to dump. If no database is specified, all databases in the database file are dumped.

- **-V**

  Write the library version number to the standard output, and exit.

Dumping and reloading Hash databases that use user-defined hash functions will result in new databases that use the default hash function. Although using the default hash function may not be optimal for the new database, it will continue to work correctly.

Dumping and reloading Btree databases that use user-defined prefix or comparison functions will result in new databases that use the default prefix and comparison functions. **In this case, it is quite likely that the database will be damaged beyond repair permitting neither record storage or retrieval.**

The only available workaround for either case is to modify the sources for the <a href="db_load.md" class="xref" title="db_load">db_load</a> utility to load the database using the correct hash, prefix, and comparison functions.

The <span class="command">**db_dump185**</span> utility may not be available on your system because it is not always built when the Berkeley DB libraries and utilities are installed. If you are unable to find it, see your system administrator for further information.

The <span class="command">**db_dump**</span> and <span class="command">**db_dump185**</span> utility output formats are documented in the <a href="../../programmer_reference/dumpload_format.html" class="olink">Dump Output Formats</a> section of the Berkeley DB Reference Guide.

The <span class="command">**db_dump**</span> utility may be used with a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_dump**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_dump**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

Even when using a Berkeley DB database environment, the db_dump utility does not use any kind of database locking if it is invoked with the **-d**, **-R**, or **-r** arguments. If used with one of these arguments, the <span class="command">**db_dump**</span> utility may only be safely run on databases that are not being modified by any other process; otherwise, the output may be corrupt.

The <span class="command">**db_dump**</span> utility exits 0 on success, and \>0 if an error occurs. Note that this utility might return `DB_VERIFY_BAD` if the `-R` or `-r` command line options are used. This indicates a corrupt database. However, the dump may still have been successful.

The <span class="command">**db_dump185**</span> utility exits 0 on success, and \>0 if an error occurs.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
