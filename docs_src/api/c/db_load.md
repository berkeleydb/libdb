---
title: "db_load"
api-name: "db_load"
source: docs/api_reference/C/db_load.html
---
## db_load

``` c
db_load [-nTV] [-b blob_dir] [-c name=value] [-f file]
    [-h home] [-P password] [-o blob_threshold] 
    [-t btree | hash | queue | recno] file

db_load [-r lsn | fileid] [-h home] [-P password] file  
```

The <span class="command">**db_load**</span> utility reads from the standard input and loads it into the database **file**. The database **file** is created if it does not already exist.

The input to <span class="command">**db_load**</span> must be in the output format specified by the <a href="db_dump.md" class="xref" title="db_dump">db_dump</a> utility or as specified by the **-T** option below.

The options are as follows:

- **-b**

  Identifies the directory where BLOB data is stored. If this option is not specified, then BLOB data is placed in a subdirectory within the DB's environment. See also the **-o** option.

- **-c**

  Specify configuration options ignoring any value they may have based on the input. The command-line format is **name=value**. See the Supported Keywords section below for a list of keywords supported by the **-c** option.

- **-f**

  Read from the specified **input** file instead of from the standard input.

- **-h**

  Specify a home directory for the database environment.

  If a home directory is specified, the database environment is opened using the <a href="envopen.md#envopen_DB_INIT_LOCK" class="link">DB_INIT_LOCK</a>, <a href="envopen.md#envopen_DB_INIT_LOG" class="link">DB_INIT_LOG</a>, <a href="envopen.md#envopen_DB_INIT_MPOOL" class="link">DB_INIT_MPOOL</a>, <a href="envopen.md#envopen_DB_INIT_TXN" class="link">DB_INIT_TXN</a>, and <a href="envopen.md#envopen_DB_USE_ENVIRON" class="link">DB_USE_ENVIRON</a> flags to <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> (This means that <span class="command">**db_load**</span> can be used to load data into databases while they are in use by other processes.) If the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> call fails, or if no home directory is specified, the database is still updated, but the environment is ignored; for example, no locking is done.

- **-n**

  Do not overwrite existing keys in the database when loading into an already existing database. If a key/data pair cannot be loaded into the database for this reason, a warning message is displayed on the standard error output, and the key/data pair are skipped.

- **-o**

  Identifies the BLOB threshold in bytes. This threshold determines when a data item will be stored as a BLOB. Data items sized less than this threshold are stored as normal data within the database. Data items larger than this size are stored on-disk in a subdirectory set aside for the purpose. Use the **-b** command line option to identify where BLOB data is stored.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-r**

  Reset the database's file ID or log sequence numbers (LSNs).

  All database pages in transactional environments contain references to the environment's log records. In order to copy a database into a different database environment, database page references to the old environment's log records must be reset, otherwise data corruption can occur when the database is modified in the new environment. The **-r** **lsn** option resets a database's log sequence numbers.

  All databases contain an ID string used to identify the database in the database environment cache. If a database is copied, and used in the same environment as another file with the same ID string, corruption can occur. The **-r** **fileid** option resets a database's file ID to a new value.

  **In both cases, the physical file specified by the ****file** argument is modified in-place.****

- **-T**

  The **-T** option allows non-Berkeley DB applications to easily load text files into databases.

  If the database to be created is of type Btree or Hash, or the keyword **keys** is specified as set, the input must be paired lines of text, where the first line of the pair is the key item, and the second line of the pair is its corresponding data item. If the database to be created is of type Queue or Recno and the keyword **keys** is not set, the input must be lines of text, where each line is a new data item for the database.

  A simple escape mechanism, where newline and backslash (\\ characters are special, is applied to the text input. Newline characters are interpreted as record separators. Backslash characters in the text will be interpreted in one of two ways: If the backslash character precedes another backslash character, the pair will be interpreted as a literal backslash. If the backslash character precedes any other character, the two characters following the backslash will be interpreted as a hexadecimal specification of a single character; for example, \0a is a newline character in the ASCII character set.

  For this reason, any backslash or newline characters that naturally occur in the text input must be escaped to avoid misinterpretation by <span class="command">**db_load**</span>.

  If the **-T** option is specified, the underlying access method type must be specified using the **-t** option.

- **-t**

  Specify the underlying access method. If no **-t** option is specified, the database will be loaded into a database of the same type as was dumped; for example, a Hash database will be created if a Hash database was dumped.

  Btree and Hash databases may be converted from one to the other. Queue and Recno databases may be converted from one to the other. If the **-k** option was specified on the call to <a href="db_dump.md" class="xref" title="db_dump">db_dump</a> then Queue and Recno databases may be converted to Btree or Hash, with the key being the integer record number.

- **-V**

  Write the library version number to the standard output, and exit.

The <span class="command">**db_load**</span> utility may be used with a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_load**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_load**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <span class="command">**db_load**</span> utility exits 0 on success, 1 if one or more key/data pairs were not loaded into the database because the key already existed, and \>1 if an error occurs.

### Examples

The <span class="command">**db_load**</span> utility can be used to load text files into databases. For example, the following command loads the standard UNIX <span class="emphasis">*/etc/passwd*</span> file into a database, with the login name as the key item and the entire password entry as the data item:

``` c
  awk -F: '{print $1; print $0}' < /etc/passwd |    
         sed 's/\\/\\\\/g' | db_load -T -t hash passwd.db  
```

Note that backslash characters naturally occurring in the text are escaped to avoid interpretation as escape characters by <span class="command">**db_load**</span>.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.

### Supported Keywords

The following keywords are supported for the **-c** command-line option to the <span class="command">**db_load**</span> utility. See the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method for further discussion of these keywords and what values should be specified.

The parenthetical listing specifies how the value part of the **name=value** pair is interpreted. Items listed as (boolean) expect value to be **1** (set) or **0** (unset). Items listed as (number) convert value to a number. Items listed as (string) use the string value without modification.

- **bt_minkey (number)**

  The minimum number of keys per page.

- **chksum (boolean)**

  Enable page checksums.

- **database (string)**

  The database to load.

- **db_lorder (number)**

  The byte order for integers in the stored database metadata. For big endian systems, the order should be 4,321 while for little endian systems is should be 1,234.

- **db_pagesize (number)**

  The size of database pages, in bytes.

- **duplicates (boolean)**

  The value of the <a href="dbset_flags.md#dbset_flags_DB_DUP" class="link">DB_DUP</a> flag.

- **dupsort (boolean)**

  The value of the <a href="dbset_flags.md#dbset_flags_DB_DUPSORT" class="link">DB_DUPSORT</a> flag.

- **extentsize (number)**

  The size of database extents, in pages, for Queue databases configured to use extents.

- **h_ffactor (number)**

  The density within the Hash database.

- **h_nelem (number)**

  The size of the Hash database.

- **keys (boolean)**

  Specify whether keys are present for Queue or Recno databases.

- **re_len (number)**

  Specify the length for fixed-length records. This number represents different things, depending on the access method the database is using. See the <a href="dbset_re_len.md" class="xref" title="DB-&gt;set_re_len()">DB-&gt;set_re_len()</a> method for details on what this number represents.

- **re_pad (string)**

  Specify the fixed-length record pad character.

- **recnum (boolean)**

  The value of the <a href="dbset_flags.md#dbset_flags_DB_RECNUM" class="link">DB_RECNUM</a> flag.

- **renumber (boolean)**

  The value of the <a href="dbset_flags.md#dbset_flags_DB_RECNUM" class="link">DB_RENUMBER</a> flag.

- **subdatabase (string)**

  The subdatabase to load.
