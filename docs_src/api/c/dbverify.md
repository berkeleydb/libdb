---
title: "DB->verify()"
api-name: "DB->verify()"
source: docs/api_reference/C/dbverify.html
---
## DB-\>verify()

``` c
#include <db.h>

int
DB->verify(DB *db, const char *file,
    const char *database, FILE *outfile, u_int32_t flags);  
```

The `DB->verify()` method verifies the integrity of all databases in the file specified by the **file** parameter, and optionally outputs the databases' key/data pairs to the file stream specified by the **outfile** parameter.

**The `DB->verify()` method does not perform any locking, even in Berkeley DB environments that are configured with a locking subsystem. As such, it should only be used on files that are not being modified by another thread of control.**

The `DB->verify()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle may not be accessed again after `DB->verify()` is called, regardless of its return.

The `DB->verify()` method is the underlying method used by the <a href="db_verify.md" class="link" title="db_verify">db_verify</a> utility. See the <a href="db_verify.md" class="link" title="db_verify">db_verify</a> utility source code for an example of using `DB->verify()` in a IEEE/ANSI Std 1003.1 (POSIX) environment.

The `DB->verify()` method will return DB_VERIFY_BAD if a database is corrupted. When the DB_SALVAGE flag is specified, the DB_VERIFY_BAD return means that all key/data pairs in the file may not have been successfully output. Unless otherwise specified, the `DB->verify()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### file

The **file** parameter is the physical file in which the databases to be verified are found.

#### database

The **database** parameter is the database in **file** on which the database checks for btree and duplicate sort order and for hashing are to be performed. See the DB_ORDERCHKONLY flag for more information.

The database parameter must be set to NULL except when the DB_ORDERCHKONLY flag is set.

#### outfile

The **outfile** parameter is an optional file stream to which the databases' key/data pairs are written.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_SALVAGE`

  Write the key/data pairs from all databases in the file to the file stream named in the **outfile** parameter. Key values are written for Btree, Hash and Queue databases, but not for Recno databases.

  The output format is the same as that specified for the <a href="db_dump.md" class="link" title="db_dump">db_dump</a> utility, and can be used as input for the <a href="db_load.md" class="link" title="db_load">db_load</a> utility.

  Because the key/data pairs are output in page order as opposed to the sort order used by <a href="db_dump.md" class="link" title="db_dump">db_dump</a>, using `DB->verify()` to dump key/data pairs normally produces less than optimal loads for Btree databases.

In addition, the following flags may be set by bitwise inclusively **OR**'ing them into the **flags** parameter:

- `DB_AGGRESSIVE`

  Output **all** the key/data pairs in the file that can be found. By default, `DB->verify()` does not assume corruption. For example, if a key/data pair on a page is marked as deleted, it is not then written to the output file. When DB_AGGRESSIVE is specified, corruption is assumed, and any key/data pair that can be found is written. In this case, key/data pairs that are corrupted or have been deleted may appear in the output (even if the file being salvaged is in no way corrupt), and the output will almost certainly require editing before being loaded into a database.

- `DB_PRINTABLE`

  When using the DB_SALVAGE flag, if characters in either the key or data items are printing characters (as defined by **isprint**(3)), use printing characters to represent them. This flag permits users to use standard text editors and tools to modify the contents of databases or selectively remove data from salvager output.

  Note: different systems may have different notions about what characters are considered <span class="emphasis">*printing characters*</span>, and databases dumped in this manner may be less portable to external systems.

- `DB_NOORDERCHK`

  Skip the database checks for btree and duplicate sort order and for hashing.

  The `DB->verify()` method normally verifies that btree keys and duplicate items are correctly sorted, and hash keys are correctly hashed. If the file being verified contains multiple databases using differing sorting or hashing algorithms, some of them must necessarily fail database verification because only one sort order or hash function can be specified before `DB->verify()` is called. To verify files with multiple databases having differing sorting orders or hashing functions, first perform verification of the file as a whole by using the DB_NOORDERCHK flag, and then individually verify the sort order and hashing function for each database in the file using the DB_ORDERCHKONLY flag.

- `DB_ORDERCHKONLY`

  Perform the database checks for btree and duplicate sort order and for hashing, skipped by DB_NOORDERCHK.

  When this flag is specified, a **database** parameter should also be specified, indicating the database in the physical file which is to be checked. This flag is only safe to use on databases that have already successfully been verified using `DB->verify()` with the DB_NOORDERCHK flag set.

### Environment Variables

If the database was opened within a database environment, the environment variable `DB_HOME` may be used as the path of the database environment home.

`DB->verify()` is affected by any database directory specified using the <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> method, or by setting the "set_data_dir" string in the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file.

### Errors

The `DB->verify()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

#### ENOENT

The file or directory does not exist.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
