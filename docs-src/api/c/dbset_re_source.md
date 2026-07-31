---
title: "DB->set_re_source()"
api-name: "DB->set_re_source()"
source: docs/api_reference/C/dbset_re_source.html
---
## DB-\>set_re_source()

``` c
#include <db.h>

int
DB->set_re_source(DB *db, char *source);  
```

Set the underlying source file for the Recno access method. The purpose of the **source** value is to provide fast access and modification to databases that are normally stored as flat text files.

The **source** parameter specifies an underlying flat text database file that is read to initialize a transient record number index. In the case of variable length records, the records are separated, as specified by <a href="dbset_re_delim.md" class="xref" title="DB-&gt;set_re_delim()">DB-&gt;set_re_delim()</a>. For example, standard UNIX byte stream files can be interpreted as a sequence of variable length records separated by \<newline\> characters.

In addition, when cached data would normally be written back to the underlying database file (for example, the <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a> or <a href="dbsync.md" class="xref" title="DB-&gt;sync()">DB-&gt;sync()</a> methods are called), the in-memory copy of the database will be written back to the **source** file.

By default, the backing source file is read lazily; that is, records are not read from the file until they are requested by the application. **If multiple processes (not threads) are accessing a Recno database concurrently, and are either inserting or deleting records, the backing source file must be read in its entirety before more than a single process accesses the database, and only that process should specify the backing source file as part of the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> call. See the <a href="dbset_flags.md#dbset_flags_DB_SNAPSHOT" class="link">DB_SNAPSHOT</a> flag for more information.**

**Reading and writing the backing source file specified by ****source** cannot be transaction-protected because it involves filesystem operations that are not part of the Db transaction methodology.**** For this reason, if a temporary database is used to hold the records, it is possible to lose the contents of the **source** file, for example, if the system crashes at the right instant. If a file is used to hold the database, normal database recovery on that file can be used to prevent information loss, although it is still possible that the contents of **source** will be lost if the system crashes.

The **source** file must already exist (but may be zero-length) when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called.

It is not an error to specify a read-only **source** file when creating a database, nor is it an error to modify the resulting database. However, any attempt to write the changes to the backing source file using either the <a href="dbsync.md" class="xref" title="DB-&gt;sync()">DB-&gt;sync()</a> or <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a> methods will fail, of course. Specify the <a href="dbclose.md#dbclose_DB_NOSYNC" class="link">DB_NOSYNC</a> flag to the <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a> method to stop it from attempting to write the changes to the backing file; instead, they will be silently discarded.

For all of the previous reasons, the **source** field is generally used to specify databases that are read-only for Berkeley DB applications; and that are either generated on the fly by software tools or modified using a different mechanism — for example, a text editor.

The `DB->set_re_source()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_re_source()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_re_source()` must be the same as that historically used to create the database or corruption can occur.

The `DB->set_re_source()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### source

The backing flat text database file for a Recno database.

When using a Unicode build on Windows (the default), the **source** argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

### Errors

The `DB->set_re_source()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
