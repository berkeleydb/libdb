---
title: "DB_MPOOLFILE->open()"
api-name: "DB_MPOOLFILE->open()"
source: docs/api_reference/C/mempfopen.html
---
## DB_MPOOLFILE-\>open()

``` c
#include <db.h>

int
DB_MPOOLFILE->open(DB_MPOOLFILE *mpf,
    char *file, u_int32_t flags, int mode, size_t pagesize);  
```

The `DB_MPOOLFILE->open()` method opens a file in the in-memory cache.

The `DB_MPOOLFILE->open()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### file

The **file** parameter is the name of the file to be opened. If **file** is NULL, a private temporary file is created that cannot be shared with any other process (although it may be shared with other threads of control in the same process).

When using a Unicode build on Windows (the default), the **file** argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

#### flags

The **flags** parameter must be set to zero or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_CREATE`

  Create any underlying files, as necessary. If the database do not already exist and the `DB_CREATE` flag is not specified, the call will fail.

- `DB_DIRECT`

  If set and supported by the system, turn off system buffering of the file to avoid double caching.

- `DB_MULTIVERSION`

  Open the file with support for <a href="../../programmer_reference/transapp_read.html" class="olink">multiversion concurrency control</a>. Calls to <a href="mempfget.md" class="xref" title="DB_MPOOLFILE-&gt;get()">DB_MPOOLFILE-&gt;get()</a> with dirty pages will cause copies to be made in the cache.

- `DB_NOMMAP`

  Always copy this file into the local cache instead of potentially mapping it into process memory (see the <a href="envset_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;set_mp_mmapsize()">DB_ENV-&gt;set_mp_mmapsize()</a> method for further information).

- `DB_ODDFILESIZE`

  Attempts to open files which are not a multiple of the page size in length will fail, by default. If the DB_ODDFILESIZE flag is set, any partial page at the end of the file will be ignored and the open will proceed.

- `DB_RDONLY`

  Open any underlying files for reading only. Any attempt to modify the file using the memory pool (cache) functions will fail, regardless of the actual permissions of the file.

#### mode

On Windows systems, the mode parameter is ignored.

On UNIX systems or in IEEE/ANSI Std 1003.1 (POSIX) environments, files created by `DB_MPOOLFILE->open()` are created with mode **mode** (as described in **chmod**(2)) and modified by the process' umask value at the time of creation (see **umask**(2)). Created files are owned by the process owner; the group ownership of created files is based on the system and directory defaults, and is not further specified by Berkeley DB. System shared memory segments created by `DB_MPOOLFILE->open()` are created with mode **mode**, unmodified by the process' umask value. If **mode** is 0, `DB_MPOOLFILE->open()` will use a default mode of readable and writable by both owner and group.

#### pagesize

The **pagesize** parameter is the size, in bytes, of the unit of transfer between the application and the cache, although it is not necessarily the unit of transfer between the cache and the underlying filesystem.

### Errors

The `DB_MPOOLFILE->open()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the file has already been entered into the cache, and the **pagesize** value is not the same as when the file was entered into the cache, or the length of the file is not zero or a multiple of the **pagesize**; the DB_RDONLY flag was specified for an in-memory cache; or if an invalid flag value or parameter was specified.

#### ENOMEM

The maximum number of open files has been reached.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
