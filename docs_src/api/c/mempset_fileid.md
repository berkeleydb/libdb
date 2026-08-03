---
title: "DB_MPOOLFILE->set_fileid()"
api-name: "DB_MPOOLFILE->set_fileid()"
source: docs/api_reference/C/mempset_fileid.html
---
## DB_MPOOLFILE-\>set_fileid()

``` c
#include <db.h>

int
DB_MPOOLFILE->set_fileid(DB_MPOOLFILE *mpf, u_int8_t *fileid);  
```

The `DB_MPOOLFILE->set_fileid()` method specifies a unique identifier for the file. (The shared memory buffer pool functions must be able to uniquely identify files in order that multiple processes wanting to share a file will correctly identify it in the cache.)

On most UNIX/POSIX systems, the **fileid** field will not need to be set, and the memory pool functions will use the file's device and inode numbers for this purpose. On Windows systems, the memory pool functions use the values returned by `GetFileInformationByHandle()` by default — these values are known to be constant between processes and over reboot in the case of NTFS (in which they are the NTFS MFT indices).

On other filesystems (for example, FAT or NFS), these default values are not necessarily unique between processes or across system reboots. **Applications wanting to maintain a shared cache between processes or across system reboots, in which the cache contains pages from files stored on such filesystems, must specify a unique file identifier using the `DB_MPOOLFILE->set_fileid()` method, and each process opening the file must provide the same unique identifier.**

This call should not be necessary for most applications. Specifically, it is not necessary if the cache is not shared between processes and is reinstantiated after each system reboot, if the application is using the Berkeley DB access methods instead of calling the pool functions explicitly, or if the files in the cache are stored on filesystems in which the default values as described previously are invariant between process and across system reboots.

The `DB_MPOOLFILE->set_fileid()` method configures a file in the cache, not only operations performed using the specified <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle.

The `DB_MPOOLFILE->set_fileid()` method may not be called after the <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a> method is called. If the mpool file already exists when `DB_MPOOLFILE->open()` is called, the information specified to `DB_MPOOLFILE->set_fileid()` must be the same as that historically used to create the mpool file or corruption can occur.

The `DB_MPOOLFILE->set_fileid()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### fileid

The **fileid** parameter is the unique identifier for the file. Unique file identifiers must be a `DB_FILE_ID_LEN` length array of bytes.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
