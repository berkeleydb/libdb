---
title: "Run-time configuration"
api-name: "Run-time configuration"
source: docs/programmer_reference/program_runtime.html
---
## Run-time configuration

It is possible for applications to configure Berkeley DB at run-time to redirect Berkeley DB library and system calls to alternate interfaces. For example, an application might want Berkeley DB to call debugging memory allocation routines rather than the standard C library interfaces. The following interfaces support this functionality:

- <a href="../../api/c/db_env_set_func_close.md" class="olink">db_env_set_func_close</a>

- <a href="../../api/c/db_env_set_func_dirfree.md" class="olink">db_env_set_func_dirfree</a>

- <a href="../../api/c/db_env_set_func_dirlist.md" class="olink">db_env_set_func_dirlist</a>

- <a href="../../api/c/db_env_set_func_exists.md" class="olink">db_env_set_func_exists</a>

- <a href="../../api/c/db_env_set_func_file_map.md" class="olink">db_env_set_func_file_map</a>

- <a href="../../api/c/db_env_set_func_free.md" class="olink">db_env_set_func_free</a>

- <a href="../../api/c/db_env_set_func_fsync.md" class="olink">db_env_set_func_fsync</a>

- <a href="../../api/c/db_env_set_func_ftruncate.md" class="olink">db_env_set_func_ftruncate</a>

- <a href="../../api/c/db_env_set_func_ioinfo.md" class="olink">db_env_set_func_ioinfo</a>

- <a href="../../api/c/db_env_set_func_malloc.md" class="olink">db_env_set_func_malloc</a>

- <a href="../../api/c/db_env_set_func_open.md" class="olink">db_env_set_func_open</a>

- <a href="../../api/c/db_env_set_func_pread.md" class="olink">db_env_set_func_pread</a>

- <a href="../../api/c/db_env_set_func_pwrite.md" class="olink">db_env_set_func_pwrite</a>

- <a href="../../api/c/db_env_set_func_read.md" class="olink">db_env_set_func_read</a>

- <a href="../../api/c/db_env_set_func_realloc.md" class="olink">db_env_set_func_realloc</a>

- <a href="../../api/c/db_env_set_func_region_map.md" class="olink">db_env_set_func_region_map</a>

- <a href="../../api/c/db_env_set_func_rename.md" class="olink">db_env_set_func_rename</a>

- <a href="../../api/c/db_env_set_func_seek.md" class="olink">db_env_set_func_seek</a>

- <a href="../../api/c/db_env_set_func_unlink.md" class="olink">db_env_set_func_unlink</a>

- <a href="../../api/c/db_env_set_func_write.md" class="olink">db_env_set_func_write</a>

- <a href="../../api/c/db_env_set_func_yield.md" class="olink">db_env_set_func_yield</a>

These interfaces are available only on POSIX platforms and from the Berkeley DB C language API.

A not-uncommon problem for applications is the new API in Solaris 2.6 for manipulating large files. Because this API was not part of Solaris 2.5, it is difficult to create a single binary that takes advantage of the large file functionality in Solaris 2.6, but still runs on Solaris 2.5. <a href="solaris.txt" class="ulink" target="_top">Example code</a> that supports this is included in the Berkeley DB distribution, however, the example code was written using previous versions of the Berkeley DB APIs, and is only useful as an example.
