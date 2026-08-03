---
title: "Appendix C.  Berkeley DB Application Space Static Functions"
api-name: "Appendix C.  Berkeley DB Application Space Static Functions"
source: docs/api_reference/C/setfunc.html
---
## Appendix C.  Berkeley DB Application Space Static Functions

This appendix describes functionality that existed on the DB_ENV handle in releases prior to Berkeley DB 3.1. In 3.1, this functionality was moved to as series of static functions, as in this appendix.

## Static Functions

| Static Function | Description |
|----|----|
| <a href="db_env_set_func_close.md" class="xref" title="db_env_set_func_close">db_env_set_func_close</a> | Replace Berkeley DB calls to close() with the identified function. |
| <a href="db_env_set_func_dirfree.md" class="xref" title="db_env_set_func_dirfree">db_env_set_func_dirfree</a> | Specify function used to free memory obtained due to a directory walk. |
| <a href="db_env_set_func_dirlist.md" class="xref" title="db_env_set_func_dirlist">db_env_set_func_dirlist</a> | Specify function used to free memory obtained due to a directory list. |
| <a href="db_env_set_func_exists.md" class="xref" title="db_env_set_func_exists">db_env_set_func_exists</a> | Specify function used to determine whether a file exists. |
| <a href="db_env_set_func_file_map.md" class="xref" title="db_env_set_func_file_map">db_env_set_func_file_map</a> | Specify function used to map a file into memory. |
| <a href="db_env_set_func_free.md" class="xref" title="db_env_set_func_free">db_env_set_func_free</a> | Specify function used to free memory. |
| <a href="db_env_set_func_fsync.md" class="xref" title="db_env_set_func_fsync">db_env_set_func_fsync</a> | Specify function used to sync a file to disk. |
| <a href="db_env_set_func_ftruncate.md" class="xref" title="db_env_set_func_ftruncate">db_env_set_func_ftruncate</a> | Specify function used to truncate a file. |
| <a href="db_env_set_func_ioinfo.md" class="xref" title="db_env_set_func_ioinfo">db_env_set_func_ioinfo</a> | Specify function used to determine file characteristics. |
| <a href="db_env_set_func_malloc.md" class="xref" title="db_env_set_func_malloc">db_env_set_func_malloc</a> | Specify function used to allocate memory. |
| <a href="db_env_set_func_open.md" class="xref" title="db_env_set_func_open">db_env_set_func_open</a> | Specify function used to open a file. |
| <a href="db_env_set_func_pread.md" class="xref" title="db_env_set_func_pread">db_env_set_func_pread</a> | Specify function used to read data from an object. |
| <a href="db_env_set_func_pwrite.md" class="xref" title="db_env_set_func_pwrite">db_env_set_func_pwrite</a> | Specify function used to write data to an object. |
| <a href="db_env_set_func_read.md" class="xref" title="db_env_set_func_read">db_env_set_func_read</a> | Specify function used to read data from an object. |
| <a href="db_env_set_func_realloc.md" class="xref" title="db_env_set_func_realloc">db_env_set_func_realloc</a> | Specify function used to change the size of memory pointed to by a pointer. |
| <a href="db_env_set_func_region_map.md" class="xref" title="db_env_set_func_region_map">db_env_set_func_region_map</a> | Specify function used to created shared memory regions. |
| <a href="db_env_set_func_rename.md" class="xref" title="db_env_set_func_rename">db_env_set_func_rename</a> | Specify function used to change the name of a file. |
| <a href="db_env_set_func_seek.md" class="xref" title="db_env_set_func_seek">db_env_set_func_seek</a> | Specify function used to specify a location in a file. |
| <a href="db_env_set_func_unlink.md" class="xref" title="db_env_set_func_unlink">db_env_set_func_unlink</a> | Specify function used to delete a file. |
| <a href="db_env_set_func_write.md" class="xref" title="db_env_set_func_write">db_env_set_func_write</a> | Specify function used to write data to an object. |
| <a href="db_env_set_func_yield.md" class="xref" title="db_env_set_func_yield">db_env_set_func_yield</a> | Specify function used to yield the processor to another thread of control. |
