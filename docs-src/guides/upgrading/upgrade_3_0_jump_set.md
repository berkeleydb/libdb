---
title: "db_jump_set"
api-name: "db_jump_set"
source: docs/upgrading/upgrade_3_0_jump_set.html
---
## db_jump_set

The db_jump_set interface has been removed from the Berkeley DB 3.0 release, replaced by method calls on the <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle.

The following table lists the db_jump_set arguments previously used by applications and the methods that should now be used instead.

| db_jump_set argument | Berkeley DB 3.X method |
|----|----|
| DB_FUNC_CLOSE | <a href="../../api/c/db_env_set_func_close.md" class="olink">db_env_set_func_close</a> |
| DB_FUNC_DIRFREE | <a href="../../api/c/db_env_set_func_dirfree.md" class="olink">db_env_set_func_dirfree</a> |
| DB_FUNC_DIRLIST | <a href="../../api/c/db_env_set_func_dirlist.md" class="olink">db_env_set_func_dirlist</a> |
| DB_FUNC_EXISTS | <a href="../../api/c/db_env_set_func_exists.md" class="olink">db_env_set_func_exists</a> |
| DB_FUNC_FREE | <a href="../../api/c/db_env_set_func_free.md" class="olink">db_env_set_func_free</a> |
| DB_FUNC_FSYNC | <a href="../../api/c/db_env_set_func_fsync.md" class="olink">db_env_set_func_fsync</a> |
| DB_FUNC_IOINFO | <a href="../../api/c/db_env_set_func_ioinfo.md" class="olink">db_env_set_func_ioinfo</a> |
| DB_FUNC_MALLOC | <a href="../../api/c/db_env_set_func_malloc.md" class="olink">db_env_set_func_malloc</a> |
| DB_FUNC_MAP | dbenv_set_func_map |
| DB_FUNC_OPEN | <a href="../../api/c/db_env_set_func_open.md" class="olink">db_env_set_func_open</a> |
| DB_FUNC_READ | <a href="../../api/c/db_env_set_func_read.md" class="olink">db_env_set_func_read</a> |
| DB_FUNC_REALLOC | <a href="../../api/c/db_env_set_func_realloc.md" class="olink">db_env_set_func_realloc</a> |
| DB_FUNC_RUNLINK | The DB_FUNC_RUNLINK functionality has been removed from the Berkeley DB 3.0 release, and should be removed from the application. |
| DB_FUNC_SEEK | <a href="../../api/c/db_env_set_func_seek.md" class="olink">db_env_set_func_seek</a> |
| DB_FUNC_SLEEP | db_env_set_func_sleep |
| DB_FUNC_UNLINK | <a href="../../api/c/db_env_set_func_unlink.md" class="olink">db_env_set_func_unlink</a> |
| DB_FUNC_UNMAP | dbenv_set_func_unmap |
| DB_FUNC_WRITE | <a href="../../api/c/db_env_set_func_write.md" class="olink">db_env_set_func_write</a> |
| DB_FUNC_YIELD | <a href="../../api/c/db_env_set_func_yield.md" class="olink">db_env_set_func_yield</a> |
