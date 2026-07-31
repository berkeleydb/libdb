---
title: "environment configuration"
api-name: "environment configuration"
source: docs/upgrading/upgrade_3_1_env.html
---
## environment configuration

A set of <a href="../../api/c/env.md" class="olink">DB_ENV</a> configuration methods which were not environment specific, but which instead affected the entire application space, have been removed from the <a href="../../api/c/env.md" class="olink">DB_ENV</a> object and replaced by static functions. The following table lists the <a href="../../api/c/env.md" class="olink">DB_ENV</a> methods previously available to applications and the static functions that should now be used instead.

| <a href="../../api/c/env.md" class="olink">DB_ENV</a> method | Berkeley DB 3.1 function |
|----|----|
| DB_ENV-\>set_func_close | <a href="../../api/c/db_env_set_func_close.md" class="olink">db_env_set_func_close</a> |
| DB_ENV-\>set_func_dirfree | <a href="../../api/c/db_env_set_func_dirfree.md" class="olink">db_env_set_func_dirfree</a> |
| DB_ENV-\>set_func_dirlist | <a href="../../api/c/db_env_set_func_dirlist.md" class="olink">db_env_set_func_dirlist</a> |
| DB_ENV-\>set_func_exists | <a href="../../api/c/db_env_set_func_exists.md" class="olink">db_env_set_func_exists</a> |
| DB_ENV-\>set_func_free | <a href="../../api/c/db_env_set_func_free.md" class="olink">db_env_set_func_free</a> |
| DB_ENV-\>set_func_fsync | <a href="../../api/c/db_env_set_func_fsync.md" class="olink">db_env_set_func_fsync</a> |
| DB_ENV-\>set_func_ioinfo | <a href="../../api/c/db_env_set_func_ioinfo.md" class="olink">db_env_set_func_ioinfo</a> |
| DB_ENV-\>set_func_malloc | <a href="../../api/c/db_env_set_func_malloc.md" class="olink">db_env_set_func_malloc</a> |
| DB_ENV-\>set_func_map | dbenv_set_func_map |
| DB_ENV-\>set_func_open | <a href="../../api/c/db_env_set_func_open.md" class="olink">db_env_set_func_open</a> |
| DB_ENV-\>set_func_read | <a href="../../api/c/db_env_set_func_read.md" class="olink">db_env_set_func_read</a> |
| DB_ENV-\>set_func_realloc | <a href="../../api/c/db_env_set_func_realloc.md" class="olink">db_env_set_func_realloc</a> |
| DB_ENV-\>set_func_rename | <a href="../../api/c/db_env_set_func_rename.md" class="olink">db_env_set_func_rename</a> |
| DB_ENV-\>set_func_seek | <a href="../../api/c/db_env_set_func_seek.md" class="olink">db_env_set_func_seek</a> |
| DB_ENV-\>set_func_sleep | db_env_set_func_sleep |
| DB_ENV-\>set_func_unlink | <a href="../../api/c/db_env_set_func_unlink.md" class="olink">db_env_set_func_unlink</a> |
| DB_ENV-\>set_func_unmap | dbenv_set_func_unmap |
| DB_ENV-\>set_func_write | <a href="../../api/c/db_env_set_func_write.md" class="olink">db_env_set_func_write</a> |
| DB_ENV-\>set_func_yield | <a href="../../api/c/db_env_set_func_yield.md" class="olink">db_env_set_func_yield</a> |
| DB_ENV-\>set_pageyield | dbenv_set_pageyield |
| DB_ENV-\>set_region_init | dbenv_set_region_init |
| DB_ENV-\>set_mutexlocks | dbenv_set_mutexlocks |
| DB_ENV-\>set_tas_spins | dbenv_set_tas_spins |
