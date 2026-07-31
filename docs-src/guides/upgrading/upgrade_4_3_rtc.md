---
title: "Run-time configuration"
api-name: "Run-time configuration"
source: docs/upgrading/upgrade_4_3_rtc.html
---
## Run-time configuration

The signatures of the <a href="../../api/c/db_env_set_func_ftruncate.md" class="olink">db_env_set_func_ftruncate</a> and <a href="../../api/c/db_env_set_func_seek.md" class="olink">db_env_set_func_seek</a> functions have been simplified to take a byte offset in one parameter rather than a page size and a page number.
