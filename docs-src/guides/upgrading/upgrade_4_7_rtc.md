---
title: "Run-time configuration"
api-name: "Run-time configuration"
source: docs/upgrading/upgrade_4_7_rtc.html
---
## Run-time configuration

In historic Berkeley DB releases, there were separate sleep and yield functions to be configured at run-time using the db_env_set_func_sleep and <a href="../../api/c/db_env_set_func_yield.md" class="olink">db_env_set_func_yield</a> functions. These functions have been merged in the Berkeley DB 4.7 release. The replacement function should always yield the processor, and optionally wait for some period of time before allowing the thread to run again.

Applications using the Berkeley DB run-time configuration interfaces should merge the functionality of their sleep and yield functions into a single configuration function.

In the 4.7 Berkeley DB release, the db_env_set_func_map and db_env_set_func_unmap functions have been replaced. This change fixes problems where applications using the Berkeley DB run-time configuration interfaces could not open multiple <a href="../../api/c/env.md" class="olink">DB_ENV class</a> handles for the same database environment in a single application or join existing database environments from within multiple processes.

Applications wanting to replace the Berkeley DB region creation functionality should replace their db_env_set_func_map and db_env_set_func_unmap calls with a call to the <a href="../../api/c/db_env_set_func_region_map.md" class="olink">db_env_set_func_region_map</a> function. Applications wanting to replace the Berkeley DB region file mapping functionality should replace their db_env_set_func_map and db_env_set_func_unmap calls with a call to the <a href="../../api/c/db_env_set_func_file_map.md" class="olink">db_env_set_func_file_map</a> function.
