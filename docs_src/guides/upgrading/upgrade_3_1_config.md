---
title: "DB_ENV->open, DB_ENV->remove"
api-name: "DB_ENV->open, DB_ENV->remove"
source: docs/upgrading/upgrade_3_1_config.html
---
## DB_ENV-\>open, DB_ENV-\>remove

In the Berkeley DB 3.1 release, the **config** argument to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> and <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> methods has been removed, replaced by additional methods on the <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle. If your application calls <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> or <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> with a NULL **config** argument, find those functions and remove the config argument from the call. If your application has non-NULL **config** argument, the strings values in that argument are replaced with calls to <a href="../../api/c/env.md" class="olink">DB_ENV</a> methods as follows:

| Previous config string | Berkeley DB 3.1 version method |
|----|----|
| DB_DATA_DIR | <a href="../../api/c/envset_data_dir.md" class="olink">DB_ENV-&gt;set_data_dir()</a> |
| DB_LOG_DIR | <a href="../../api/c/envset_lg_dir.md" class="olink">DB_ENV-&gt;set_lg_dir()</a> |
| DB_TMP_DIR | <a href="../../api/c/envset_tmp_dir.md" class="olink">DB_ENV-&gt;set_tmp_dir()</a> |
