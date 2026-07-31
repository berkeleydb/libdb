---
title: "DB->set_isalive"
api-name: "DB->set_isalive"
source: docs/upgrading/upgrade_4_5_alive.html
---
## DB-\>set_isalive

In previous releases, the function specified to the <a href="../../api/c/envset_isalive.md" class="olink">DB_ENV-&gt;set_isalive()</a> method did not take a flags parameter. In the Berkeley DB 4.5 release, an additional flags argument has been added: <a href="../../api/c/envset_isalive.md#isalive_DB_MUTEX_PROCESS_ONLY" class="olink">DB_MUTEX_PROCESS_ONLY</a>.

Applications configuring an is-alive function should add a flags argument to the function, and change the function to ignore any thread ID and return the status of just the process, when the <a href="../../api/c/envset_isalive.md#isalive_DB_MUTEX_PROCESS_ONLY" class="olink">DB_MUTEX_PROCESS_ONLY</a> flag is specified.
