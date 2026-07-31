---
title: "Application-specific logging and recovery"
api-name: "Application-specific logging and recovery"
source: docs/upgrading/upgrade_4_1_app_dispatch.html
---
## Application-specific logging and recovery

The application-specific logging and recovery tools and interfaces have been reworked in the 4.1 release to make it simpler for applications to use Berkeley DB to support their own logging and recovery of non-Berkeley DB objects. Specifically, the DB_ENV-\>set_recovery_init and DB_ENV-\>set_tx_recover interfaces have been removed, replaced by <a href="../../api/c/envset_app_dispatch.md" class="olink">DB_ENV-&gt;set_app_dispatch()</a>. Applications using either of the removed interfaces should be updated to call <a href="../../api/c/envset_app_dispatch.md" class="olink">DB_ENV-&gt;set_app_dispatch()</a>. For more information see <a href="../../guides/programmer_reference/apprec.md#apprec_intro" class="olink">Introduction to application specific logging and recovery</a> and the <a href="../../api/c/envset_app_dispatch.md" class="olink">DB_ENV-&gt;set_app_dispatch()</a> documentation.
