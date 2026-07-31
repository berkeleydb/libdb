---
title: "DB_ENV->set_intermediate_dir"
api-name: "DB_ENV->set_intermediate_dir"
source: docs/upgrading/upgrade_4_7_interdir.html
---
## DB_ENV-\>set_intermediate_dir

Historic releases of Berkeley DB contained an undocumented <a href="../../api/c/env.md" class="olink">DB_ENV</a> method named DB_ENV-\>set_intermediate_dir, which configured the creation of any intermediate directories needed during recovery. This method has been standardized as the <a href="../../api/c/envset_intermediate_dir_mode.md" class="olink">DB_ENV-&gt;set_intermediate_dir_mode()</a> method.

Applications using DB_ENV-\>set_intermediate_dir should be modified to use the <a href="../../api/c/envset_intermediate_dir_mode.md" class="olink">DB_ENV-&gt;set_intermediate_dir_mode()</a> method instead.
