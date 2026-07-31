---
title: "mutex_set_align"
api-name: "mutex_set_align"
source: docs/api_reference/C/mutex_set_align_parameter.html
---
## mutex_set_align

Sets the mutex alignment, in bytes. It is sometimes advantageous to align mutexes on specific byte boundaries in order to minimize cache line collisions. This parameter specifies an alignment for mutexes allocated by Berkeley DB.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `mutex_set_align`, one or more whitespace characters, and the mutex alignment in bytes. Because the DB_CONFIG file is read when the database environment is opened, it will silently overrule configuration done before that time.

For more information, see <a href="mutexset_align.md" class="xref" title="DB_ENV-&gt;mutex_set_align()">DB_ENV-&gt;mutex_set_align()</a>.
