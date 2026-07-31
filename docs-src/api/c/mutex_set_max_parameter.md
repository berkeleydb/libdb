---
title: "mutex_set_max"
api-name: "mutex_set_max"
source: docs/api_reference/C/mutex_set_max_parameter.html
---
## mutex_set_max

Configures the total number of mutexes to allocate. Berkeley DB allocates a default number of mutexes based on the initial configuration of the database environment. That default calculation may be too small if the application has an unusual need for mutexes. This parameter is used to specify an absolute number of mutexes to allocate.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `mutex_set_max`, one or more whitespace characters, and the total number of mutexes. Because the DB_CONFIG file is read when the database environment is opened, it will silently overrule configuration done before that time.

For more information, see <a href="mutexset_max.md" class="xref" title="DB_ENV-&gt;mutex_set_max()">DB_ENV-&gt;mutex_set_max()</a>.
