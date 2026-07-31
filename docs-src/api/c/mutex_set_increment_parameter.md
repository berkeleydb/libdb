---
title: "mutex_set_increment"
api-name: "mutex_set_increment"
source: docs/api_reference/C/mutex_set_increment_parameter.html
---
## mutex_set_increment

Configures the number of additional mutexes to allocate. If an application will allocate mutexes for its own use, this parameter is used to add a number of mutexes to the default allocation.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `mutex_set_increment`, one or more whitespace characters, and the number of additional mutexes. Because the DB_CONFIG file is read when the database environment is opened, it will silently overrule configuration done before that time.

For more information, see <a href="mutexset_increment.md" class="xref" title="DB_ENV-&gt;mutex_set_increment()">DB_ENV-&gt;mutex_set_increment()</a>.
