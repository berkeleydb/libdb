---
title: "set_lk_detect"
api-name: "set_lk_detect"
source: docs/api_reference/C/set_lk_detect_parameter.html
---
## set_lk_detect

Sets the maximum number of locking entities supported by the Berkeley DB environment. This value is used by Berkeley DB to estimate how much space to allocate for various lock-table data structures. When using the DB, the default value is 2,000 lockers.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_lk_detect`, one or more whitespace characters, and the method **detect** parameter as a string. The detect parameter configures the deadlock detector. The deadlock detector will reject the lock request with the lowest priority. If multiple lock requests have the lowest priority, then the detect parameter is used to select which of those lock requests to reject.

For example:

``` c
set_lk_detect DB_LOCK_OLDEST
```

Sets the deadlock detector such that the lock request for the locker ID with the oldest lock is rejected.

The **detect** parameter values are:

- DB_LOCK_DEFAULT
- DB_LOCK_EXPIRE
- DB_LOCK_MAXLOCKS
- DB_LOCK_MAXWRITE
- DB_LOCK_MINLOCKS
- DB_LOCK_MINWRITE
- DB_LOCK_OLDEST
- DB_LOCK_RANDOM
- DB_LOCK_YOUNGEST

For more information, see <a href="envset_lk_detect.md" class="xref" title="DB_ENV-&gt;set_lk_detect()">DB_ENV-&gt;set_lk_detect()</a>.
