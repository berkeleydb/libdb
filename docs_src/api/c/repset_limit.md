---
title: "DB_ENV->rep_set_limit()"
api-name: "DB_ENV->rep_set_limit()"
source: docs/api_reference/C/repset_limit.html
---
## DB_ENV-\>rep_set_limit()

``` c
#include <db.h>

int
DB_ENV->rep_set_limit(DB_ENV *env, u_int32_t gbytes, u_int32_t bytes);  
```

The `DB_ENV->rep_set_limit()` method sets record transmission throttling. This is a byte-count limit on the amount of data that will be transmitted from a site in response to a single message processed by the <a href="repmessage.md" class="xref" title="DB_ENV-&gt;rep_process_message()">DB_ENV-&gt;rep_process_message()</a> method. The limit is not a hard limit, and the record that exceeds the limit is the last record to be sent.

Record transmission throttling is turned on by default with a limit of 10MB.

If the values passed to the `DB_ENV->rep_set_limit()` method are both zero, then the transmission limit is turned off.

The database environment's replication subsystem may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "rep_set_limit", one or more whitespace characters, and the limit specified in two parts: the gigabytes and the bytes values. For example, "rep_set_limit 0 1048576" sets a 1 megabyte limit. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->rep_set_limit()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->rep_set_limit()` method may be called at any time during the life of the application.

The `DB_ENV->rep_set_limit()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytes

The **gbytes** parameter specifies the number of gigabytes which, when added to the **bytes** parameter, specifies the maximum number of bytes that will be sent in a single call to the <a href="repmessage.md" class="xref" title="DB_ENV-&gt;rep_process_message()">DB_ENV-&gt;rep_process_message()</a> method.

#### bytes

The **bytes** parameter specifies the number of bytes which, when added to the **gbytes** parameter, specifies the maximum number of bytes that will be sent in a single call to the <a href="repmessage.md" class="xref" title="DB_ENV-&gt;rep_process_message()">DB_ENV-&gt;rep_process_message()</a> method.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
