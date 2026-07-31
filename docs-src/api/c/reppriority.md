---
title: "DB_ENV->rep_set_priority()"
api-name: "DB_ENV->rep_set_priority()"
source: docs/api_reference/C/reppriority.html
---
## DB_ENV-\>rep_set_priority()

``` c
#include <db.h>

int
DB_ENV->rep_set_priority(DB_ENV *env, u_int32_t priority);  
```

The `DB_ENV->rep_set_priority()` method specifies the database environment's priority in replication group elections. A special value of 0 indicates that this environment cannot be a replication group master.

### Note

The <a href="repmgrset_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_set_ack_policy()">DB_ENV-&gt;repmgr_set_ack_policy()</a> method describes <span class="emphasis">*electable peers*</span>, which are replication sites with a non-zero priority. For some acknowledgement policies, Replication Manager's computation of the durability result for each new update transaction is sensitive to whether each site in the group is a peer. Therefore, if you change a site's priority from a non-zero value to `0`, or from `0` to a non-zero value, this can invalidate the durability result of previously committed transactions.

The database environment's replication subsystem may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "rep_set_priority", one or more whitespace characters, and the priority of this site. For example, "rep_set_priority 1" sets the priority of this site to 1. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

Note that if the application never explicitly sets a priority, then a default value of 100 is used.

The `DB_ENV->rep_set_priority()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->rep_set_priority()` method may be called at any time during the life of the application.

The `DB_ENV->rep_set_priority()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### priority

The priority of this database environment in the replication group. The priority must be a non-zero integer, or 0 if this environment cannot be a replication group master. (See <a href="../../guides/programmer_reference/rep_pri.md" class="olink">Replication environment priorities</a> for more information).

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
