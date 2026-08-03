---
title: "DB_ENV->repmgr_get_ack_policy()"
api-name: "DB_ENV->repmgr_get_ack_policy()"
source: docs/api_reference/C/repmgrget_ack_policy.html
---
## DB_ENV-\>repmgr_get_ack_policy()

``` c
#include <db.h>

int
DB_ENV->repmgr_get_ack_policy(DB_ENV *env, int *ack_policyp);  
```

The `DB_ENV->repmgr_get_ack_policy()` method returns the Replication Manager's client acknowledgment policy. This is configured using the <a href="repmgrset_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_set_ack_policy()">DB_ENV-&gt;repmgr_set_ack_policy()</a> method.

The `DB_ENV->repmgr_get_ack_policy()` method may be called at any time during the life of the application.

The `DB_ENV->repmgr_get_ack_policy()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### ack_policyp

The **ack_policyp** parameter references memory into which the Replication Manager's client acknowledgement policy is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>, <a href="repmgrset_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_set_ack_policy()">DB_ENV-&gt;repmgr_set_ack_policy()</a>
