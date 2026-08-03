---
title: "DB_SITE->set_config()"
api-name: "DB_SITE->set_config()"
source: docs/api_reference/C/dbsite_set_config.html
---
## DB_SITE-\>set_config()

``` c
#include <db.h>

int
DB_SITE->set_config(DB_SITE *site, u_int32_t which, u_int32_t value); 
```

The `DB_SITE->set_config()` method configures a Replication Manager site.

The `DB_SITE->set_config()` method returns a non-zero error value on failure and 0 on success.

The Replication Manager site may also be configured using the environment's DB_CONFIG file. The syntax of the entry in that file is described in <a href="repmgr_site_parameter.md" class="xref" title="repmgr_site">repmgr_site</a>.

### Parameters

#### which

This parameter must be set to one of the following values:

- `DB_BOOTSTRAP_HELPER`

  Specifies that a remote site may be used as a "helper" when the local site is first joining the replication group. Once the local site has been established as a member of the group, this setting is ignored.

- `DB_GROUP_CREATOR`

  Specifies that this site should create the initial membership database contents, defining a "group" of just the one site, rather than trying to join an existing group when it starts for the first time.

  This setting can only be used on the local site and is ignored when configured for a remote site.

- `DB_LEGACY`

  Specifies that the site is already part of an existing group. This setting causes the site to be upgraded from a previous version of BDB. All sites in the legacy group must specify this setting for themselves (the local site) and for all other sites currently existing in the group. Once the upgrade has been completed, this setting is no longer required.

- `DB_LOCAL_SITE`

  Specifies that this site is the local site within the replication group. The application must identify exactly one site as the local site in this way, before calling the <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> method.

- `DB_REPMGR_PEER`

  Specifies that the site may be used as a target for "client-to-client" synchronization messages. This setting is ignored if it is specified for the local site.

#### value

If `0`, the parameter identified by the **which** is turned off. Otherwise, it is turned on.

### Errors

The `DB_SITE->set_config()` method may fail and return one of the following non-zero errors:

#### EINVAL

If an invalid flag value or parameter was specified.

### Class

<a href="db_site.md" class="link" title="The DB_SITE Handle">DB_SITE</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
