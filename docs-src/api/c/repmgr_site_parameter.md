---
title: "repmgr_site"
api-name: "repmgr_site"
source: docs/api_reference/C/repmgr_site_parameter.html
---
## repmgr_site

Identifies a Replication Manager site.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `repmgr_site`, one or more whitespace characters, the host and port parameters specified as a string and an integer respectively. This can optionally be followed by one or more space-delimited keywords and `on`/`off`. For example:

``` c
repmgr_site example.com 49200 db_local_site on db_legacy off
```

Available keywords are:

- `db_bootstrap_helper`

  If turned `on`, the identified site may be used as a "helper" when the local site is first joining the replication group. Once the local site has been established as a member of the group, this setting is ignored.

- `db_group_creator`

  If turned `on`, this site should create the initial membership database contents, defining a "group" of just the one site, rather than trying to join an existing group when it starts for the first time. This setting is only used on the local site, and is ignored when configured on a remote site.

- `db_legacy`

  If turned `on`, specifies that the site is already part of an existing group. This setting causes the site to be upgraded from a previous version of BDB. All sites in the legacy group must specify this setting for themselves (the local site) and for all other sites currently existing in the group. Once the upgrade has been completed, this setting is no longer required.

- `db_local_site`

  If turned `on`, specifies that this site is the local site within the replication group. The application must identify exactly one site as the local site before replication is started.

- `db_repmgr_peer`

  If turned `on`, specifies that the site may be used as a target for "client-to-client" synchronization messages. This setting is ignored if it is turned `on` for the local site.

For more information, see <a href="dbsite_set_config.md" class="xref" title="DB_SITE-&gt;set_config()">DB_SITE-&gt;set_config()</a>.
