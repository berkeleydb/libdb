---
title: "Group Membership in Repmgr"
api-name: "Group Membership in Repmgr"
source: docs/installation/upgrade_11gr2_52_grp_mbr.html
---
## Group Membership in Repmgr

<span class="sect2"> [Upgrading](upgrade_11gr2_52_grp_mbr.md#idp929720) </span>

<span class="sect2"> [New Functions](upgrade_11gr2_52_grp_mbr.md#idp910056) </span>

<span class="sect2"> [Modified Functions](upgrade_11gr2_52_grp_mbr.md#idp901088) </span>

<span class="sect2"> [New Events](upgrade_11gr2_52_grp_mbr.md#idp924520) </span>

<span class="sect2"> [Removed Functions](upgrade_11gr2_52_grp_mbr.md#idp937928) </span>

<span class="sect2"> [New Parameters](upgrade_11gr2_52_grp_mbr.md#idp909344) </span>

<span class="sect2"> [New Structure](upgrade_11gr2_52_grp_mbr.md#idp924776) </span>

Replication Manager now manages group membership much more closely, making it much easier for applications to add and remove sites from a replication group without risk of transaction loss. In order to accomplish this, the API for configuring group membership has changed significantly. The `repmgr_set_local_site()` and `repmgr_add_remote_site()` methods no longer exist; they are replaced by a new handle type, `DB_SITE`. The `repmgr_get_local_site()` method has been replaced by <a href="../../api/c/repmgr_site.md" class="olink">DB_ENV-&gt;repmgr_site()</a>, which now returns a `DB_SITE` handle instead of a raw host/port network address.

Replication Manager applications may no longer call the <a href="../../api/c/repnsites.md" class="olink">DB_ENV-&gt;rep_set_nsites()</a> method, because the Replication Manager now tracks the number of sites in the replication group for you. Replication Manager applications may still call <a href="../../api/c/repget_nsites.md" class="olink">DB_ENV-&gt;rep_get_nsites()</a>, but only after a successful call to <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a>.

For applications using the replication Base API there is no change, except that they may now call <a href="../../api/c/repnsites.md" class="olink">DB_ENV-&gt;rep_set_nsites()</a> to change the group size even when Master Leases are in use.

The new Replication Manager group membership functionality is described in the <a href="../../guides/programmer_reference/group_membership.md" class="olink">Managing Replication Manager Group Membership</a> chapter in the *Berkeley DB Programmer's Reference Guide*.

Replication Manager no longer prints an error message on a connection failure. Instead it generates an event with the equivalent information (invoking the application's event-handling call-back function).

### Upgrading

An existing application running a previous version of BDB can do a "live upgrade" so that only one site at a time has to be shut down. To do this, restart each site in the group, with the old master being shutdown last. When each site is restarted, use `DB_SITE` to configure the local site with the flag `DB_LEGACY`, and create a `DB_SITE` handle with a full specification of all the remote site addresses for all other sites currently in the group, and configure each handle with the `DB_LEGACY` flag. When the old master is restarted and a new master has been established, the new master is ready to manage membership changes, and new sites can be added as usual. But the application must not try to add new sites, or remove existing sites, during the mixed-version transitional phase.

To do a non-live upgrade shutdown the entire replication group. Then restart the group with each site configured with the `DB_LEGACY` flag, and in `DB_REP_ELECTION` mode.

### New Functions

- <a href="../../api/c/repmgr_site.md" class="olink">DB_ENV-&gt;repmgr_site()</a>
- <a href="../../api/c/repmgr_site_by_eid.md" class="olink">DB_ENV-&gt;repmgr_site_by_eid()</a>
- <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a>
- <a href="../../api/c/dbsite_get_config.md" class="olink">DB_SITE-&gt;get_config()</a>
- <a href="../../api/c/dbsite_remove.md" class="olink">DB_SITE-&gt;remove()</a>
- <a href="../../api/c/dbsite_get_eid.md" class="olink">DB_SITE-&gt;get_eid()</a>
- <a href="../../api/c/dbsite_get_address.md" class="olink">DB_SITE-&gt;get_address()</a>
- <a href="../../api/c/dbsite_close.md" class="olink">DB_SITE-&gt;close()</a>

### Modified Functions

- <a href="../../api/c/repnsites.md" class="olink">DB_ENV-&gt;rep_set_nsites()</a> is no longer used by the Replication Manager, but is still used by the Base API. It can now be used to change the number of sites dynamically, even when master leases are in use.

### New Events

- `DB_EVENT_REP_SITE_ADDED`
- `DB_EVENT_REP_SITE_REMOVED`
- `DB_EVENT_REP_LOCAL_SITE_REMOVED`
- `DB_EVENT_REP_CONNECT_BROKEN`
- `DB_EVENT_REP_CONNECT_ESTD`
- `DB_EVENT_REP_CONNECT_TRY_FAILED`
- `DB_EVENT_REP_INIT_DONE`

### Removed Functions

- `DB_ENV->repmgr_set_local_site()`
- `DB_ENV->repmgr_add_local_site()`
- `DB_ENV->repmgr_add_remote_site()`
- `DB_ENV->repmgr_get_local_site()`

### New Parameters

The following new parameters are passed to <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a>.

- `DB_BOOTSTRAP_HELPER`
- `DB_GROUP_CREATOR`
- `DB_LEGACY`
- `DB_LOCAL_SITE`
- `DB_REPMGR_PEER`

### New Structure

- `DB_REPMGR_CONN_ERR` encapsulates an EID and an integer system error code.
