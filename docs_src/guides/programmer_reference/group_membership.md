---
title: "Managing Replication Manager Group Membership"
api-name: "Managing Replication Manager Group Membership"
source: docs/programmer_reference/group_membership.html
---
## Managing Replication Manager Group Membership

<span class="sect2"> [Adding Sites to a Replication Group](group_membership.md#group_mem_add) </span>

<span class="sect2"> [Removing Sites from a Replication Group](group_membership.md#group_mem_remove) </span>

<span class="sect2"> [Primordial Startups](group_membership.md#group_mem_primordialstartup) </span>

<span class="sect2"> [Upgrading Groups](group_membership.md#group_mem_upgrade) </span>

A replication group is a collection of two or more database environments which are configured to replicate with one another. When operating normally, a replication group consists of a master site and one or more read-only sites.

For Replication Manager applications, the sites comprising the replication group are recorded in an internal database, so even if a group member is not available, it counts towards the group's total site count. This matters for certain replication activities, such as holding elections and acknowledging replication messages that require some number of sites to participate in these activities. Replicated applications will often require all sites, or a majority of sites, to participate before the activity can be completed.

### Note

If you are configuring your application to keep replication metadata in-memory by specifying the <a href="../../api/c/repconfig.md#config_DB_REP_CONF_INMEM" class="olink">DB_REP_CONF_INMEM</a> flag to the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method, then the internal database containing group site information is not stored persistently on disk. This severely limits Replication Manager's ability to automatically manage group membership. For more information, including some work-arounds, see <a href="rep_filename.md" class="xref" title="Managing Replication Files">Managing Replication Files</a>.

Because Replication Manager tracks group members, there are some administrative activities that you should know about when using BDB replication.

### Adding Sites to a Replication Group

To add a site to a replication group, you merely start up the site such that it knows where at least one site in the group is located. The site new site then joins the group. When this happens, the new site is recorded in the Replication Manager's group member database.

Note that when you are starting the very first site in the group for the very first time (called <span class="emphasis">*the primordial start up*</span>), there are no other existing sites to help the new site join the group. In fact, a primordial start up actually creates the group. For this reason, there are some slight differences on how to perform a primordial start up. For a description of this, see <a href="group_membership.md#group_mem_primordialstartup" class="xref" title="Primordial Startups">Primordial Startups</a>.

When you add a site to a replication group, you use the following general procedure:

- Make sure your replication group is operating well enough that write activity can occur.

- Create and open the environment such that it is configured to use replication.

- Use <a href="../../api/c/repmgr_site.md" class="olink">DB_ENV-&gt;repmgr_site()</a> to obtain a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle. Configure this handle for the local site's host and port information when you create the handle. Then, use <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a> to indicate that this is the local site by setting the `DB_LOCAL_SITE` parameter.

- Use <a href="../../api/c/repmgr_site.md" class="olink">DB_ENV-&gt;repmgr_site()</a> to obtain a second <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle. Configure this handle with the host and port information for a site that already belongs to the replication group. Then, use <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a> to indicate this site is a "helper" site by setting the `DB_BOOTSTRAP_HELPER` parameter. By configuring a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle in this way, your new site will know how to contact the replication group so that it can join the group.

- Start replication as normal by configuring an acknowledgement policy, setting the site's replication priority, and then calling <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a>.

Note that on subsequent start-ups of your replication code, any helper site information you might provide is ignored because the Replication Manager reads the group membership database in order to obtain this information.

Also, be aware that if the new site cannot be added to the group for some reason (because a master site is not available, or because insufficient replicas are running to acknowledge the new site), the attempt to start the new site via <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a> will fail and return `DB_REP_UNAVAIL`. You can then pause and retry the start up attempt until it completes successfully.

You must use the exact same host string and port number to refer to a given site throughout your application and on each of its sites.

### Removing Sites from a Replication Group

Elections and message acknowledgements require knowledge of the total number of sites in the group. If a site is shut down, or is otherwise unable to communicate with the rest of the group, it still counts towards the total number of sites in the group. In most cases, this is the desirable behavior.

However, if you are shutting down a site permanently, then you should remove that site from the group. You might also want to remove a site from the group if you are shutting it down temporarily, but nevertheless for a very long period of time (days or weeks). In either case, you remove a site from the group by:

- Make sure your replication group is operating well enough that write activity can occur.

- On one of the sites in your replication group (this does not have to be the master site), use <a href="../../api/c/repmgr_site.md" class="olink">DB_ENV-&gt;repmgr_site()</a> to obtain a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle. Configure this handle with the host and port information of the site that you want to remove.

  Note that this step can occur at any site — including the site that you are removing from the group.

- Call the <a href="../../api/c/dbsite_remove.md" class="olink">DB_SITE-&gt;remove()</a> method. This removes the identified site from the replication group database. If this action is not performed on the master site, the client sends a request to the master to perform the operation and awaits confirmation.

### Note

Upon completing the above procedure, DO NOT call the <a href="../../api/c/dbsite_close.md" class="olink">DB_SITE-&gt;close()</a> method. After removing (or even attempting to remove) a site from the group using a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle, the handle must never be accessed again.

### Primordial Startups

If you have never started a site in a replication group before, then the replication group membership database does not exist. In this situation, you must start the site and declare it to be the group creator. This causes the site to become the master, create the group membership database, and create a replication group of size 1. After that, subsequent sites can add themselves to the group as described in <a href="group_membership.md#group_mem_add" class="xref" title="Adding Sites to a Replication Group">Adding Sites to a Replication Group</a>.

### Note

It is never incorrect to declare a site the group creator. This is true even well-after the replication group has been established. This is because group creator information is ignored on any site start-up, except for the primoridial start-up; that is, a start-up where the group membership database does not exist.

To declare a site as the group creator:

- Create and open the environment such that it is configured to use replication.

- Use <a href="../../api/c/repmgr_site.md" class="olink">DB_ENV-&gt;repmgr_site()</a> to obtain a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle. Configure this handle for the local site's host and port information when you create the handle. Then, use <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a> to indicate that this is the group creator site by setting the `DB_GROUP_CREATOR` parameter.

- Start replication as normal by configuring acknowledgement policies, setting replication priorities for the site, and then calling <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a>.

### Upgrading Groups

Prior to the Berkeley DB 11.2.5.2 release, replication group membership was managed differently than in the way it is described in the previous sections. For this reason, when you upgrade from older releases of Berkeley DB to 11.2.5.2 or later, the upgrade procedure is different than when upgrading between other releases.

To perform an upgrade that takes you from the old way of managing group membership to the new way of managing group membership (pre-11.2.5.2 to 11.2.5.2 and later), do the following:

- Update your replication code to use the new <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle and related methods. Recompile and thoroughly test your code to make sure it is production-ready.

- Do the following one production machine at a time. Make sure to do this at the master site LAST.

  1.  Shut down the old replication code.

  2.  Install the new replication code.

  3.  Configure a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle for the local site. Use <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a> to indicate that this is a legacy site by setting the `DB_LEGACY` parameter.

  4.  Configure a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle for <span class="emphasis">*every other site*</span> in the replication group. Set the `DB_LEGACY` parameter for each of these handles.

      Please pay careful attention to this step. To repeat: a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle MUST be configured for EVERY site in the replication group.

  5.  Start replication. The site is upgraded at this point.

  Once you have performed this procedure for each production site, making sure to upgrade the master only after every other site has been upgraded, you are done upgrading your replicated application to use the current group membership mechanism.

On subsequent restarts of your replication code, you do not need to specify the `DB_LEGACY` parameter, nor do you need to identify all of the replication group members. However, it is not an error if you do specify this information on subsequent start ups.
