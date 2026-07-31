---
title: "Managing Replication Files"
api-name: "Managing Replication Files"
source: docs/programmer_reference/rep_filename.html
---
## Managing Replication Files

Whether you use the Base API or the Replication Manager, replication creates a set of internal files that are normally stored on-disk in your environment home directory. These files contain metadata which is necessary for replication operations, and so you should never delete these files.

You can cause these files to not be stored on disk, but instead to be held entirely in-memory, by specifying the <a href="../../api/c/repconfig.md#config_DB_REP_CONF_INMEM" class="olink">DB_REP_CONF_INMEM</a> flag to the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method. Doing this can improve your application's data throughput by avoiding the disk I/O associated with these metadata files. However, in the event that your application is shut down, the contents of these files are lost. This results in some loss of functionality, including an increased chance that elections will fail, or that the wrong site will win an election. See the <a href="../../api/c/repconfig.md#config_DB_REP_CONF_INMEM" class="olink">DB_REP_CONF_INMEM</a> flag description for more information.

Note that turning on <a href="../../api/c/repconfig.md#config_DB_REP_CONF_INMEM" class="olink">DB_REP_CONF_INMEM</a> means that Replication Manager cannot store group membership changes persistently. This is because Replication Manager stores group membership information in an internal database, which is held in memory when <a href="../../api/c/repconfig.md#config_DB_REP_CONF_INMEM" class="olink">DB_REP_CONF_INMEM</a> is turned on. For this reason, if your Replication Manager application requires replication metadata to be stored in memory, then you must manually identify all the sites in your replication group using the `DB_LEGACY` site configuration attribute. Be aware that this configuration needs to be made permanent. (Normally, `DB_LEGACY` is used only on a temporary basis for the purpose of upgrading old Replication Manager applications.)

Do the following:

1.  Shut down all the sites in your replication group.

2.  For every site in your replication group:

    1.  Configure a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle for the local site. Use <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a> to indicate that this is a legacy site by setting the `DB_LEGACY` parameter.

    2.  Configure a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle for <span class="emphasis">*every other site*</span> in the replication group. Set the `DB_LEGACY` parameter for each of these handles.

        Please pay careful attention to this step. To repeat: a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle MUST be configured for EVERY site in the replication group.

3.  Restart all the sites in the replication group.

Alternatively, you can store persistent environment metadata files, including those required by replication, in a location other than your environment home directory. Doing so can help improve I/O throughput by placing these files on a spindle that is not being used for other environment data I/O. You do this using the <a href="../../api/c/envset_metadata_dir.md" class="olink">DB_ENV-&gt;set_metadata_dir()</a> method.

Note that you must configure the handling of your environment metadata consistently across your entire replication group. That is, if you place your replication metadata in-memory on one site, then it must be placed in-memory on all the sites in the group. Similarly, if you place your replication metadata files in a non-standard directory location on one site, then they must be placed in the exact same directory location on all the sites in your group.
