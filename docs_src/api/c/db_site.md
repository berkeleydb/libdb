---
title: "The DB_SITE Handle"
api-name: "The DB_SITE Handle"
source: docs/api_reference/C/db_site.html
---
## The DB_SITE Handle

The DB_SITE handle is used by Replication Manager applications to manage and configure replication sites. You create a DB_SITE handle using the <a href="repmgr_site.md" class="xref" title="DB_ENV-&gt;repmgr_site()">DB_ENV-&gt;repmgr_site()</a>, <a href="repmgr_site_by_eid.md" class="xref" title="DB_ENV-&gt;repmgr_site_by_eid()">DB_ENV-&gt;repmgr_site_by_eid()</a>, or <a href="repmgr_local_site.md" class="xref" title="DB_ENV-&gt;repmgr_local_site()">DB_ENV-&gt;repmgr_local_site()</a>, methods. All DB_SITE handles must be closed before closing DB_ENV handles. Use the <a href="dbsite_close.md" class="xref" title="DB_SITE-&gt;close()">DB_SITE-&gt;close()</a> method to close a DB_SITE handle.
