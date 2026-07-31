---
title: "Connecting to a new site"
api-name: "Connecting to a new site"
source: docs/programmer_reference/rep_newsite.html
---
## Connecting to a new site

To add a new site to the replication group all that is needed is for the client member to join. Berkeley DB will perform an internal initialization from the master to the client automatically and will run recovery on the client to bring it up to date with the master.

For Base API applications, connecting to a new site in the replication group happens whenever the <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> method returns <a href="../../api/c/repmessage.md#repmsg_DB_REP_NEWSITE" class="olink">DB_REP_NEWSITE</a>. The application should assign the new site a local environment ID number, and all future messages from the site passed to <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> should include that environment ID number. It is possible, of course, for the application to be aware of a new site before the return of <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> (for example, applications using connection-oriented protocols are likely to detect new sites immediately, while applications using broadcast protocols may not).

Regardless, in applications supporting the dynamic addition of database environments to replication groups, environments joining an existing replication group may need to provide contact information. (For example, in an application using TCP/IP sockets, a DNS name or IP address might be a reasonable value to provide.) This can be done using the **cdata** parameter to the <a href="../../api/c/repstart.md" class="olink">DB_ENV-&gt;rep_start()</a> method. The information referenced by **cdata** is wrapped in the initial contact message sent by the new environment, and is provided to the existing members of the group using the **rec** parameter returned by <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a>. If no additional information was provided for Berkeley DB to forward to the existing members of the group, the **data** field of the **rec** parameter passed to the <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> method will be NULL after <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> returns <a href="../../api/c/repmessage.md#repmsg_DB_REP_NEWSITE" class="olink">DB_REP_NEWSITE</a>.

Replication Manager automatically distributes contact information using the mechanisms previously described.
