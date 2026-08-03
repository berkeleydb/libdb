---
title: "Replication environment IDs"
api-name: "Replication environment IDs"
source: docs/programmer_reference/rep_id.html
---
## Replication environment IDs

Each database environment included in a replication group must have a unique identifier for itself and for the other members of the replication group. The identifiers do not need to be global, that is, each database environment can assign local identifiers to members of the replication group as it encounters them. For example, given three sites: A, B and C, site A might assign the identifiers 1 and 2 to sites B and C respectively, while site B might assign the identifiers 301 and 302 to sites A and C respectively. Note that it is not wrong to have global identifiers, it is just not a requirement.

Replication Manager assigns and manages environment IDs on behalf of the application.

It is the responsibility of a Base API application to label each incoming replication message passed to <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> method with the appropriate identifier. Subsequently, Berkeley DB will label outgoing messages to the **send** function with those same identifiers.

Negative identifiers are reserved for use by Berkeley DB, and should never be assigned to environments by the application. Two of these reserved identifiers are intended for application use, as follows:

<span class="term"> <a href="../../api/c/reptransport.md#transport_DB_EID_BROADCAST" class="olink">DB_EID_BROADCAST</a> </span>  
The <a href="../../api/c/reptransport.md#transport_DB_EID_BROADCAST" class="olink">DB_EID_BROADCAST</a> identifier indicates a message should be broadcast to all members of a replication group.

<span class="term">DB_EID_INVALID</span>  
The DB_EID_INVALID identifier is an invalid environment ID, and may be used to initialize environment ID variables that are subsequently checked for validity.
