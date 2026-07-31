---
title: "Support for Multiple Client-to-Client Peers"
api-name: "Support for Multiple Client-to-Client Peers"
source: docs/installation/upgrade_11gr2_repmgr.html
---
## Support for Multiple Client-to-Client Peers

A Berkeley DB Replication Manager application can now designate one or more remote sites (called its "peers") to receive client-to-client requests.

In previous releases, there could be only one peer at a time. If you called the `DB_ENV->repmgr_add_remote_site` method specifying site "A" as a peer and you made another call specifying site "B" as a peer, site "B" would become the only peer, and site "A" would no longer be a peer.

Starting with Berkeley DB 11gR2, the same sequence of calls results in both site "A" and site "B" being possible peers. Replication Manager may select any of a site's possible peers to use for client-to-client requests. If the first peer that the Replication Manager selects cannot be used (for example, it is unavailable or it is the current master), Replication Manager attempts to use a different peer if there is more than one peer.

To get the pre-11gR2 peer behavior in this example, you must make an additional call to the `DB_ENV->repmgr_add_remote_site` method, specifying site "A" and a flag value that excludes the `DB_REPMGR_PEER` bit value to remove site "A" as a possible peer.
