---
title: "lock statistics"
api-name: "lock statistics"
source: docs/upgrading/upgrade_4_4_lockstat.html
---
## lock statistics

The names of two fields in the lock statistics changed in the Berkeley DB 4.4 release. The **st_nconflicts** field was renamed to be **st_lock_wait**, and the **st_nnowaits** field was renamed to be **st_lock_nowait**. The meaning of the fields is unchanged (although the documentation has been updated to make it clear what these fields really represent).
