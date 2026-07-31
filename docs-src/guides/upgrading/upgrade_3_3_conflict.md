---
title: "DB_LOCK_CONFLICT"
api-name: "DB_LOCK_CONFLICT"
source: docs/upgrading/upgrade_3_3_conflict.html
---
## DB_LOCK_CONFLICT

The DB_LOCK_CONFLICT flag has been removed from the lock_detect function. Applications specifying the DB_LOCK_CONFLICT flag should simply replace it with a flags argument of 0.
