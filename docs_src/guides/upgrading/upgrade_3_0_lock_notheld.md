---
title: "DB_LOCK_NOTHELD"
api-name: "DB_LOCK_NOTHELD"
source: docs/upgrading/upgrade_3_0_lock_notheld.html
---
## DB_LOCK_NOTHELD

Historically, the Berkeley DB lock_put and lock_vec interfaces could return the DB_LOCK_NOTHELD error to indicate that a lock could not be released as it was held by another locker. This error can no longer be returned under any circumstances. The application should be searched for any occurrences of DB_LOCK_NOTHELD. For each of these, the test and any error processing should be removed.
