---
title: "DB_ENV->set_mutexlocks"
api-name: "DB_ENV->set_mutexlocks"
source: docs/upgrading/upgrade_3_2_mutexlock.html
---
## DB_ENV-\>set_mutexlocks

Previous Berkeley DB releases included the db_env_set_mutexlocks function, intended for debugging, that allows applications to always obtain requested mutual exclusion mutexes without regard for their availability. This function has been replaced with dbenv_set_mutexlocks, which provides the same functionality on a per-database environment basis. Applications using the old function should be updated to use the new one.
