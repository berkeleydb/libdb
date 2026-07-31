---
title: "lock_put"
api-name: "lock_put"
source: docs/upgrading/upgrade_3_0_lock_put.html
---
## lock_put

An argument change has been made in the lock_put function.

The application should be searched for any occurrences of lock_put. For each one, instead of passing a DB_LOCK variable as the last argument to the function, the address of the DB_LOCK variable should be passed.
