---
title: "lock_detect"
api-name: "lock_detect"
source: docs/upgrading/upgrade_3_0_lock_detect.html
---
## lock_detect

An additional argument has been added to the lock_detect function.

The application should be searched for any occurrences of lock_detect. For each one, a NULL argument should be appended to the current arguments.
