---
title: "EACCES"
api-name: "EACCES"
source: docs/upgrading/upgrade_3_0_eacces.html
---
## EACCES

There was an error in previous releases of the Berkeley DB documentation that said that the lock_put and lock_vec interfaces could return EACCES as an error to indicate that a lock could not be released because it was held by another locker. The application should be searched for any occurrences of EACCES. For each of these, any that are checking for an error return from lock_put or lock_vec should have the test and any error handling removed.
