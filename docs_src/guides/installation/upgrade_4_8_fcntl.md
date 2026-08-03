---
title: "Dropped Support for fcntl System Calls"
api-name: "Dropped Support for fcntl System Calls"
source: docs/installation/upgrade_4_8_fcntl.html
---
## Dropped Support for fcntl System Calls

Berkeley DB no longer supports mutex implementations based on the `fcntl` system call. If you have been configuring Berkeley DB to use this type of mutex, you need to either switch to a different mutex type or contact the Berkeley DB team for support.
