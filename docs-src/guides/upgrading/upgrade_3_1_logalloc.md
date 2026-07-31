---
title: "log file pre-allocation"
api-name: "log file pre-allocation"
source: docs/upgrading/upgrade_3_1_logalloc.html
---
## log file pre-allocation

This change only affects Win/32 applications.

On Win/32 platforms Berkeley DB no longer pre-allocates log files. The problem was a noticeable performance spike as each log file was created. To turn this feature back on, search for the flag DB_OSO_LOG in the source file `log/log_put.c` and make the change described there, or contact us for assistance.
