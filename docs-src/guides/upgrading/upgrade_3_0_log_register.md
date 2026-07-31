---
title: "log_register"
api-name: "log_register"
source: docs/upgrading/upgrade_3_0_log_register.html
---
## log_register

An argument has been removed from the log_register function. The application should be searched for any occurrences of log_register. In each of these, the DBTYPE argument (it is the fourth argument) should be removed.
