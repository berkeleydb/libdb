---
title: "log_register"
api-name: "log_register"
source: docs/upgrading/upgrade_3_1_log_register.html
---
## log_register

The arguments to the log_register and log_unregister interfaces have changed. Instead of returning (and passing in) a logging file ID, a reference to the <a href="../../api/c/db.md" class="olink">DB</a> structure being registered (or unregistered) is passed. The application should be searched for any occurrences of log_register and log_unregister. For each one, change the arguments to be a reference to the <a href="../../api/c/db.md" class="olink">DB</a> structure being registered or unregistered.
