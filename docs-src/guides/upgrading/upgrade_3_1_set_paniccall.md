---
title: "DB_ENV->set_paniccall, DB->set_paniccall"
api-name: "DB_ENV->set_paniccall, DB->set_paniccall"
source: docs/upgrading/upgrade_3_1_set_paniccall.html
---
## DB_ENV-\>set_paniccall, DB-\>set_paniccall

Starting with the 3.1 release of Berkeley DB, the DB_ENV-\>set_paniccall and DB-\>set_paniccall methods may return an error value, that is, they are no longer declared as returning no value, instead they return an int or throw an exception as appropriate when an error occurs.

If your application calls these functions, you may want to check for a possible error on return.
