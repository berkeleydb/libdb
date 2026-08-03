---
title: "DB->set_pagesize"
api-name: "DB->set_pagesize"
source: docs/upgrading/upgrade_4_5_pagesize.html
---
## DB-\>set_pagesize

In previous releases, when creating a new database in a physical file which already contained databases, it was an error to specify a page size different from the existing databases in the file. In the Berkeley DB 4.5 release, any page size specified is ignored if the file in which the database is being created already exists.
