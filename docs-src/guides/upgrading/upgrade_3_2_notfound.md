---
title: "Java java.io.FileNotFoundException"
api-name: "Java java.io.FileNotFoundException"
source: docs/upgrading/upgrade_3_2_notfound.html
---
## Java java.io.FileNotFoundException

The Java DbEnv.remove, Db.remove and Db.rename methods now throw java.io.FileNotFoundException in the case where the named file does not exist. Applications should be modified to catch this exception where appropriate.
