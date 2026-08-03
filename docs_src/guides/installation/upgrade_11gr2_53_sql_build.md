---
title: "Changes in the SQL API Build"
api-name: "Changes in the SQL API Build"
source: docs/installation/upgrade_11gr2_53_sql_build.html
---
## Changes in the SQL API Build

Several changes have been made to the SQL API build. Encryption is now disabled by default on Windows, and the compile time flag BDBSQL_OMIT_SHARING has been changed to BDBSQL_SINGLE_PROCESS.

Encryption is now disabled by default in the Windows SQL API build. This makes it consistent with builds of the SQL API on other systems. To enable encryption in Visual Studios right click the db_sql project, and select `Properties->Configuration Properties->C/C++->Preprocessor` and add `SQLITE_HAS_CODEC` to `Preprocessor Definitions`.
