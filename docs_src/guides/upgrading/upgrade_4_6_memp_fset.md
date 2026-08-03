---
title: "B_MPOOLFILE->set"
api-name: "B_MPOOLFILE->set"
source: docs/upgrading/upgrade_4_6_memp_fset.html
---
## B_MPOOLFILE-\>set

The DB_MPOOLFILE-\>set method has been removed from the Berkeley DB 4.6 release. Applications calling this method can upgrade by removing all calls to the method. This will result in no change in the application's behavior.
