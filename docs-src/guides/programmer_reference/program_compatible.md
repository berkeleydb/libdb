---
title: "Compatibility with historic UNIX interfaces"
api-name: "Compatibility with historic UNIX interfaces"
source: docs/programmer_reference/program_compatible.html
---
## Compatibility with historic UNIX interfaces

The Berkeley DB version 2 library provides backward-compatible interfaces for the historic UNIX <a href="../../api/c/dbm.md" class="olink">dbm</a>, <a href="../../api/c/dbm.md" class="olink">ndbm</a> and <a href="../../api/c/hsearch.md" class="olink">hsearch</a> interfaces. It also provides a backward-compatible interface for the historic Berkeley DB 1.85 release.

Berkeley DB version 2 does not provide database compatibility for any of the previous interfaces, and existing databases must be converted manually. To convert existing databases from the Berkeley DB 1.85 format to the Berkeley DB version 2 format, review the <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility and the <a href="../../api/c/db_load.md" class="olink">db_load</a> utility information. No utilities are provided to convert UNIX <a href="../../api/c/dbm.md" class="olink">dbm</a>, <a href="../../api/c/dbm.md" class="olink">ndbm</a> or <a href="../../api/c/hsearch.md" class="olink">hsearch</a> databases.
