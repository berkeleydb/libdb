---
title: "Loading text into databases"
api-name: "Loading text into databases"
source: docs/programmer_reference/dumpload_text.html
---
## Loading text into databases

The <a href="../../api/c/db_load.md" class="olink">db_load</a> utility can be used to load text into databases. The **-T** option permits nondatabase applications to create flat-text files that are then loaded into databases for fast, highly-concurrent access. For example, the following command loads the standard UNIX `/etc/passwd` file into a database, with the login name as the key item and the entire password entry as the data item:

``` c
awk -F: '{print $1; print $0}' < /etc/passwd |\
    sed 's/\\/\\\\/g' | db_load -T -t hash passwd.db
```

Note that backslash characters naturally occurring in the text are escaped to avoid interpretation as escape characters by the <a href="../../api/c/db_load.md" class="olink">db_load</a> utility.
