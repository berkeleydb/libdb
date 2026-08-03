---
title: "Chapter 23.  Dumping and Reloading Databases"
api-name: "Chapter 23.  Dumping and Reloading Databases"
source: docs/programmer_reference/dumpload.html
---
## Chapter 23.  Dumping and Reloading Databases

**Table of Contents**

<span class="sect1"> [The db_dump and db_load utilities](dumpload.md#dumpload_utility) </span>

<span class="sect1"> [Dump output formats](dumpload_format.md) </span>

<span class="sect1"> [Loading text into databases](dumpload_text.md) </span>

## The db_dump and db_load utilities

There are three utilities used for dumping and loading Berkeley DB databases: the <a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility, the <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility and the <a href="../../api/c/db_load.md" class="olink">db_load</a> utility.

The <a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility and the <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility dump Berkeley DB databases into a flat-text representation of the data that can be read by <a href="../../api/c/db_load.md" class="olink">db_load</a> utility. The only difference between them is that the <a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility reads Berkeley DB version 2 and greater database formats, whereas the <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility reads Berkeley DB version 1.85 and 1.86 database formats.

The <a href="../../api/c/db_load.md" class="olink">db_load</a> utility reads either the output format used by the dump utilities or (optionally) a flat-text representation created using other tools, and stores it into a Berkeley DB database.

Dumping and reloading Hash databases that use user-defined hash functions will result in new databases that use the default hash function. Although using the default hash function may not be optimal for the new database, it will continue to work correctly.

Dumping and reloading Btree databases that use user-defined prefix or comparison functions will result in new databases that use the default prefix and comparison functions. In this case, it is quite likely that applications will be unable to retrieve records, and it is possible that the load process itself will fail.

The only available workaround for either Hash or Btree databases is to modify the sources for the <a href="../../api/c/db_load.md" class="olink">db_load</a> utility to load the database using the correct hash, prefix, and comparison functions.
