---
title: "Dump output formats"
api-name: "Dump output formats"
source: docs/programmer_reference/dumpload_format.html
---
## Dump output formats

There are two output formats used by the <a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility and <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility.

In both output formats, the first few lines of the output contain header information describing the underlying access method, filesystem page size, and other bookkeeping information.

The header information starts with a single line, VERSION=N, where N is the version number of the dump output format.

The header information is then output in name=value pairs, where name may be any of the keywords listed in the <a href="../../api/c/db_load.md" class="olink">db_load</a> utility manual page, and value will be its value. Although this header information can be manually edited before the database is reloaded, there is rarely any reason to do so because all of this information can also be specified or overridden by command-line arguments to the <a href="../../api/c/db_load.md" class="olink">db_load</a> utility.

The header information ends with single line HEADER=END.

Following the header information are the key/data pairs from the database. If the database being dumped is a Btree or Hash database, or if the **-k** option was specified, the output will be paired lines of text where the first line of the pair is the key item, and the second line of the pair is its corresponding data item. If the database being dumped is a Queue or Recno database, and the **-k** option was not specified, the output will be lines of text where each line is the next data item for the database. Each of these lines is preceded by a single space.

If the **-p** option was specified to the <a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility or <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility, the key/data lines will consist of single characters representing any characters from the database that are <span class="emphasis">*printing characters*</span> and backslash (**\\**) escaped characters for any that were not. Backslash characters appearing in the output mean one of two things: if the backslash character precedes another backslash character, it means that a literal backslash character occurred in the key or data item. If the backslash character precedes any other character, the next two characters must be interpreted as hexadecimal specification of a single character; for example, **\0a** is a newline character in the ASCII character set.

Although some care should be exercised, it is perfectly reasonable to use standard text editors and tools to edit databases dumped using the **-p** option before reloading them using the <a href="../../api/c/db_load.md" class="olink">db_load</a> utility.

Note that the definition of a printing character may vary from system to system, so database representations created using the **-p** option may be less portable than those created without it.

If the **-p** option in not specified to <a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility or <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility, each output line will consist of paired hexadecimal values; for example, the line **726f6f74** is the string **root** in the ASCII character set.

In all output formats, the key and data items are ended by a single line DATA=END.

Where multiple databases have been dumped from a file, the overall output will repeat; that is, a new set of headers and a new set of data items.
