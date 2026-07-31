---
title: "db_tuner"
api-name: "db_tuner"
source: docs/api_reference/C/db_tuner.html
---
## db_tuner

``` c
db_tuner [-c cachesize] -d file [-h home] [-s database] [-v]  
```

The <span class="command">**db_tuner**</span> utility analyzes the data in a btree database, and suggests a page size that is likely to deliver optimal operation.

### Note

The <span class="command">**db_tuner**</span> utility assumes that databases are compacted when analysing the data. The analysis is based on a static view of the data and the data access and update patterns are not take into account.

The options are as follows:

- **-c**

  Specify a value of the cachesize, otherwise, the default value will be set.

- **-d**

  Display database statistics for the specified file. If the database contains multiple databases and the -s flag is not specified, the statistics are for the internal database that describes the other databases the file contains, and not for the file as a whole.

- **-h**

  Specify a home directory for the database environment.

- **-s**

  Display page size recommendation for the specified database contained in the file specified with the -d flag.

- **-v**

  Display verbose information.
