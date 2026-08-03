---
title: "Java CLASSPATH environment variable"
api-name: "Java CLASSPATH environment variable"
source: docs/upgrading/upgrade_4_0_java.html
---
## Java CLASSPATH environment variable

The Berkeley DB Java class files are now packaged as jar files. In the 4.0 release, the `CLASSPATH` environment variable must change to include at least the `db.jar` file. It can optionally include the `dbexamples.jar` file if you want to run the examples. For example, on UNIX:

``` c
export CLASSPATH="/usr/local/BerkeleyDB.4.8/lib/db.jar: \
/usr/local/BerkeleyDB.4.8/lib/dbexamples.jar"
```

For example, on Windows:

``` c
set CLASSPATH="D:\db\build_windows\Release\db.jar;
D:\db\build_windows\Release\dbexamples.jar"
```

For more information on Java configuration, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/installation/index.html" class="ulink" target="_top">Berkeley DB Installation and Build Guide.</a> .
