---
title: "Chapter 5.  Java API"
api-name: "Chapter 5.  Java API"
source: docs/programmer_reference/java.html
---
## Chapter 5.  Java API

**Table of Contents**

<span class="sect1"> [Java configuration](java.md#java_conf) </span>

<span class="sect1"> [Compatibility](java_compat.md) </span>

<span class="sect1"> [Java programming notes](java_program.md) </span>

<span class="sect1"> [Java FAQ](java_faq.md) </span>

## Java configuration

Building the Berkeley DB java classes, the examples and the native support library is integrated into the normal build process. See <a href="../../guides/installation/build_unix_conf.md" class="olink">Configuring Berkeley DB</a> and <a href="../../guides/installation/build_win_java.md" class="olink">Building the Java API</a> in the Berkeley DB Installation and Build Guide for more information.

We expect that you already installed the Java JDK or equivalent on your system. For the sake of discussion, we assume that it is in a directory called db-VERSION; for example, you downloaded a Berkeley DB archive, and you did not change the top-level directory name. The files related to Java are in three subdirectories of db-VERSION: java (the java source files), libdb_java (the C++ files that provide the "glue" between java and Berkeley DB) and examples_java (containing all examples code). The directory tree looks like this:

``` c
db-VERSION
|-- java
|   `-- src
|       `-- com
|           `-- sleepycat
|               |-- bind
|               |-- db
|               |   `-- ...
|               `-- util
|-- examples_java
|   `-- src
|       `-- db
|           `-- ...
`-- libdb_java
    `-- ...
```

This naming conforms to the de facto standard for naming java packages. When the java code is built, it is placed into two jar files: `db.jar`, containing the db package, and `dbexamples.jar`, containing the examples.

For your application to use Berkeley DB successfully, you must set your `CLASSPATH` environment variable to include the full pathname of the db jar files as well as the classes in your java distribution. On UNIX, `CLASSPATH` is a colon-separated list of directories and jar files; on Windows, it is separated by semicolons. On UNIX, the jar files are put in your build directory, and when you do the make install step, they are copied to the lib directory of your installation tree. On Windows, the jar files are placed in the Release or Debug subdirectory with your other objects.

The Berkeley DB Java classes are mostly implemented in native methods. Before you can use them, you need to make sure that the DLL or shared library containing the native methods can be found by your Java runtime. On Windows, you should set your PATH variable to include:

``` c
          db-VERSION\build_windows\Release
        
```

On UNIX, you should set the `LD_LIBRARY_PATH` environment variable or local equivalent to include the Berkeley DB library installation directory. Of course, the standard install directory may have been changed for your site; see your system administrator for details.

On other platforms, the path can be set on the command line as follows (assuming the shared library is in `/usr/local/BerkeleyDB/lib`:)

``` c
% java -Djava.library.path=/usr/local/BerkeleyDB/lib ...
```

Regardless, if you get the following exception when you run, you probably do not have the library search path configured correctly:

``` c
java.lang.UnsatisfiedLinkError
```

Different Java interpreters provide different error messages if the `CLASSPATH` value is incorrect, a typical error is the following:

``` c
java.lang.NoClassDefFoundError
```

To ensure that everything is running correctly, you may want to try a simple test from the example programs in

``` c
          db-VERSION/examples_java/src/db
        
```

For example, the following sample program will prompt for text input lines, which are then stored in a Btree database named `access.db` in your current directory:

``` c
% java db.AccessExample
```

Try giving it a few lines of input text and then end-of-file. Before it exits, you should see a list of the lines you entered display with data items. This is a simple check to make sure the fundamental configuration is working correctly.
