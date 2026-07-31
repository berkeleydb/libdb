---
title: "File naming"
api-name: "File naming"
source: docs/programmer_reference/env_naming.html
---
## File naming

<span class="sect2"> [Specifying file naming to Berkeley DB](env_naming.md#idp51749352) </span>

<span class="sect2"> [Filename resolution in Berkeley DB](env_naming.md#idp51763728) </span>

<span class="sect2"> [Examples](env_naming.md#idp51756464) </span>

One of the most important tasks of the database environment is to structure file naming within Berkeley DB. Cooperating applications (or multiple invocations of the same application) must agree on the location of the database environment, log files and other files used by the Berkeley DB subsystems, and, of course, the database files. Although it is possible to specify full pathnames to all Berkeley DB methods, this is cumbersome and requires applications be recompiled when database files are moved.

Applications are normally expected to specify a single directory home for the database environment. This can be done easily in the call to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> by specifying a value for the **db_home** argument. There are more complex configurations in which it may be desirable to override **db_home** or provide supplementary path information.

### Specifying file naming to Berkeley DB

The following list describes the possible ways in which file naming information may be specified to the Berkeley DB library. The specific circumstances and order in which these ways are applied are described in a subsequent paragraph.

<span class="term">db_home</span>  
If the **db_home** argument to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> is non-NULL, its value may be used as the database home, and files named relative to its path.

<span class="term">DB_HOME</span>  
If the DB_HOME environment variable is set when <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> is called, its value may be used as the database home, and files named relative to its path.

The DB_HOME environment variable is intended to permit users and system administrators to override application and installation defaults. For example:

``` c
env DB_HOME=/database/my_home application
```

Application writers are encouraged to support the **-h** option found in the supporting Berkeley DB utilities to let users specify a database home.

<span class="term"><a href="../../api/c/env.md" class="olink">DB_ENV</a> methods</span>  
There are four <a href="../../api/c/env.md" class="olink">DB_ENV</a> methods that affect file naming:

- The <a href="../../api/c/envadd_data_dir.md" class="olink">DB_ENV-&gt;add_data_dir()</a> method specifies a directory to search for database files.

- The <a href="../../api/c/envset_lg_dir.md" class="olink">DB_ENV-&gt;set_lg_dir()</a> method specifies a directory in which to create logging files.

- The <a href="../../api/c/envset_tmp_dir.md" class="olink">DB_ENV-&gt;set_tmp_dir()</a> method specifies a directory in which to create backing temporary files.

- The <a href="../../api/c/envset_metadata_dir.md" class="olink">DB_ENV-&gt;set_metadata_dir()</a> method specifies the directory in which to create persistent metadata files used by the environment.

These methods are intended to permit applications to customize a file locations for an environment. For example, an application writer can place data files and log files in different directories or instantiate a new log directory each time the application runs.

<span class="term"> <a href="env_db_config.md" class="link" title="DB_CONFIG configuration file">DB_CONFIG</a> </span>  
The same information specified to the <a href="../../api/c/env.md" class="olink">DB_ENV</a> methods may also be specified using the <a href="env_db_config.md" class="link" title="DB_CONFIG configuration file">DB_CONFIG</a> configuration file.

### Filename resolution in Berkeley DB

The following list describes the specific circumstances and order in which the different ways of specifying file naming information are applied. Berkeley DB filename processing proceeds sequentially through the following steps:

<span class="term">absolute pathnames</span>  
If the filename specified to a Berkeley DB function is an <span class="emphasis">*absolute pathname*</span>, that filename is used without modification by Berkeley DB.

On UNIX systems, an absolute pathname is defined as any pathname that begins with a leading slash (**/**).

On Windows systems, an absolute pathname is any pathname that begins with a leading slash or leading backslash (**\\**); or any pathname beginning with a single alphabetic character, a colon and a leading slash or backslash (for example, `C:/tmp`).

<span class="term"><a href="../../api/c/env.md" class="olink">DB_ENV</a> methods, DB_CONFIG</span>  
If a relevant configuration string (for example, set_data_dir), is specified either by calling a <a href="../../api/c/env.md" class="olink">DB_ENV</a> method or as a line in the <a href="env_db_config.md" class="link" title="DB_CONFIG configuration file">DB_CONFIG</a> configuration file, the value is prepended to the filename. If the resulting filename is an absolute pathname, the filename is used without further modification by Berkeley DB.

<span class="term">db_home</span>  
If the application specified a non-NULL **db_home** argument to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>, its value is prepended to the filename. If the resulting filename is an absolute pathname, the filename is used without further modification by Berkeley DB.

<span class="term">DB_HOME</span>  
If the **db_home** argument is NULL, the DB_HOME environment variable was set, and the application has set the appropriate <a href="../../api/c/envopen.md#envopen_DB_USE_ENVIRON" class="olink">DB_USE_ENVIRON</a> or <a href="../../api/c/envopen.md#envopen_DB_USE_ENVIRON_ROOT" class="olink">DB_USE_ENVIRON_ROOT</a> flags, its value is prepended to the filename. If the resulting filename is an absolute pathname, the filename is used without further modification by Berkeley DB.

<span class="term">default</span>  
Finally, all filenames are interpreted relative to the current working directory of the process.

The common model for a Berkeley DB environment is one in which only the DB_HOME environment variable, or the **db_home** argument is specified. In this case, all data filenames are relative to that directory, and all files created by the Berkeley DB subsystems will be created in that directory.

The more complex model for a transaction environment might be one in which a database home is specified, using either the DB_HOME environment variable or the **db_home** argument to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>; and then the data directory and logging directory are set to the relative pathnames of directories underneath the environment home.

### Examples

Store all files in the directory `/a/database`:

``` c
dbenv->open(dbenv, "/a/database", flags, mode);
```

Create temporary backing files in `/b/temporary`, and all other files in `/a/database`:

``` c
dbenv->set_tmp_dir(dbenv, "/b/temporary");
dbenv->open(dbenv, "/a/database", flags, mode);
```

Store data files in `/a/database/datadir`, log files in `/a/database/logdir`, and all other files in the directory `/a/database`:

``` c
dbenv->set_lg_dir(dbenv, "logdir");
dbenv->set_data_dir(dbenv, "datadir");
dbenv->open(dbenv, "/a/database", flags, mode);
```

Store data files in `/a/database/data1` and `/b/data2`, and all other files in the directory `/a/database`. Any data files that are created will be created in `/b/data2`, because it is the first data file directory specified:

``` c
dbenv->set_data_dir(dbenv, "/b/data2");
dbenv->set_data_dir(dbenv, "data1");
dbenv->open(dbenv, "/a/database", flags, mode);
```
