---
title: "DB_ENV->set_metadata_dir()"
api-name: "DB_ENV->set_metadata_dir()"
source: docs/api_reference/C/envset_metadata_dir.html
---
## DB_ENV-\>set_metadata_dir()

``` c
#include <db.h>

int
DB_ENV->set_metadata_dir(DB_ENV *envp, const char *dir); 
```

The `DB_ENV->set_metadata_dir()` method sets the directory where persistent metadata is stored. By default, persistent metadata is stored in the environment home directory.

When used in a replicated application, the metadata directory must be the same location for all sites within a replication group.

The `DB_ENV->set_metadata_dir()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. The directory identified by this method must already exist when the `DB_ENV->open()` method is called. The directory identified by this method is added to the environment's list of data directories, if this directory is not already included on that list.

The database environment's metadata directory may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_metadata_dir", one or more whitespace characters, followed by the directory location. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_metadata_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dir

The **dir** parameter identifies the directory used to store persistent metadata files.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envget_metadata_dir.md" class="xref" title="DB_ENV-&gt;get_metadata_dir()">DB_ENV-&gt;get_metadata_dir()</a>
