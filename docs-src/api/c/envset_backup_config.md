---
title: "DB_ENV->set_backup_config()"
api-name: "DB_ENV->set_backup_config()"
source: docs/api_reference/C/envset_backup_config.html
---
## DB_ENV-\>set_backup_config()

``` c
#include <db.h>
 
DB_ENV->set_backup_config(DB_ENV, db_backup_config_t option, 
                          u_int32_t value);  
```

The `DB_ENV->set_backup_config()` method configures tuning parameters for the hot backup APIs. See the <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a> and <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a> methods for a description of the hot backup APIs.

The `DB_ENV->set_backup_config()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_backup_config()` method may be called at any time during the life of the application.

The `DB_ENV->set_backup_config()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### option

The **option** parameter identifies the backup parameter to be modified. It must be one of the following:

- `DB_BACKUP_WRITE_DIRECT`

  Turning this on causes direct I/O to be used when writing pages to the disk. For some environments, direct I/O can provide faster write throughput, but usually it is slower because the OS buffer pool offers asynchronous activity.

  By default, this option is turned off.

- `DB_BACKUP_READ_COUNT`

  Configures the number of pages to read before pausing. Increasing this value increases the amount of I/O the backup process performs for any given time interval. If your application is already heavily I/O bound, setting this value to a lower number may help to improve your overall data throughput by reducing the I/O demands placed on your system.

  By default, all pages are read without a pause.

- `DB_BACKUP_READ_SLEEP`

  Configures the number of microseconds to sleep between batches of reads. Increasing this value decreases the amount of I/O the backup process performs for any given time interval. If your application is already heavily I/O bound, setting this value to a higher number may help to improve your overall data throughput by reducing the I/O demands placed on your system.

- `DB_BACKUP_SIZE`

  Configures the size of the buffer, in bytes, to read from the database. Default is 1 megabyte.

#### value

The **value** parameter sets the configuration value for the option identified by the **option** parameter. For those options which can only be turned on or off, this parameter should be set to `0` for off and `1` for on. Otherwise, set this parameter to an integer value that represents the number of units for which you are configuring the backup APIs.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>,

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envget_backup_config.md" class="xref" title="DB_ENV-&gt;get_backup_config()">DB_ENV-&gt;get_backup_config()</a>, <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a>, <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a>
