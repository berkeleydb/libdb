---
title: "DB_ENV->rep_set_nsites()"
api-name: "DB_ENV->rep_set_nsites()"
source: docs/api_reference/C/repnsites.html
---
## DB_ENV-\>rep_set_nsites()

``` c
#include <db.h>

int
DB_ENV->rep_set_nsites(DB_ENV *env, u_int32_t nsites);  
```

The `DB_ENV->rep_set_nsites()` method specifies the total number of sites in a replication group. This method should not be used by Replication Manager applications; the number of sites in use by a Replication Manager application is determined dynamically.

The `DB_ENV->rep_set_nsites()` method is typically called by Base API applications. (However, see also the <a href="repelect.md" class="xref" title="DB_ENV-&gt;rep_elect()">DB_ENV-&gt;rep_elect()</a> method **nsites** parameter.)

The database environment's replication subsystem may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "rep_set_nsites", one or more whitespace characters, and the number of sites specified. For example, "rep_set_nsites 5" sets the number of sites to 5. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->rep_set_nsites()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

If master leases are in use, the `DB_ENV->rep_set_nsites()` method should not be called after the <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> method is called as this could cause you to lose data previously thought to be durable. If master leases are not in use, the `DB_ENV->rep_set_nsites()` method may be called at any time during the life of the application.

The `DB_ENV->rep_set_nsites()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### nsites

An integer specifying the total number of sites in the replication group.

### Errors

The `DB_ENV->rep_set_nsites()` method may fail and return one of the following non-zero errors:

#### EINVAL

If master leases are in use and replication has already been started; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
