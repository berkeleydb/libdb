---
title: "DB_ENV->rep_set_clockskew()"
api-name: "DB_ENV->rep_set_clockskew()"
source: docs/api_reference/C/repclockskew.html
---
## DB_ENV-\>rep_set_clockskew()

``` c
#include <db.h>

int
DB_ENV->rep_set_clockskew(DB_ENV *env,
    u_int32_t fast_clock, u_int32_t slow_clock);  
```

The `DB_ENV->rep_set_clockskew()` method sets the clock skew ratio among replication group members based on the fastest and slowest measurements among the group for use with master leases. Calling this method is optional; the default values for clock skew assume no skew. The user must also configure leases via the <a href="repconfig.md" class="xref" title="DB_ENV-&gt;rep_set_config()">DB_ENV-&gt;rep_set_config()</a> method. Additionally, the user must also set the master lease timeout via the <a href="repset_timeout.md" class="xref" title="DB_ENV-&gt;rep_set_timeout()">DB_ENV-&gt;rep_set_timeout()</a> method. For Base API applications, the user must also set the number of sites in the replication group via the <a href="repnsites.md" class="xref" title="DB_ENV-&gt;rep_set_nsites()">DB_ENV-&gt;rep_set_nsites()</a> method. These methods may be called in any order. For a description of the clock skew values, see <a href="../../programmer_reference/rep_clock_skew.html" class="olink">Clock skew</a> in the *Berkeley DB Programmer's Reference Guide*. For a description of master leases, see <a href="../../programmer_reference/rep_lease.html" class="olink">Master leases</a> in the *Berkeley DB Programmer's Reference Guide*.

These arguments can be used to express either raw measurements of a clock timing experiment or a percentage across machines. For example, if a group of sites has a 2% variance, then **fast_clock** should be set to 102, and **slow_clock** should be set to 100. Or, for a 0.03% difference, you can use 10003 and 10000 respectively.

The database environment's replication subsystem may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "rep_set_clockskew", one or more whitespace characters, and the clockskew specified in two parts: the fast_clock and the slow_clock. For example, "rep_set_clockskew 102 100". Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->rep_set_clockskew()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->rep_set_clockskew()` method may not be called after the <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> or <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> methods are called.

The `DB_ENV->rep_set_clockskew()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### fast_clock

The value, relative to the **slow_clock**, of the fastest clock in the group of sites.

#### slow_clock

The value of the slowest clock in the group of sites.

### Errors

The `DB_ENV->rep_set_clockskew()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after replication is started with a call to the <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> or the <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> method; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
