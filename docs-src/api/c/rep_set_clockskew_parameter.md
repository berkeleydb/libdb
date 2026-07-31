---
title: "rep_set_clockskew"
api-name: "rep_set_clockskew"
source: docs/api_reference/C/rep_set_clockskew_parameter.html
---
## rep_set_clockskew

Sets the clock skew ratio among replication group members based on the fastest and slowest measurements among the group for use with master leases.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `rep_set_clockskew`, one or more whitespace characters, and the clockskew specified in two parts: the fast_clock and the slow_clock.

For example:

``` c
rep_set_clockskew 102 100
```

Sets the fast_clock to 102 and the slow_clock to 100 if a group of sites has a 2% variance.

For more information, see <a href="repclockskew.md" class="xref" title="DB_ENV-&gt;rep_set_clockskew()">DB_ENV-&gt;rep_set_clockskew()</a>.
