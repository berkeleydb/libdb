---
title: "mutex_set_tas_spins"
api-name: "mutex_set_tas_spins"
source: docs/api_reference/C/mutex_set_tas_spins_parameter.html
---
## mutex_set_tas_spins

Specifies the number of times the test-and-set mutexes should spin without blocking. The value defaults to 1 time on uniprocessor systems and to 50 times the number of processors on multiprocessor systems.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_tas_spins`, one or more whitespace characters, and the number of spins.

For more information, see <a href="mutexset_tas_spins.md" class="xref" title="DB_ENV-&gt;mutex_set_tas_spins()">DB_ENV-&gt;mutex_set_tas_spins()</a>.
