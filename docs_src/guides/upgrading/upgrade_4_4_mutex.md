---
title: "mutexes"
api-name: "mutexes"
source: docs/upgrading/upgrade_4_4_mutex.html
---
## mutexes

The DB_ENV\>set_tas_spins and DB_ENV\>get_tas_spins methods have been renamed to <a href="../../api/c/mutexset_tas_spins.md" class="olink">DB_ENV-&gt;mutex_set_tas_spins()</a> and <a href="../../api/c/mutexset_tas_spins.md" class="olink">DB_ENV-&gt;mutex_set_tas_spins()</a> to match the new mutex support in the Berkeley DB 4.4 release. Applications calling the old methods should be updated to use the new method names.

For backward compatibility, the string "set_tas_spins" is still supported in <a href="../../guides/programmer_reference/env_db_config.md" class="olink">DB_CONFIG</a> files.

The --with-mutexalign="ALIGNMENT" compile-time configuration option has been removed from Berkeley DB configuration. Mutex alignment should now be configured at run-time, using the <a href="../../api/c/mutexset_align.md" class="olink">DB_ENV-&gt;mutex_set_align()</a> method.
