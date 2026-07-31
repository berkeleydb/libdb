---
title: "set_shm_key"
api-name: "set_shm_key"
source: docs/api_reference/C/set_shm_key_parameter.html
---
## set_shm_key

Configures the database environment's base segment ID. This base segment ID will be used when Berkeley DB shared memory regions are first created. It will be incremented a small integer value each time a new shared memory region is created; that is, if the base ID is 35, the first shared memory region created will have a segment ID of 35, and the next one will have a segment ID between 36 and 40 or so.

See <a href="../../guides/programmer_reference/env_region.md" class="olink">Shared Memory Regions</a> for more information.

The syntax of the entry in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_shm_key` one or more whitespace characters, and the ID.

For more information, see <a href="envset_shm_key.md" class="xref" title="DB_ENV-&gt;set_shm_key()">DB_ENV-&gt;set_shm_key()</a>.
