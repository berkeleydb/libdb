---
title: "set_mp_mmapsize"
api-name: "set_mp_mmapsize"
source: docs/api_reference/C/set_mp_mmapsize_parameter.html
---
## set_mp_mmapsize

Sets the maximum file size, in bytes, for a file to be mapped into the process address space. If no value is specified, it defaults to 10MB.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_mp_mmapsize`, one or more whitespace characters, and the size in bytes.

For more information, see <a href="envset_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;set_mp_mmapsize()">DB_ENV-&gt;set_mp_mmapsize()</a>.
