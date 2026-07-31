---
title: "set_intermediate_dir_mode"
api-name: "set_intermediate_dir_mode"
source: docs/api_reference/C/set_intermediate_dir_mode_parameter.html
---
## set_intermediate_dir_mode

Configures the database environment's intermediate directory permissions.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_intermediate_dir_mode`, one or more whitespace characters, and the directory permissions.

Directory permissions are interpreted as a string of nine characters, using the character set **r** (read), **w** (write), **x** (execute or search), and **-** (none). The first character is the read permissions for the directory owner (set to either **r** or **-**). The second character is the write permissions for the directory owner (set to either **w** or **-**). The third character is the execute permissions for the directory owner (set to either **x** or **-**).

Similarly, the second set of three characters are the read, write and execute/search permissions for the directory group, and the third set of three characters are the read, write and execute/search permissions for all others. For example, the string **rwx------** would configure read, write and execute/search access for the owner only. The string **rwxrwx---** would configure read, write and execute/search access for both the owner and the group. The string **rwxr-----** would configure read, write and execute/search access for the directory owner and read-only access for the directory group.

For more information, see <a href="envset_intermediate_dir_mode.md" class="xref" title="DB_ENV-&gt;set_intermediate_dir_mode()">DB_ENV-&gt;set_intermediate_dir_mode()</a>.
