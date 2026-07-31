---
title: "Security"
api-name: "Security"
source: docs/programmer_reference/env_security.html
---
## Security

The following are security issues that should be considered when writing Berkeley DB applications:

<span class="term">Database environment permissions</span>  
The directory used as the Berkeley DB database environment should have its permissions set to ensure that files in the environment are not accessible to users without appropriate permissions. Applications that add to the user's permissions (for example, UNIX setuid or setgid applications), must be carefully checked to not permit illegal use of those permissions such as general file access in the environment directory.

<span class="term">Environment variables</span>  
Setting the <a href="../../api/c/envopen.md#envopen_DB_USE_ENVIRON" class="olink">DB_USE_ENVIRON</a> and <a href="../../api/c/envopen.md#envopen_DB_USE_ENVIRON_ROOT" class="olink">DB_USE_ENVIRON_ROOT</a> flags and allowing the use of environment variables during file naming can be dangerous. Setting those flags in Berkeley DB applications with additional permissions (for example, UNIX setuid or setgid applications) could potentially allow users to read and write databases to which they would not normally have access.

<span class="term">File permissions</span>  
By default, Berkeley DB always creates files readable and writable by the owner and the group (that is, S_IRUSR, S_IWUSR, S_IRGRP and S_IWGRP; or octal mode 0660 on historic UNIX systems). The group ownership of created files is based on the system and directory defaults, and is not further specified by Berkeley DB.

<span class="term">Temporary backing files</span>  
If an unnamed database is created and the cache is too small to hold the database in memory, Berkeley DB will create a temporary physical file to enable it to page the database to disk as needed. In this case, environment variables such as **TMPDIR** may be used to specify the location of that temporary file. Although temporary backing files are created readable and writable by the owner only (S_IRUSR and S_IWUSR, or octal mode 0600 on historic UNIX systems), some filesystems may not sufficiently protect temporary files created in random directories from improper access. To be absolutely safe, applications storing sensitive data in unnamed databases should use the <a href="../../api/c/envset_tmp_dir.md" class="olink">DB_ENV-&gt;set_tmp_dir()</a> method to specify a temporary directory with known permissions.

<span class="term">Tcl API</span>  
The Berkeley DB Tcl API does not attempt to avoid evaluating input as Tcl commands. For this reason, it may be dangerous to pass unreviewed user input through the Berkeley DB Tcl API, as the input may subsequently be evaluated as a Tcl command. Additionally, the Berkeley DB Tcl API initialization routine resets process' effective user and group IDs to the real user and group IDs, to minimize the effectiveness of a Tcl injection attack.
