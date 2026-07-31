---
title: "Environment variables"
api-name: "Environment variables"
source: docs/programmer_reference/program_environ.html
---
## Environment variables

The Berkeley DB library uses the following environment variables:

<span class="term">DB_HOME</span>  
If the environment variable DB_HOME is set, it is used as part of <a href="env_naming.md" class="xref" title="File naming">File naming</a>. Note: For the DB_HOME variable to take effect, either the <a href="../../api/c/envopen.md#envopen_DB_USE_ENVIRON" class="olink">DB_USE_ENVIRON</a> or <a href="../../api/c/envopen.md#envopen_DB_USE_ENVIRON_ROOT" class="olink">DB_USE_ENVIRON_ROOT</a> flags must be specified to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>.

<span class="term">TMPDIR, TEMP, TMP, TempFolder</span>  
The TMPDIR, TEMP, TMP, and TempFolder environment variables are all checked as locations in which to create temporary files. See <a href="../../api/c/envset_tmp_dir.md" class="olink">DB_ENV-&gt;set_tmp_dir()</a> for more information.
