---
title: "Verbose Output"
api-name: "Verbose Output"
source: docs/upgrading/upgrade_4_6_verbose.html
---
## Verbose Output

When an error occurs in the Berkeley DB library, an exception is thrown or an error return value is returned by the interface. In some cases, however, the exception or returned value may be insufficient to completely describe the cause of the error, especially during initial application debugging. Applications can configure Berkeley DB for verbose messages to be output when an error occurs, but it's a common cause of confusion for new users that no verbose messages are available by default.

In the Berkeley DB 4.6 release, verbose messages are configured by default. For the C and C++ APIs, this means the default configuration when applications first create <a href="../../api/c/db.md" class="olink">DB</a> or <a href="../../api/c/env.md" class="olink">DB_ENV</a> handles is as if the <a href="../../api/c/envset_errfile.md" class="olink">DB_ENV-&gt;set_errfile()</a> or <a href="../../api/c/dbset_errfile.md" class="olink">DB-&gt;set_errfile()</a> methods were called with the standard error output (stderr) specified as the FILE \* argument. Applications wanting no output at all can turn off this default configuration by calling the <a href="../../api/c/envset_errfile.md" class="olink">DB_ENV-&gt;set_errfile()</a> or <a href="../../api/c/dbset_errfile.md" class="olink">DB-&gt;set_errfile()</a> methods with NULL as the FILE \* argument. Additionally, explicitly configuring the error output channel using any of the <a href="../../api/c/envset_errfile.md" class="olink">DB_ENV-&gt;set_errfile()</a>, <a href="../../api/c/envset_errcall.md" class="olink">DB_ENV-&gt;set_errcall()</a>, <a href="../api_reference/CXX/envset_error_stream.html" class="olink">DbEnv::set_error_stream()</a> or <a href="../api_reference/CXX/dbset_error_stream.html" class="olink">Db::set_error_stream()</a> methods will also turn off this default output for the application.

Applications which configure Berkeley DB with any error output channel should not require any changes.

Applications which depend on having no output from the Berkeley DB library by default, should be changed to call the <a href="../../api/c/envset_errfile.md" class="olink">DB_ENV-&gt;set_errfile()</a> or <a href="../../api/c/dbset_errfile.md" class="olink">DB-&gt;set_errfile()</a> methods with NULL as the FILE \* argument.
