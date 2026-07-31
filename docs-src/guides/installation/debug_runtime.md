---
title: "Run-time error information"
api-name: "Run-time error information"
source: docs/installation/debug_runtime.html
---
## Run-time error information

Normally, when an error occurs in the Berkeley DB library, an integer value (either a Berkeley DB specific value or a system `errno` value) is returned by Berkeley DB. In some cases, however, this value may be insufficient to completely describe the cause of the error, especially during initial application debugging.

Most Berkeley DB errors will result in additional information being written to a standard file descriptor or output stream. Additionally, Berkeley DB can be configured to pass these verbose error messages to an application function. There are four methods intended to provide applications with additional error information: <a href="../../api/c/envset_errcall.md" class="olink">DB_ENV-&gt;set_errcall()</a>, <a href="../../api/c/envset_errfile.md" class="olink">DB_ENV-&gt;set_errfile()</a>, <a href="../../api/c/envset_errpfx.md" class="olink">DB_ENV-&gt;set_errpfx()</a> and <a href="../../api/c/envset_verbose.md" class="olink">DB_ENV-&gt;set_verbose()</a>.

The Berkeley DB error-reporting facilities do not slow performance or significantly increase application size, and may be run during normal operation as well as during debugging. Where possible, we recommend these options always be configured and the output saved in the filesystem. We have found that this often saves time when debugging installation or other system-integration problems.

In addition, there are three methods to assist applications in displaying their own error messages: <a href="../../api/c/envstrerror.md" class="olink">db_strerror()</a>, <a href="../../api/c/enverr.md" class="olink">DB_ENV-&gt;err()</a>, and `DB_ENV->errx()`. The first is a superset of the ANSI C strerror function, and returns a descriptive string for any error return from the Berkeley DB library. The <a href="../../api/c/enverr.md" class="olink">DB_ENV-&gt;err()</a> and `DB_ENV->errx()` methods use the error message configuration options described previously to format and display error messages to appropriate output devices.
