---
title: "Name spaces"
api-name: "Name spaces"
source: docs/programmer_reference/program_namespace.html
---
## Name spaces

<span class="sect2"> [C Language Name Space](program_namespace.md#idp52962960) </span>

<span class="sect2"> [Filesystem Name Space](program_namespace.md#idp53001824) </span>

### C Language Name Space

The Berkeley DB library is careful to avoid C language programmer name spaces, but there are a few potential areas for concern, mostly in the Berkeley DB include file db.h. The db.h include file defines a number of types and strings. Where possible, all of these types and strings are prefixed with "DB\_" or "db\_". There are a few notable exceptions.

The Berkeley DB library uses a macro named "\_\_P" to configure for systems that do not provide ANSI C function prototypes. This could potentially collide with other systems using a "\_\_P" macro for similar or different purposes.

The Berkeley DB library needs information about specifically sized types for each architecture. If they are not provided by the system, they are typedef'd in the db.h include file. The types that may be typedef'd by db.h include the following: u_int8_t, int16_t, u_int16_t, int32_t, u_int32_t, u_char, u_short, u_int, and u_long.

The Berkeley DB library declares a few external routines. All these routines are prefixed with the strings "db\_". All internal Berkeley DB routines are prefixed with the strings "\_\_XXX\_", where "XXX" is the subsystem prefix (for example, "\_\_db_XXX\_" and "\_\_txn_XXX\_").

### Filesystem Name Space

Berkeley DB environments create or use some number of files in environment home directories. These files are named <a href="env_db_config.md" class="link" title="DB_CONFIG configuration file">DB_CONFIG</a>, "log.NNNNN" (for example, log.0000000003, where the number of digits following the dot is unspecified), or with the string prefix "\_\_db" (for example, \_\_db.001). Applications should never create files or databases in database environment home directories with names beginning with the characters "log" or "\_\_db".

In some cases, applications may choose to remove Berkeley DB files as part of their cleanup procedures, using system utilities instead of Berkeley DB interfaces (for example, using the UNIX rm utility instead of the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method). This is not a problem, as long as applications limit themselves to removing only files named "\_\_db.###", where "###" are the digits 0 through 9. Applications should never remove any files named with the prefix "\_\_db" or "log", other than "\_\_db.###" files.
