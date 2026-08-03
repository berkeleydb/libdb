---
title: "Tcl API programming notes"
api-name: "Tcl API programming notes"
source: docs/programmer_reference/tcl_program.html
---
## Tcl API programming notes

The Berkeley DB Tcl API does not attempt to avoid evaluating input as Tcl commands. For this reason, it may be dangerous to pass unreviewed user input through the Berkeley DB Tcl API, as the input may subsequently be evaluated as a Tcl command. Additionally, the Berkeley DB Tcl API initialization routine resets process' effective user and group IDs to the real user and group IDs, to minimize the effectiveness of a Tcl injection attack.

The Tcl API closely parallels the Berkeley DB programmatic interfaces. If you are already familiar with one of those interfaces, there will not be many surprises in the Tcl API.

The Tcl API currently does not support multithreading although it could be made to do so. The Tcl shell itself is not multithreaded and the Berkeley DB extensions use global data unprotected from multiple threads.

Several pieces of Berkeley DB functionality are not available in the Tcl API. Any of the functions that require a user-provided function are not supported via the Tcl API. For example, there is no equivalent to the <a href="../../api/c/dbset_dup_compare.md" class="olink">DB-&gt;set_dup_compare()</a> or <a href="../../api/c/envset_errcall.md" class="olink">DB_ENV-&gt;set_errcall()</a> methods. Additionally, the heap access method is not available.
