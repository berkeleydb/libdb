---
title: "Converting Applications"
api-name: "Converting Applications"
source: docs/upgrading/upgrade_2_0_convert.html
---
## Converting Applications

Mapping the Berkeley DB 1.85 functionality into Berkeley DB version 2 is almost always simple. The manual page <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> replaces the Berkeley DB 1.85 manual pages `dbopen`(3), `btree`(3), `hash`(3) and `recno`(3). You should be able to convert each 1.85 function call into a Berkeley DB version 2 function call using just the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> documentation.

Some guidelines and things to watch out for:

1.  Most access method functions have exactly the same semantics as in Berkeley DB 1.85, although the arguments to the functions have changed in some cases. To get your code to compile, the most common change is to add the transaction ID as an argument (NULL, since Berkeley DB 1.85 did not support transactions.)

2.  You must always initialize DBT structures to zero before using them with any Berkeley DB version 2 function. (They do not normally have to be reinitialized each time, only when they are first allocated. Do this by declaring the DBT structure external or static, or by calling the C library routine `bzero`(3) or `memset`(3).)

3.  The error returns are completely different in the two versions. In Berkeley DB 1.85, \< 0 meant an error, and \> 0 meant a minor Berkeley DB exception. In Berkeley DB 2.0, \> 0 means an error (the Berkeley DB version 2 functions return `errno` on error) and \< 0 means a Berkeley DB exception. See <a href="../../guides/programmer_reference/program_errorret.md" class="olink">Program returns to applications</a> for more information.

4.  The Berkeley DB 1.85 DB-\>seq function has been replaced by cursors in Berkeley DB version 2. The semantics are approximately the same, but cursors require the creation of an extra object (the DBC object), which is then used to access the database.

    Specifically, the partial key match and range search functionality of the R_CURSOR flag in DB-\>seq has been replaced by the <a href="../../api/c/dbcget.md#dbcget_DB_SET_RANGE" class="olink">DB_SET_RANGE</a> flag in <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a>.

5.  In version 2 of the Berkeley DB library, additions or deletions into Recno (fixed and variable-length record) databases no longer automatically logically renumber all records after the add/delete point, by default. The default behavior is that deleting records does not cause subsequent records to be renumbered, and it is an error to attempt to add new records between records already in the database. Applications wanting the historic Recno access method semantics should call the <a href="../../api/c/dbset_flags.md" class="olink">DB-&gt;set_flags()</a> method with the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RENUMBER" class="olink">DB_RENUMBER</a> flag.

6.  Opening a database in Berkeley DB version 2 is a much heavier-weight operation than it was in Berkeley DB 1.85. Therefore, if your historic applications were written to open a database, perform a single operation, and close the database, you may observe performance degradation. In most cases, this is due to the expense of creating the environment upon each open. While we encourage restructuring your application to avoid repeated opens and closes, you can probably recover most of the lost performance by simply using a persistent environment across invocations.

While simply converting Berkeley DB 1.85 function calls to Berkeley DB version 2 function calls will work, we recommend that you eventually reconsider your application's interface to the Berkeley DB database library in light of the additional functionality supplied by Berkeley DB version 2, as it is likely to result in enhanced application performance.
