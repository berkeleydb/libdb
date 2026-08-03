---
title: "Error support"
api-name: "Error support"
source: docs/programmer_reference/am_misc_error.html
---
## Error support

Berkeley DB offers programmatic support for displaying error return values.

The <a href="../../api/c/envstrerror.md" class="olink">db_strerror()</a> function returns a pointer to the error message corresponding to any Berkeley DB error return, similar to the ANSI C strerror function, but is able to handle both system error returns and Berkeley DB specific return values.

For example:

``` c
int ret;
...
if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) != 0) {
    fprintf(stderr, "put failed: %s\n", db_strerror(ret));
    return (1);
}
```

There are also two additional error methods, <a href="../../api/c/dberr.md" class="olink">DB-&gt;err()</a> and `DB->errx()`. These methods work like the ANSI C X3.159-1989 (ANSI C) printf function, taking a printf-style format string and argument list, and writing a message constructed from the format string and arguments.

The <a href="../../api/c/dberr.md" class="olink">DB-&gt;err()</a> method appends the standard error string to the constructed message; the `DB->errx()` method does not. These methods provide simpler ways of displaying Berkeley DB error messages. For example, if your application tracks session IDs in a variable called session_id, it can include that information in its error messages:

Error messages can additionally be configured to always include a prefix (for example, the program name) using the <a href="../../api/c/dbset_errpfx.md" class="olink">DB-&gt;set_errpfx()</a> method.

``` c
#define DATABASE "access.db"

int ret;

(void)dbp->set_errpfx(dbp, program_name);

if ((ret = dbp->open(dbp,
    NULL, DATABASE, NULL, DB_BTREE, DB_CREATE, 0664)) != 0) {
    dbp->err(dbp, ret, "%s", DATABASE);
    dbp->errx(dbp,
        "contact your system administrator: session ID was %d",
            session_id);
    return (1);
}
```

For example, if the program were called my_app and the open call returned an EACCESS system error, the error messages shown would appear as follows:

``` c
my_app: access.db: Permission denied.
my_app: contact your system administrator: session ID was 14
```
