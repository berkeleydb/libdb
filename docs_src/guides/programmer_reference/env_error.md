---
title: "Error support"
api-name: "Error support"
source: docs/programmer_reference/env_error.html
---
## Error support

Berkeley DB offers programmatic support for displaying error return values. The <a href="../../api/c/envstrerror.md" class="olink">db_strerror()</a> function returns a pointer to the error message corresponding to any Berkeley DB error return. This is similar to the ANSI C strerror interface, but can handle both system error returns and Berkeley DB-specific return values.

For example:

``` c
int ret;
if ((ret = dbenv->set_cachesize(dbenv, 0, 32 * 1024, 1)) != 0) {
    fprintf(stderr, "set_cachesize failed: %s\n", db_strerror(ret));
    return (1);
}
```

There are also two additional error methods: <a href="../../api/c/enverr.md" class="olink">DB_ENV-&gt;err()</a> and `DB_ENV->errx()`. These methods work like the ANSI C printf function, taking a printf-style format string and argument list, and writing a message constructed from the format string and arguments.

The <a href="../../api/c/enverr.md" class="olink">DB_ENV-&gt;err()</a> function appends the standard error string to the constructed message; the `DB_ENV->errx()` function does not.

Error messages can be configured always to include a prefix (for example, the program name) using the <a href="../../api/c/envset_errpfx.md" class="olink">DB_ENV-&gt;set_errpfx()</a> method.

These functions provide simpler ways of displaying Berkeley DB error messages:

``` c
int ret;
...
dbenv->set_errpfx(dbenv, program_name);
if ((ret = dbenv->open(dbenv, home,
    DB_CREATE | DB_INIT_LOG | DB_INIT_TXN | DB_USE_ENVIRON, 0))
    != 0) {
    dbenv->err(dbenv, ret, "open: %s", home);
    dbenv->errx(dbenv,
        "contact your system administrator: session ID was %d",
        session_id);
    return (1);
}
```

For example, if the program was called "my_app", and it tried to open an environment home directory in "/tmp/home" and the open call returned a permission error, the error messages shown would look like this:

``` c
my_app: open: /tmp/home: Permission denied.
my_app: contact your system administrator: session ID was 2
```
