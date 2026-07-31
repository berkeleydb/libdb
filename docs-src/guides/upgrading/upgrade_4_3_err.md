---
title: "DB_ENV->set_errcall, DB->set_errcall"
api-name: "DB_ENV->set_errcall, DB->set_errcall"
source: docs/upgrading/upgrade_4_3_err.html
---
## DB_ENV-\>set_errcall, DB-\>set_errcall

The signature of the error callback passed to the <a href="../../api/c/envset_errcall.md" class="olink">DB_ENV-&gt;set_errcall()</a> and <a href="../../api/c/dbset_errcall.md" class="olink">DB-&gt;set_errcall()</a> methods has changed in the 4.3 release. For example, if you previously had a function such as this:

``` c
void handle_db_error(const char *prefix, char *message);
```

it should be changed to this:

``` c
void handle_db_error(const DB_ENV *dbenv,
    const char *prefix, const char *message);
```

This change adds the <a href="../../api/c/env.md" class="olink">DB_ENV</a> handle to provide database environment context for the callback function, and incidentally makes it clear the message parameter cannot be changed by the callback.
