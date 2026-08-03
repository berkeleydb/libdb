---
title: "Database Open Flags"
api-name: "Database Open Flags"
source: docs/gsg/C/DBOpenFlags.html
---
## Database Open Flags

The following are the flags that you may want to use at database open time. Note that this list is not exhaustive — it includes only those flags likely to be of interest for introductory, single-threaded database applications. For a complete list of the flags available to you, see the *Berkeley DB C API Reference Guide.*

### Note

To specify more than one flag on the call to `DB->open()`, you must bitwise inclusively OR them together:

``` c
u_int32_t open_flags = DB_CREATE | DB_EXCL;
```

- `DB_CREATE`

  If the database does not currently exist, create it. By default, the database open fails if the database does not already exist.

- `DB_EXCL`

  Exclusive database creation. Causes the database open to fail if the database already exists. This flag is only meaningful when used with `DB_CREATE`.

- `DB_RDONLY`

  Open the database for read operations only. Causes any subsequent database write operations to fail.

- `DB_TRUNCATE`

  Physically truncate (empty) the on-disk file that contains the database. Causes DB to delete all databases physically contained in that file.
