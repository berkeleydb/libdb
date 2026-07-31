---
title: "DB structure"
api-name: "DB structure"
source: docs/upgrading/upgrade_3_0_db.html
---
## DB structure

The <a href="../../api/c/db.md" class="olink">DB</a> structure is now opaque for applications in the Berkeley DB 3.0 release. Accesses to any fields within that structure by the application should be replaced with method calls. The following example illustrates this using the historic type structure field. In the Berkeley DB 2.X releases, applications could find the type of an underlying database using code similar to the following:

``` c
DB *db;
DB_TYPE type;

type = db->type;
```

in the Berkeley DB 3.X releases, this should be done using the <a href="../../api/c/dbget_type.md" class="olink">DB-&gt;get_type()</a> method, as follows:

``` c
DB *db;
DB_TYPE type;

type = db->get_type(db);
```

The following table lists the <a href="../../api/c/db.md" class="olink">DB</a> fields previously used by applications and the methods that should now be used to get or set them.

| <a href="../../api/c/db.md" class="olink">DB</a> field | Berkeley DB 3.X method |
|----|----|
| byteswapped | <a href="../../api/c/dbget_byteswapped.md" class="olink">DB-&gt;get_byteswapped()</a> |
| db_errcall | <a href="../../api/c/dbset_errcall.md" class="olink">DB-&gt;set_errcall()</a> |
| db_errfile | <a href="../../api/c/dbset_errfile.md" class="olink">DB-&gt;set_errfile()</a> |
| db_errpfx | <a href="../../api/c/dbset_errpfx.md" class="olink">DB-&gt;set_errpfx()</a> |
| db_paniccall | DB-\>set_paniccall |
| type | <a href="../../api/c/dbget_type.md" class="olink">DB-&gt;get_type()</a> |
