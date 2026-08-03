---
title: "DB->get_encrypt_flags()"
api-name: "DB->get_encrypt_flags()"
source: docs/api_reference/C/dbget_encrypt_flags.html
---
## DB-\>get_encrypt_flags()

``` c
#include <db.h>

int
DB->get_encrypt_flags(DB *db, u_int32_t *flagsp);  
```

The `DB->get_encrypt_flags()` method returns the encryption flags. This flag can be set using the <a href="dbset_encrypt.md" class="xref" title="DB-&gt;set_encrypt()">DB-&gt;set_encrypt()</a> method.

The `DB->get_encrypt_flags()` method may be called at any time during the life of the application.

The `DB->get_encrypt_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flagsp

The `DB->get_encrypt_flags()` method returns the encryption flags in **flagsp**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_encrypt.md" class="xref" title="DB-&gt;set_encrypt()">DB-&gt;set_encrypt()</a>
