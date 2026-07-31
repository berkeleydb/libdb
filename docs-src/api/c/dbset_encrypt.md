---
title: "DB->set_encrypt()"
api-name: "DB->set_encrypt()"
source: docs/api_reference/C/dbset_encrypt.html
---
## DB-\>set_encrypt()

``` c
#include <db.h>

int
DB->set_encrypt(DB *db, const char *passwd, u_int32_t flags);  
```

Set the password used by the Berkeley DB library to perform encryption and decryption.

Because databases opened within Berkeley DB environments use the password specified to the environment, it is an error to attempt to set a password in a database created within an environment.

The `DB->set_encrypt()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->set_encrypt()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### passwd

The **passwd** parameter is the password used to perform encryption and decryption.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_ENCRYPT_AES`

  Use the Rijndael/AES (also known as the Advanced Encryption Standard and Federal Information Processing Standard (FIPS) 197) algorithm for encryption or decryption.

### Errors

The `DB->set_encrypt()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

#### EOPNOTSUPP

Cryptography is not available in this Berkeley DB release.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
