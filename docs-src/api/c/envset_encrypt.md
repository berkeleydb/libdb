---
title: "DB_ENV->set_encrypt()"
api-name: "DB_ENV->set_encrypt()"
source: docs/api_reference/C/envset_encrypt.html
---
## DB_ENV-\>set_encrypt()

``` c
#include <db.h>

int
DB_ENV->set_encrypt(DB_ENV *dbenv, const char *passwd, u_int32_t flags);  
```

Set the password used by the Berkeley DB library to perform encryption and decryption.

The `DB_ENV->set_encrypt()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_encrypt()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->set_encrypt()` must be consistent with the existing environment or an error will be returned.

The `DB_ENV->set_encrypt()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### passwd

The **passwd** parameter is the password used to perform encryption and decryption.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_ENCRYPT_AES`

  Use the Rijndael/AES (also known as the Advanced Encryption Standard and Federal Information Processing Standard (FIPS) 197) algorithm for encryption or decryption.

### Errors

The `DB_ENV->set_encrypt()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

#### EOPNOTSUPP

Cryptography is not available in this Berkeley DB release.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
