---
title: "DB_ENV->set_encrypt_keyfile()"
api-name: "DB_ENV->set_encrypt_keyfile()"
---
## DB_ENV-\>set_encrypt_keyfile()

``` c
#include <db.h>

int
DB_ENV->set_encrypt_keyfile(DB_ENV *dbenv, const char *path, u_int32_t flags);
```

Encrypt the environment with a key read from a file, instead of one derived from a password (<a href="envset_encrypt.md" class="xref" title="DB_ENV-&gt;set_encrypt()">DB_ENV-&gt;set_encrypt()</a>).

The file must hold exactly 256 bits (32 bytes) of raw key, be owned by the calling user, and be readable by its owner only, with no group or other permission bits. It is checked when this method is called and again when the environment is opened. A symbolic link is refused. The key wraps a random data key held in the region, so the key itself is never stored there. `db_keytool -k` creates a suitable file.

The `DB_ENV->set_encrypt_keyfile()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_encrypt_keyfile()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the key must be the one the environment was created with, or an error will be returned.

The `DB_ENV->set_encrypt_keyfile()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### path

The **path** parameter is the path of the key file.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_ENCRYPT_AES`

  Use the Rijndael/AES (also known as the Advanced Encryption Standard and Federal Information Processing Standard (FIPS) 197) algorithm for encryption or decryption.

### Errors

The `DB_ENV->set_encrypt_keyfile()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; if **path** is NULL or empty; if the file is not a regular file, is a symbolic link, is not exactly 256 bits, is not owned by the caller, or is accessible by group or other; or if an invalid flag value was specified.

#### EOPNOTSUPP

Cryptography is not available in this Berkeley DB release.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_encrypt.md" class="xref" title="DB_ENV-&gt;set_encrypt()">DB_ENV-&gt;set_encrypt()</a>, <a href="../../guides/upgrading/upgrade_crypto_key_provider.md">the key-provider upgrade note</a>
