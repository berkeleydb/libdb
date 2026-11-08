---
title: "DB_ENV->set_encrypt_tpm()"
api-name: "DB_ENV->set_encrypt_tpm()"
---
## DB_ENV-\>set_encrypt_tpm()

``` c
#include <db.h>

int
DB_ENV->set_encrypt_tpm(DB_ENV *dbenv, const char *blobpath, u_int32_t flags);
```

Encrypt the environment with a key sealed to a TPM 2.0 device. When the environment is opened, the key is unsealed from the blob at **blobpath**. If the blob was sealed with a PCR policy, it unseals only while the platform state matches. `db_keytool -T` seals a key and writes the blob.

This method is available only when the library was built with `--enable-tpm` (autoconf) or `-Dtpm=enabled` (meson), which is off by default, so a default build has no dependency on libtss2.

The `DB_ENV->set_encrypt_tpm()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_encrypt_tpm()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the key must be the one the environment was created with, or an error will be returned.

The `DB_ENV->set_encrypt_tpm()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### blobpath

The **blobpath** parameter is the path of the sealed key blob.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_ENCRYPT_AES`

  Use the Rijndael/AES (also known as the Advanced Encryption Standard and Federal Information Processing Standard (FIPS) 197) algorithm for encryption or decryption.

### Errors

The `DB_ENV->set_encrypt_tpm()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; if **blobpath** is NULL or empty; or if an invalid flag value was specified.

#### EOPNOTSUPP

Cryptography is not available in this Berkeley DB release, or the library was built without TPM support.

When the environment is opened, unsealing can also fail: `ENOENT` if the blob does not exist, `EINVAL` if it is not a valid sealed blob, `EAGAIN` if the TPM is unavailable, and `EPERM` if the TPM refuses to unseal (for example, a PCR policy that no longer matches).

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_encrypt.md" class="xref" title="DB_ENV-&gt;set_encrypt()">DB_ENV-&gt;set_encrypt()</a>, <a href="../../guides/upgrading/upgrade_crypto_key_provider.md">the key-provider upgrade note</a>
