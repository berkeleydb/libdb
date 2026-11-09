---
title: "DB_ENV->set_encrypt_provider()"
api-name: "DB_ENV->set_encrypt_provider()"
---
## DB_ENV-\>set_encrypt_provider()

``` c
#include <db.h>

int
DB_ENV->set_encrypt_provider(DB_ENV *dbenv, DB_CRYPTO_PROVIDER *provider,
    u_int32_t flags);
```

Encrypt the environment with a key supplied by the application, for example from an HSM or a key-management service. libdb owns the data key and the wrapping. The provider supplies only the key-encryption key, through a small table of callbacks:

``` c
struct __db_crypto_provider {
	u_int32_t provider_id;		/* DB_CRYPTO_PROV_* or app-defined. */
	u_int32_t flags;
	void	 *ctx;			/* Provider-private state. */
	int	(*get_key)(DB_CRYPTO_PROVIDER *,
		    const char *key_id, u_int8_t *buf, size_t len);
	void	(*release)(DB_CRYPTO_PROVIDER *);
	int	(*rotate)(DB_CRYPTO_PROVIDER *, const char *key_id);
};
```

- **get_key** fills **buf** with **len** bytes of key-encryption key for **key_id** and returns 0, or an error number.
- **release** frees transient provider state. It is never asked to free the data key.
- **rotate** is optional (NULL if unsupported). It replaces the key-encryption key, after which libdb re-wraps the data key rather than re-encrypting every page.

libdb calls out through these from inside the library, so they must not call back into the same environment.

The `DB_ENV->set_encrypt_provider()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_encrypt_provider()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the key must be the one the environment was created with, or an error will be returned.

The `DB_ENV->set_encrypt_provider()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### provider

The **provider** parameter is the key provider. It must remain valid until the environment handle is closed.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_ENCRYPT_AES`

  Use the Rijndael/AES (also known as the Advanced Encryption Standard and Federal Information Processing Standard (FIPS) 197) algorithm for encryption or decryption.

### Errors

The `DB_ENV->set_encrypt_provider()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; if **provider** is NULL or has no **get_key** callback; or if an invalid flag value was specified.

#### EOPNOTSUPP

Cryptography is not available in this Berkeley DB release.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_encrypt.md" class="xref" title="DB_ENV-&gt;set_encrypt()">DB_ENV-&gt;set_encrypt()</a>, <a href="../../guides/upgrading/upgrade_crypto_key_provider.md">the key-provider upgrade note</a>
