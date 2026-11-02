---
title: "DB_ENV->repmgr_set_tls()"
api-name: "DB_ENV->repmgr_set_tls()"
source: docs/api_reference/C/repmgrset_tls.html
---
## DB_ENV-\>repmgr_set_tls()

``` c
#include <db.h>

int
DB_ENV->repmgr_set_tls(DB_ENV *env,
    const char *cert_file, const char *key_file,
    const char *ca_file, u_int32_t flags);
```

The `DB_ENV->repmgr_set_tls()` method configures the Replication Manager to
use TLS 1.3 with mutual authentication on every connection it makes or accepts.
Without it, repmgr sends log records, database pages and election messages over
plain TCP in the clear — even when the environment is encrypted with
`DB_ENCRYPT`, because the pages and log are decrypted in the buffer pool before
they reach the wire.

TLS support is a **configure-time option** (`--enable-tls` for autoconf,
`-Dtls=enabled` for meson) and is **off by default**, so a default build links
no TLS library. On a build without TLS support this method returns
`DB_OPNOTSUP`. When enabled, libdb links the system OpenSSL (3.x; LibreSSL ≥ 3.5
is supported but not yet exercised in CI), which is distributed under its own
Apache-2.0 / ISC terms as an optional linked dependency.

Each site presents `cert_file` and proves possession of `key_file`; each peer
is verified against the CA bundle in `ca_file`. By default both ends require
and verify a peer certificate (mutual authentication). Whether a given
connection is TLS or plaintext is decided before the first repmgr byte, from
this configuration, not negotiated on the wire; the repmgr wire-protocol
version is unchanged.

The database environment's Replication Manager may also be configured using the
environment's DB_CONFIG file with the directives `repmgr_tls_cert`,
`repmgr_tls_key`, `repmgr_tls_ca`, `repmgr_tls_pin` (repeatable), and
`repmgr_tls_flags`.

The `DB_ENV->repmgr_set_tls()` method must be called before
`DB_ENV->repmgr_start()`. It returns a non-zero error value on failure and 0 on
success.

### Parameters

#### cert_file

The path to this site's certificate chain in PEM format. Required.

#### key_file

The path to this site's private key in PEM format. Required. If the key is
passphrase-protected, the passphrase is taken from the same source as
`DB_ENV->set_encrypt()`.

#### ca_file

The path to the CA bundle (PEM) used to verify peer certificates. May be `NULL`
only when verification is disabled, which is not recommended.

#### flags

The **flags** value is the bitwise inclusive OR of:

- **DB_REPMGR_TLS_VERIFY_PEER**

  Require and verify a peer certificate. This is the default behaviour and is
  assumed unless `DB_REPMGR_TLS_NO_VERIFY` is set.

- **DB_REPMGR_TLS_NO_VERIFY**

  Disable peer-certificate verification. Intended only for bootstrapping or
  testing; it logs a warning and must not be used in production. Incompatible
  with `DB_REPMGR_TLS_VERIFY_PEER`.

- **DB_REPMGR_TLS_REQUIRE**

  Refuse any plaintext peer. Without this flag a mixed group is permitted
  during a rolling upgrade; bring every site up with TLS configured, confirm
  from the statistics that all connections are TLS, then set this flag
  everywhere.

### Base-API applications

Applications that supply their own transport with
`DB_ENV->rep_set_transport()` do not use this method. For those, confidentiality
of the replication traffic is the transport callback's responsibility; see the
worked TLS transport in `examples/c/ex_rep_tls.c`.

### Errors

The `DB_ENV->repmgr_set_tls()` method may fail and return one of the following
non-zero errors:

- **EINVAL**

  An invalid flag combination was specified, or `cert_file`/`key_file` was
  `NULL`.

- **DB_OPNOTSUP**

  The library was built without TLS support (`--enable-tls`).

### Class

[DB_ENV](env.md)

### See Also

[Replication and Related Methods](rep.md)
