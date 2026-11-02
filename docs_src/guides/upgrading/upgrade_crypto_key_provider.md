---
title: "Upgrading encrypted environments: key providers and the region verifier"
api-name: "Upgrading encrypted environments (RFC 0019)"
---
## Upgrading encrypted environments: key providers and the region verifier

This change (RFC 0019) is a **region-format change on `master` only**. It is not
backported to any LTS line, and it does not change the on-disk database format
or the log format.

### What changed

Before this release, `DB_ENV->set_encrypt(passwd)` copied the password
**verbatim into the shared environment region**, and a process joining the
environment authenticated by comparing its password against that copy. When the
region is file-backed (the default `__db.00x` files), the password was written
to disk in the clear and persisted after every process exited.

Now the region stores a **salted verifier** — PBKDF2-HMAC-SHA256 of the key over
a random 128-bit salt, at a high iteration count — and never the secret itself.
A joining process recomputes the verifier and compares it in constant time. The
derived AES and MAC keys live only in process memory; they are best-effort
locked out of swap (`mlock`) and zeroed on close.

The **page and log encryption key derivation is unchanged.** A database created
and encrypted by an earlier `master` (or by Berkeley DB) opens with the same
password: the page key still derives from the password through the same
`__aes_derivekeys` path. Only the *region* representation of the secret changed.

### Upgrade procedure

**Databases and logs:** none. Existing encrypted databases and log files are
byte-compatible and open unchanged with the same password.

**Shared region:** the region signature changes, so an environment must be shut
down cleanly before upgrading the library, the same as for any other
`master` release that carries a region-format change. Run recovery
(`db_recover`) once with the new library, or open the environment with
`DB_RECOVER`. All processes sharing one environment must run the same library
version, as always.

### New: key providers

The key no longer has to be a password the application holds in memory. A new
interface supplies the key from elsewhere:

- `DB_ENV->set_encrypt(passwd, flags)` — unchanged; installs the built-in
  password provider.
- `DB_ENV->set_encrypt_keyfile(path, flags)` — reads a raw 256-bit key from a
  file. The file is refused unless it is owner-only (not group- or
  world-accessible), owned by the caller, and exactly 256 bits.
- `DB_ENV->set_encrypt_tpm(blobpath, flags)` — unseals the key from a TPM 2.0
  device (or `swtpm`). Available only when the library is built with
  `--enable-tpm` / `-Dtpm=enabled`, which is **off by default** so a default
  build gains no dependency on libtss2.
- `DB_ENV->set_encrypt_provider(provider, flags)` — installs a caller-supplied
  provider (an HSM, a KMS client, …) through a small `get_key`/`release`/`rotate`
  vtable.

The key-file and TPM providers wrap a random 256-bit data-encryption key (DEK)
with the provider's key-encryption key (KEK) using AES Key Wrap (RFC 3394); the
wrapped DEK lives in the region. A key rotation re-wraps the DEK rather than
re-encrypting every page. The password provider keeps its original behaviour
(the page key derives from the password directly), so existing encrypted
databases open unchanged.

A utility, `db_keytool`, creates a raw key file (`-k`), seals a key to the TPM
(`-T`, with an optional PCR policy `-C`), and inspects a sealed blob (`-i`); it
never prints a secret.

### Scope

The page cipher is still AES-CBC with a SHA-1 MAC — this change does not touch
it. Authenticated (AEAD) page encryption is a separate future proposal.
