# RFC 0019: Key providers, a salted verifier, and TPM-backed key storage

- **Status:** Draft
- **Type:** Normative
- **Author:** W5 (libdb contributors)
- **Date:** 2026-11-02
- **Tracking:** directed work #3 (store secret material properly; add TPM-backed key storage); review D-3 (`/scratch/libdb/review/REVIEW-2026-11.md`, `A/loverso-bram.md`)
- **Prototype:** `rfc/0019/`

---

## Summary

Today `DB_ENV->set_encrypt(passwd)` stores the application's password **verbatim
in the shared environment region** (`__crypto_region_init`, `src/crypto/crypto.c`),
and a joining process authenticates by `memcmp` against that copy. If the region
is file-backed — the default — the password sits on disk in clear and survives
the process. This RFC (1) replaces the region password copy with a **salted,
slow-KDF verifier** (PBKDF2-HMAC-SHA256) that a joiner recomputes and compares in
constant time, keeping the derived keys in process memory only; (2) adds a
**key-provider interface** (`DB_ENV->set_encrypt_provider`) with built-in
password, key-file, and TPM 2.0 providers so the key need not be a password the
application holds; and (3) introduces **key wrapping** so a data-encryption key
(DEK) is wrapped by the provider's key-encryption key (KEK), making a password
change or key rotation a re-wrap rather than a re-encryption of every page. The
page cipher (AES-CBC) is **unchanged**; AEAD page encryption is a separate future
RFC. LTS is untouched.

## Motivation

The 2026-11 review (D-3) and both the LoVerso and the general-notes sections of
`A/loverso-bram.md` name the region-resident password as "the single highest-value
secret in the system", and LoVerso's own 2002 design doc flags it in its Risks
section: "The password is stored in the shared data regions in the clear …
file backed regions will be written in the clear, including the portion of the
regions containing passwords." The 2002 rationale was that
`__aes_derivekeys` is not reproducible across calls, so "we don't have any way to
check passwords other than retaining and comparing the actual passwords"
(crypto-design-loverso.txt, "Sharing the Environment"). That rationale no longer
holds: a salted KDF verifier authenticates a joiner without retaining the secret.

Three concrete harms today, all verified by reading `src/crypto/crypto.c`:

1. **Secret at rest.** With the default file-backed region (`__db.00x`), anyone
   who can read the env directory reads the password in clear, and it persists
   after every process exits.
2. **No alternative to an in-memory password.** The only key source is a password
   the application passes to `set_encrypt` and must hold in its own memory. There
   is no file-based or hardware-sealed key path.
3. **No rotation.** Changing the key means dump/reload of every database, because
   the password gates the page key directly (`__aes_init` → `__aes_derivekeys`
   over `dbenv->passwd`).

## North-star check

- **Embedded / no server:** all three providers run in-process. The TPM provider
  talks to a local TPM (or `swtpm`) over the platform TSS stack; no network, no
  server process. It is compile-time optional and **off by default**, so the
  default build gains no new dependency.
- **ACID / recovery / access methods:** the **page and log encryption key
  derivation is unchanged**. `__aes_derivekeys` still derives the AES key and MAC
  key from a secret via SHA1 exactly as before, so every existing encrypted
  database and log opens and recovers unchanged. The verifier and the wrapped-DEK
  machinery are *additional* state; they do not alter any page, log-record, or
  meta-page byte layout. db_verify / db_printlog are unaffected (no log-record
  format change).
- **Multi-process correctness:** joiners authenticate against the region verifier
  (recompute KDF, constant-time compare) instead of `memcmp` of the password.
  Same accept/reject semantics (wrong secret → `EPERM`), observable identically.
- **Format stability — this is the hard gate.** The change **does** alter the
  shared-region `CIPHER` struct (the region no longer holds the password; it
  holds a salt + verifier + optional wrapped DEK). That is a **region format
  change**, master-only, gated by `REGION-SIG-CHANGE-INTENDED` with an upgrade
  note in `docs_src/guides/upgrading/`. It is **not** an on-disk database or log
  format change: existing encrypted `.db` files and log files are byte-compatible
  and open unchanged. Regions are recreated on every environment (re)start and
  never backported, so a region-signature bump is the sanctioned mechanism (user
  decision Q1). ABI: `set_encrypt_provider` is a new slot on `DB_ENV`; the soname
  bump is W6/RFC 0020's concern and this RFC adds its symbol there.

## Design

### 0. Compatibility spine: two secrets, one derivation

The crucial separation this RFC rests on:

- **The page/log encryption secret** (the DEK) feeds `__aes_derivekeys`
  unchanged. For the password provider in the default (non-wrapped, non-provider)
  mode, the DEK *is* the password, exactly as today — so a database created by a
  pre-RFC master opens with the same password with no migration.
- **The verifier** is a *separate* value derived by a *different, slow, salted*
  KDF (PBKDF2-HMAC-SHA256). It never feeds the cipher. Its only job is to let a
  joiner prove it holds the right secret without the region storing the secret.

This separation is what lets us remove the cleartext region copy without touching
the page cipher.

### 1. The region verifier (replaces the password copy)

The shared `CIPHER` struct (`src/dbinc/crypto.h`) changes from holding a password
offset to holding a verifier record. New region layout (all inline, fixed size —
no more `__env_alloc` of a variable-length password):

```c
#define DB_CRYPTO_SALT_LEN   16    /* 128-bit random salt */
#define DB_CRYPTO_VRFY_LEN   32    /* PBKDF2-HMAC-SHA256 output */
#define DB_CRYPTO_WRAP_LEN   40    /* RFC 3394 wrap of a 256-bit DEK (32+8) */

typedef struct __cipher {
	u_int32_t	flags;			/* Algorithm used - see above */
	u_int32_t	fmt_version;		/* CIPHER region format version = 2 */
	u_int32_t	kdf_iters;		/* PBKDF2 iteration count */
	u_int32_t	provider;		/* DB_CRYPTO_PROV_* that created it */
	u_int8_t	salt[DB_CRYPTO_SALT_LEN];
	u_int8_t	verifier[DB_CRYPTO_VRFY_LEN];
	u_int8_t	wrapped_dek[DB_CRYPTO_WRAP_LEN]; /* 0 if no DEK wrapping */
	u_int32_t	dek_wrapped;		/* boolean: wrapped_dek is valid */
} CIPHER;
```

**Creator** (`__crypto_region_init`, region-create branch): generate a random
`salt` from `__os_csprng`; compute `verifier = PBKDF2-HMAC-SHA256(KEK_check,
salt, iters)` where `KEK_check` is the provider's verifier input (for the
password provider, the password bytes); store `flags/fmt_version/kdf_iters/
provider/salt/verifier`. Then smash and free `dbenv->passwd` as today. No secret
remains in the region.

**Joiner** (`__crypto_region_init`, join branch): read `salt`, `kdf_iters`;
recompute `verifier'` the same way; compare `verifier'` to the region `verifier`
with a **constant-time compare** (`__crypto_ct_memeq`, below). Mismatch → `EPERM`
("Invalid password"), same as today. Algorithm mismatch → `EINVAL`, unchanged.

`kdf_iters` default: **210,000** (OWASP 2023 PBKDF2-HMAC-SHA256 floor). Tunable
via `DB_ENV->set_encrypt_provider` flags / an env knob for test speed. Stored in
the region so a joiner uses the creator's count.

**Constant-time compare** (`__crypto_ct_memeq`): fold all byte differences into
one accumulator, branch only on the final zero-test. Used for the verifier and
for any secret comparison. New function in `src/crypto/crypto.c`.

### 2. Process-local key hygiene

The derived AES key material already lives only in the per-process `AES_CIPHER`
(`keyInstance`) and the per-process `DB_CIPHER->mac_key` — neither is in the
region. This RFC hardens them:

- **mlock** the `AES_CIPHER` allocation (and the DEK scratch buffer during
  wrap/unwrap) where `mlock`/`VirtualLock` is available, behind a new
  `__os_mlock`/`__os_munlock` wrapper (best-effort: a failure is logged, not
  fatal, since unprivileged `mlock` limits vary).
- **Zero on close**: `__aes_close` and `__crypto_env_close` zero the key material
  with `explicit_bzero` (or `memset_s`, or a volatile-pointer memset fallback)
  before free, via a new `__os_explicit_bzero` wrapper. Today they `__os_free`
  without zeroing.
- The DEK, after unwrap, lives only in the stack/heap scratch that feeds
  `__aes_derivekeys`; it is zeroed immediately after key derivation.

### 3. The key-provider interface

A new public env method and a small vtable:

```c
int DB_ENV->set_encrypt_provider(DB_ENV *, DB_CRYPTO_PROVIDER *, u_int32_t flags);

struct __db_crypto_provider {
	u_int32_t provider_id;   /* DB_CRYPTO_PROV_PASSWD | _KEYFILE | _TPM | app */
	void     *ctx;           /* provider-private */
	/* Fill buf (len bytes) with the KEK/secret for key_id. Return 0/errno. */
	int  (*get_key)(DB_CRYPTO_PROVIDER *, const char *key_id,
	                u_int8_t *buf, size_t len);
	/* Release any transient provider state (not the DEK). */
	void (*release)(DB_CRYPTO_PROVIDER *);
	/* Optional: rotate the KEK (TPM/keyfile). NULL if unsupported. */
	int  (*rotate)(DB_CRYPTO_PROVIDER *, const char *key_id);
};
```

`set_encrypt(passwd)` stays and is now sugar for installing the built-in password
provider — full backward compatibility, no caller change. The provider supplies
the **KEK**; libdb owns the DEK and the wrapping.

Built-in providers (`src/crypto/provider_*.c`):

- **(a) Password provider** — `get_key` returns the password-derived KEK. In the
  default no-wrapping mode the KEK *is* the page secret (today's behaviour) so old
  databases open; in wrapping mode the KEK unwraps the DEK.
- **(b) Key-file provider** — reads a raw 256-bit key from a file. **Refuses** a
  file that is group- or world-readable or not owned by the caller
  (`fstat` → `(st_mode & (S_IRWXG|S_IRWXO)) != 0` → `EINVAL` with a clear
  message); refuses a wrong-length file. On Windows, check the ACL is owner-only;
  if the check is not implementable, refuse unless an explicit override flag is
  set (documented).
- **(c) TPM 2.0 provider** — see §5.

### 4. Key wrapping (DEK/KEK)

- A random 256-bit **DEK** is generated at env create (`__os_csprng`).
- The provider's **KEK** wraps the DEK with **AES Key Wrap (RFC 3394)** — a small,
  dependency-free, public-domain-describable algorithm implemented over the
  existing AES primitive (`src/crypto/wrap.c`). Output is 40 bytes for a 256-bit
  DEK.
- The wrapped DEK is stored **in the region `CIPHER` struct** (`wrapped_dek`,
  `dek_wrapped=1`). Rationale: the region is the one place every joiner already
  consults, it is recreated per env lifetime, and it already carries the
  algorithm id and verifier; putting the wrapped DEK there means **no new file**
  and a multi-process join obtains the wrapped DEK exactly where it finds the
  verifier. The wrapped DEK is ciphertext under the KEK, so region-at-rest
  exposure reveals only ciphertext — the whole point.

  *Persistence note:* the region is ephemeral, but the DEK must be stable across
  env restarts (or every database would need re-encryption). So the authoritative
  copy of `{salt, kdf_iters, provider, wrapped_dek, verifier}` is also written to
  a small env-home file **`__db_keys`** (owner-only perms, created at env create,
  read at env create to seed the region). The region is the fast path for
  joiners within one env lifetime; `__db_keys` is the durable source across
  restarts. `__db_keys` holds no cleartext secret — only salt, iteration count,
  the verifier, and the KEK-wrapped DEK.

- **Default mode (no DEK, maximal compat):** if the application uses
  `set_encrypt` and does **not** opt into wrapping (`DB_ENCRYPT_WRAP` flag or a
  provider that requires it), `dek_wrapped=0` and the page secret is the
  password-derived KEK directly — byte-identical key derivation to pre-RFC master.
  This is the path the "existing encrypted DB still opens" test exercises.
- **Wrapping mode:** page secret = unwrapped DEK. Available for new environments
  and required for the key-file and TPM providers (whose KEK is not a password).

**Rotation** (`rotate`): unwrap the DEK with the old KEK, obtain the new KEK from
the provider, re-wrap, rewrite `__db_keys` and the region. The DEK is unchanged,
so **no page is re-encrypted**. A password change is the password-provider's
`rotate`. This is a maintenance op run with the env quiescent (documented);
concurrent rotation is out of scope for this RFC.

### 5. TPM 2.0 provider

Behind `--enable-tpm` (autoconf) / `-Dtpm=enabled` (meson), **off by default**
(user decision Q3: both build systems). Uses **libtss2 ESAPI** (BSD-2-Clause,
license-compatible); `src/crypto/provider_tpm.c` is compiled only when enabled.

- The DEK is **sealed to the TPM**: `Esys_Create` of a sealed-data (keyedhash,
  `TPM2_ALG_KEYEDHASH` with `sensitiveDataOrigin` clear so we supply the data)
  object under the **storage hierarchy** (SRK, template `TPM2_ALG_ECC`/`RSA` per
  platform), optionally bound to a **PCR policy** (a `PolicyPCR` over a
  user-selected PCR set). The sealed blob (public + private parts) is stored in
  `__db_keys`.
- At env open the provider `TPM2_Load`s the sealed object under the SRK and
  `Esys_Unseal`s it to recover the DEK (or, in KEK mode, a TPM-held KEK that then
  unwraps the DEK). A wrong PCR state fails `PolicyPCR` → unseal returns a TPM
  error → `EPERM`.
- `db_keytool` (util/) creates the SRK context, seals a fresh DEK (or a supplied
  key), optionally sets the PCR policy, and writes `__db_keys`; and inspects a
  sealed blob (public area, policy digest, no secret).

### 6. Utility: `db_keytool`

`util/db_keytool.c`, built like the other `db_*` utilities. Subcommands:

- `create -h HOME -p PASSWD` — initialise key material for the password provider
  (writes `__db_keys` with salt/verifier/wrapped-DEK).
- `keyfile -h HOME -f KEYFILE` — install a key-file provider.
- `tpm-seal -h HOME [-c PCRSPEC]` — seal a fresh DEK to the TPM (requires
  `--enable-tpm`).
- `inspect -h HOME` — print provider id, KDF iterations, format version, PCR
  policy digest; **never** a secret.
- `rotate -h HOME ...` — re-wrap the DEK under a new KEK.

### 7. Sabotage arm (teeth, per the brief)

A build flag `-DCRYPTO_REGION_SECRET_SABOTAGE` makes `__crypto_region_init` store
the password back into the region (old behaviour). The leak test (§Tests) must
**FAIL** under this flag and **PASS** without it, proving the test actually
detects the secret.

## Alternatives considered

- **scrypt / Argon2id instead of PBKDF2.** Stronger memory-hardness, but a
  Sleepycat/BSD/ISC/MIT/public-domain implementation small enough to vendor
  cleanly is harder to source and audit than PBKDF2, which is a ~40-line wrapper
  over the existing SHA1→(add SHA256) HMAC. PBKDF2-HMAC-SHA256 at 210k iterations
  meets the brief's "high iteration count" bar and the OWASP floor. Argon2id is
  noted as a future upgrade (bump `fmt_version`, store the new params). **Chosen:
  PBKDF2-HMAC-SHA256.**
- **Wrapped DEK in database meta-pages.** Would be a per-database on-disk format
  change (the brief forbids changing the page key derivation and wants existing
  DBs to open). The region + `__db_keys` placement is env-scoped, matches the
  single-env-password model LoVerso chose, and needs no meta-page change.
- **Keep the password in the region but encrypt the region.** Circular (what key
  encrypts the region?) and still leaves a decryptable secret at rest. The
  verifier removes the secret entirely.
- **Change the page cipher to AEAD now.** Explicitly out of scope (brief §4);
  separate future RFC. SHA-256 is added only for the KDF/verifier, not the page
  MAC.

## Risks & open questions

- **New SHA-256 primitive.** `src/hmac/` ships SHA1 only. This RFC vendors a
  small public-domain SHA-256 + HMAC-SHA256 (e.g. the widely-used public-domain
  `sha256.c`) for the KDF/verifier/key-wrap only. License must be
  Sleepycat/BSD/ISC/MIT/public-domain — verified before import. Page/log MAC stay
  SHA1 (unchanged).
- **mlock limits.** Unprivileged `RLIMIT_MEMLOCK` is small on some systems; mlock
  is best-effort and logged, never fatal, so an env still opens without it.
- **`__db_keys` durability vs. region.** Two copies of the key metadata (region +
  file) must agree. Creator writes the file first, then seeds the region from it;
  join reads the region; restart reads the file. Rotation rewrites the file then
  the region, with the file as source of truth. Documented as env-quiescent.
- **TPM availability in CI.** Covered by `swtpm` on the dev box; the TPM provider
  is skipped (not failed) where no TPM/`swtpm` is present, and `--enable-tpm` is
  listed in `option_sweep.sh` (G14 gate).
- **Open:** whether to also mlock the mpool region for encrypted envs (pages are
  cleartext in the cache — a pre-existing, documented limitation). Out of scope;
  noted for a future RFC.

## Prototype / evidence

`rfc/0019/` holds a standalone PBKDF2-HMAC-SHA256 + RFC-3394 key-wrap self-check
and a constant-time-compare micro-test, validating the primitives against known
test vectors (RFC 6070 PBKDF2 vectors; RFC 3394 wrap vectors) before they are
wired into the engine.

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Accepted | Rejected — YYYY-MM-DD
- **Rationale:** why.
- **Conditions / follow-ups:** any.
