# RFC 0018: TLS 1.3 for the Replication Manager

- **Status:** Draft
- **Type:** Normative
- **Author:** W4 (libdb contributors)
- **Date:** 2026-11-02
- **Tracking:** Q2 (invest in replication), review D-2 / O-REP-2; `feat/repmgr-tls`
- **Prototype:** `src/repmgr/repmgr_tls.c`

---

## Summary

The Replication Manager (`src/repmgr`) sends every log record, every page and
every election message over a plain TCP socket, in the clear, even when the
environment is encrypted with `DB_ENCRYPT`. There is no peer authentication:
any host that can reach the port can join the group as a replica or inject
messages. This RFC adds optional TLS 1.3 with mutual authentication on every
repmgr connection, built against OpenSSL 3.x, enabled by `--enable-tls` /
`-Dtls=enabled`, **off by default** so the default build gains no dependency.
The hard part — driving a non-blocking TLS state machine through repmgr's
single `select()`/`poll()` loop — is the bulk of this document.

## Motivation

The 2026-11 Sleepycat-panel review (`REVIEW-2026-11.md`, personas
`A/loverso-bram.md`, `B/env-rep-misc.md` items O-REP-1/2/4) flagged cleartext
replication as the single most serious exposure in the shipped product:

- **Confidentiality.** An encrypted database (`DB_ENCRYPT`, AES) is decrypted
  in the master's buffer pool and the cleartext page/log bytes go out on the
  wire to every replica. An observer on the replication network sees the
  plaintext of a database the operator went out of their way to encrypt at
  rest. The on-disk encryption is defeated by the transport.
- **Authentication.** `__repmgr_accept` (repmgr_net.c) accepts any inbound
  TCP connection and runs the version handshake; the peer is never
  cryptographically identified. A reachable port is a writable replica.
- **Integrity.** With no MAC on the wire, a man-in-the-middle can alter log
  records in flight; the client applies them under a page latch with no
  logical check (see memory note on client-isolation: redo takes latches, not
  verification).

The base-replication API (`DB_ENV->rep_set_transport`) hands the transport to
the application, so there confidentiality is the application's job — but repmgr
*is* the transport, and it ships one. This is the gap to close.

## North-star check

- **Embedded / no-server.** TLS adds no server and no new process. It is a
  per-connection filter on sockets repmgr already owns. When `--enable-tls` is
  not configured, not one byte of OpenSSL is linked and the product is
  bit-for-bit what it is today.
- **ACID / recovery / access methods.** TLS sits strictly below the message
  layer: it changes *how* the same bytes cross the wire, not the bytes, the
  log, the pages, or recovery. No access method is touched.
- **Multi-process.** The `SSL*` objects and the OpenSSL library context are
  **process-local**, created after `fork`/`start`, never placed in the shared
  region. A subordinate process that opens its own repmgr connections builds
  its own `SSL_CTX`. No pointer crosses the region boundary.
- **On-disk / log / region / ABI format.** **No format change.** No on-disk,
  log or region structure changes. The wire framing is unchanged — TLS wraps
  the existing framed stream. The region signature is unaffected (TLS config
  lives in process-local `DB_REP`, not the region). The public API grows by
  new methods only (additive; see Q6/soname work for the coordinated bump).
  The repmgr wire-protocol version is **not** bumped: whether a connection is
  TLS or plaintext is decided before the first repmgr byte, by configuration,
  not negotiated in-band.

No north-star invariant is broken. The one external change is an *optional*
link-time dependency, addressed next.

## Licensing of the dependency

libdb is distributed under the Sleepycat license. OpenSSL 3.x is Apache-2.0.
Apache-2.0 is a permissive license compatible with distributing libdb under
Sleepycat **as an optional, separately-licensed linked dependency**: we do not
vendor or relicense any OpenSSL source, we link against the system/third-party
library only when the operator opts in with `--enable-tls`. The default build
links nothing. We document in `LICENSE`/the TLS guide that enabling TLS pulls
in OpenSSL under its own Apache-2.0 terms. This is the same posture already
used for optional Tcl (test) and other optional deps.

- **LibreSSL** is API-compatible for the subset we use (`SSL_CTX_new`,
  `TLS_method`, `SSL_read`/`SSL_write`, memory BIOs, `SSL_get_error`,
  `X509_VERIFY_PARAM`). It lacks a few OpenSSL-3 niceties (`OSSL_PROVIDER`,
  `SSL_CTX_new_ex` with a libctx); we use only the portable subset and guard
  OpenSSL-3-only calls with `#ifdef`. LibreSSL ≥ 3.5 is expected to work; we
  state it as "supported, untested in CI" until a CI arm exists.
- **BoringSSL** omits `SSL_CTX_set_options(..., SSL_OP_...)` constants in a few
  places and does not ship a stable soname; it is **not supported** (stated in
  the guide) but the code compiles against it bar the version-pinning calls.
- We require **TLS 1.3 minimum** (`SSL_CTX_set_min_proto_version(TLS1_3_VERSION)`),
  which OpenSSL ≥ 1.1.1 and LibreSSL ≥ 3.4 provide.

## Design

### 1. The integration problem

repmgr's I/O model (`__repmgr_select_loop`, repmgr_posix.c) is a single thread
that holds `db_rep->mutex`, builds two `fd_set`s, drops the mutex only across
`select()`, then for each ready connection calls `__repmgr_write_some` (if the
outbound queue is non-empty and the fd is writable) and `__repmgr_read_from_site`
(if the fd is readable). Writes otherwise happen inline in the sender's own
thread via `__repmgr_send_internal` → `__repmgr_write_iovecs` → `writev`.

TLS breaks three of the model's assumptions:

1. **A write may need to read, and a read may need to write.** During a TLS
   handshake or a TLS key update, `SSL_write` can return `SSL_ERROR_WANT_READ`
   and `SSL_read` can return `SSL_ERROR_WANT_WRITE`. repmgr currently selects a
   socket for *write* only when the outbound queue is non-empty, and for *read*
   always. We must let a connection ask to be selected for the *opposite*
   direction than the work it wants to do.

2. **There is a handshake before any application byte.** A new connection
   (outbound `__repmgr_connect`, inbound `__repmgr_accept`) must complete a TLS
   handshake — itself several non-blocking round trips — before the existing
   version-negotiation bytes may flow.

3. **`writev`/`readv` scatter-gather does not exist in the TLS API.** `SSL_write`
   takes one contiguous buffer. The iovec send path must be adapted (coalesce,
   or write segment-by-segment).

### 2. Where TLS lives

A new file `src/repmgr/repmgr_tls.c` (POSIX) holds all OpenSSL calls behind a
small internal interface, so repmgr_net.c / repmgr_sel.c / repmgr_posix.c carry
*no* `#include <openssl/*>` and almost no `#ifdef HAVE_REPMGR_SSL`. The
interface:

```
int  __repmgr_ssl_init(ENV *);                 /* process-local SSL_CTX */
void __repmgr_ssl_destroy(ENV *);
int  __repmgr_ssl_conn_init(ENV *, REPMGR_CONNECTION *, int is_client);
void __repmgr_ssl_conn_destroy(REPMGR_CONNECTION *);
int  __repmgr_ssl_handshake(ENV *, REPMGR_CONNECTION *);  /* drives SSL_do_handshake */
int  __repmgr_ssl_read(REPMGR_CONNECTION *, void *, size_t, size_t *nr);
int  __repmgr_ssl_write(REPMGR_CONNECTION *, const void *, size_t, size_t *nw);
```

Each returns one of `0` (progress/done), `WOULDBLOCK` (want-read), a new
`DB_REPMGR_WANT_WRITE` sentinel (want-write), `DB_REP_UNAVAIL` (clean EOF /
peer closed), or a real errno. The three callers (`__repmgr_writev` consumers,
`__repmgr_read_conn`, the connect/accept paths) route those return codes into
the select-direction logic below. The SSL object hangs off the connection:

```c
struct __repmgr_connection {
	...
	socket_t fd;
#ifdef HAVE_REPMGR_SSL
	void *ssl;              /* SSL *  (opaque here; cast in repmgr_tls.c) */
	u_int32_t ssl_flags;    /* SSL_WANT_READ_TO_WRITE | SSL_WANT_WRITE_TO_READ
	                         * | SSL_HANDSHAKING | SSL_PLAINTEXT */
#endif
	...
	phase_t reading_phase;  /* gains HANDSHAKE_PHASE below */
};
```

`ssl == NULL` on a connection means plaintext (either TLS off, or a plaintext
peer we chose to allow). Every read/write site checks: if `ssl != NULL`, go
through `__repmgr_ssl_*`; else the existing `__repmgr_readv`/`__repmgr_writev`.
This keeps a single code path for framing and only forks at the byte layer.

### 3. Mapping WANT_READ / WANT_WRITE into select()

Two new per-connection bits, set by `repmgr_tls.c` when `SSL_get_error` says so:

- `SSL_WANT_WRITE_TO_READ`: the connection wants to *read* application data
  (or finish a handshake), but OpenSSL needs to *write* TLS bytes first. ⇒ ask
  select() for **writable**.
- `SSL_WANT_READ_TO_WRITE`: the connection has a pending *write* (handshake or
  queued app data), but OpenSSL needs to *read* TLS bytes first. ⇒ ask
  select() for **readable**.

`prepare_io` (repmgr_posix.c) and its poll/Windows twins change from:

```c
if (!STAILQ_EMPTY(&conn->outbound_queue)) FD_SET(fd, writes);
FD_SET(fd, reads);                 /* always read */
```

to the TLS-aware form:

```c
want_read  = TRUE;                 /* repmgr always wants to read */
want_write = !STAILQ_EMPTY(&conn->outbound_queue) || conn->ssl_handshaking;
if (conn->ssl != NULL) {
	if (F_ISSET(conn, SSL_WANT_WRITE_TO_READ)) want_write = TRUE;
	if (F_ISSET(conn, SSL_WANT_READ_TO_WRITE)) want_read  = TRUE; /* already */
}
if (want_read)  { FD_SET(fd, reads);  maxfd = max; }
if (want_write) { FD_SET(fd, writes); maxfd = max; }
```

`__repmgr_conn_work` (the dispatcher) becomes direction-agnostic: on *either*
readable or writable it calls `__repmgr_conn_io`, which (a) if handshaking,
drives `__repmgr_ssl_handshake` and returns; (b) else, if writable and there's
outbound work, drives `__repmgr_write_some`; (c) else/also, if readable, drives
`__repmgr_read_from_site`. The want-bits are cleared and re-set by each
`SSL_*` call, so the *next* loop iteration selects the correct direction. This
is the standard "reflect SSL_get_error into the event mask" technique; the
single-threaded, mutex-held loop makes it race-free (no SSL object is touched
by two threads).

**Inline-write caveat.** `__repmgr_send_internal` writes inline in the caller's
thread when the outbound queue is empty. Under TLS that first `SSL_write` can
return WANT_READ. We handle it exactly like a short/`WOULDBLOCK` write today:
set `SSL_WANT_READ_TO_WRITE`, enqueue the flattened remainder, wake the main
thread (`__repmgr_wake_main_thread`). The select loop then completes it. No new
blocking is introduced on the app thread.

### 4. The handshake phase

`reading_phase` gains `HANDSHAKE_PHASE` (ordered before `SIZES_PHASE`). A new
connection is created with `ssl_handshaking = TRUE` when `ssl != NULL`:

- **Outbound** (`__repmgr_finish_connect`): after the socket connects, create a
  client `SSL` (`__repmgr_ssl_conn_init(..., is_client=1)`), set
  `SSL_set_connect_state`, mark handshaking, and let the select loop drive
  `SSL_do_handshake` across WANT_READ/WANT_WRITE until it completes; only then
  do we send the version-negotiation message.
- **Inbound** (`__repmgr_accept`): after `accept()`, create a server `SSL`
  (`is_client=0`, `SSL_set_accept_state`), mark handshaking. The server does
  not read the version proposal until the handshake completes. Because the TLS
  ClientHello is the very first thing a TLS client sends, and a plaintext peer
  would instead send repmgr's version bytes, the server can tell them apart —
  see "plaintext peers" below.

The handshake verifies the peer certificate (mutual auth): `SSL_CTX` is built
with `SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT` on both ends when
`DB_REPMGR_TLS_VERIFY_PEER` is set (the default), against the configured CA
bundle. Optional **certificate pinning**: if a SHA-256 fingerprint list is
configured, a verify callback additionally requires the peer leaf cert's
fingerprint to be in the list (defends against a compromised CA).

### 5. The iovec send path

`__repmgr_write_iovecs` currently calls `__repmgr_writev` with the scatter
array. For TLS we cannot scatter into one `SSL_write` record cheaply. Chosen
approach (simplest correct): **write the iovec segments sequentially** through
`__repmgr_ssl_write`, advancing `v->offset`/partial exactly as the existing
`__repmgr_update_consumed` logic does, treating WANT_WRITE/WANT_READ like a
short write. OpenSSL coalesces small records internally; repmgr messages are a
9-byte header + up to two DBTs, so at most ~3 segments. We keep
`SSL_MODE_ENABLE_PARTIAL_WRITE` and `SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER` off
(default) — meaning once an `SSL_write` of a segment starts with WANT_*, we
must retry with the *same* pointer and length, which the connection's saved
offset guarantees.

### 6. Configuration API

New method on `DB_ENV` (additive, needs the coordinated soname bump already in
flight for Q6):

```c
int DB_ENV->repmgr_set_tls(DB_ENV *dbenv,
    const char *cert_file,   /* this site's certificate (PEM) */
    const char *key_file,    /* this site's private key (PEM) */
    const char *ca_file,     /* CA bundle to verify peers (PEM) */
    u_int32_t flags);
```

Flags:

- `DB_REPMGR_TLS_VERIFY_PEER` — require and verify a peer certificate. **On by
  default** (set unless `DB_REPMGR_TLS_NO_VERIFY` is passed; a no-verify mode
  exists only for bootstrapping/testing and logs a warning).
- `DB_REPMGR_TLS_REQUIRE` — refuse any plaintext peer; every connection must be
  TLS. Without it, a mixed group is allowed during rollout (see below).

A matching set of DB_CONFIG directives (parsed in `env_config.c`, the complex
path like `repmgr_site`), so no code change is needed to turn it on:

```
repmgr_tls_cert   <path>
repmgr_tls_key    <path>
repmgr_tls_ca     <path>
repmgr_tls_pin    <sha256-hex>      # repeatable; optional pinning
repmgr_tls_flags  require,verify    # or no-verify
```

Private-key passphrase: if the key is encrypted, the passphrase is taken from
the same source as `DB_ENV->set_encrypt`'s password callback (no new secret
channel); documented in the guide.

### 7. Plaintext peers and rollout

To let an operator turn on TLS without a flag-day, the server distinguishes a
TLS ClientHello from a plaintext repmgr version message by peeking the first
byte (`recv(..., MSG_PEEK)`): a TLS 1.x record always begins with content-type
`0x16` (handshake) and version bytes `0x03 0x0{1,3,4}`; repmgr's first byte is
its message-type/handshake marker, which is not `0x16`. If the first byte is
not a TLS record and `DB_REPMGR_TLS_REQUIRE` is set, the connection is dropped
with a logged "plaintext peer refused" event. If REQUIRE is not set, the
connection proceeds in plaintext (`ssl = NULL`). This gives a clean rollout:
bring every site up with TLS configured but REQUIRE off, confirm all
connections are TLS (a new stat counter, below), then set REQUIRE everywhere.

### 8. Base-replication API users

For applications that provide their own transport via
`DB_ENV->rep_set_transport`, confidentiality is the transport callback's job.
We (a) document this explicitly in the replication guide, and (b) ship an
example transport in `examples/c/ex_rep/` that wraps the sockets in TLS using
the same OpenSSL calls, so an application has a worked reference. No library
code change is needed for this item.

### 9. Windows

`repmgr_windows.c` uses `WSAEventSelect`/`WSAWaitForMultipleEvents` instead of
`select()`. The same want-bit mapping applies: the event mask requested for a
connection's `WSAEventSelect` becomes `FD_READ|FD_WRITE|FD_CLOSE` with the
direction gated by the same `SSL_WANT_*` bits. OpenSSL on Windows uses the same
`SSL_*` API over a Winsock `SOCKET`. We **design** for Windows here and share
`repmgr_tls.c` (it is OS-agnostic OpenSSL), but the event-loop wiring in
repmgr_windows.c is implemented best-effort; if CI cannot validate it, the RFC
and guide **state the gap** and Windows TLS ships as "untested". The POSIX path
is the deliverable this cycle.

### 10. Statistics & observability

Add to `DB_REPMGR_STAT`: `st_connections_tls` (count of TLS connections) and
`st_connections_plaintext`. A per-connection TLS state is visible in the
existing connection diagnostics. These let an operator confirm a rollout
reached full-TLS before setting REQUIRE.

## Investment items (separate commits, after TLS lands)

Per Q2, as smaller follow-on commits on this branch:

- **(a) Bulk default.** Measure `DB_REP_CONF_BULK` on/off for throughput and
  latency over TLS and plaintext; if bulk is measured safe and a net win,
  switch the default to on. Format-free (runtime flag). Reported with numbers,
  or left off with the reason.
- **(b) Many-client, high-contention test** (review item A13): N sites, writers
  on the master, readers on the clients, a forced election mid-load, then
  verify convergence (all sites agree on the final DB) and no lost acknowledged
  commit. Exercises the rebuilt lock striping and CLOCK eviction under
  replication. Ships as a `test/db/run_rep_*.sh`.
- **(c) `rep_lease.c` coverage.** Master-leases path currently has 0% coverage
  (`REPLICATION-COVERAGE.md`). Add a leases test (grant, expiry, read on a
  client under a valid lease, refusal after expiry) so the lease code is
  exercised.

## Tests

The core FAIL-before / PASS-after test is a **wire capture**:

1. Start a master + client replicating a known marker key/value.
2. Capture the loopback traffic with an `LD_PRELOAD` shim that logs every
   `send()`/`write()` payload (no root needed, portable, deterministic — chosen
   over tcpdump).
3. **Without TLS**: the marker bytes MUST appear in the capture (proves the
   leak exists). **With TLS**: the marker MUST NOT appear (proves it is fixed).

Plus:

- Handshake success with valid certs on both ends.
- Refusal with a wrong CA; with an expired cert; with a plaintext peer when
  `DB_REPMGR_TLS_REQUIRE` is set.
- Elections and failover over TLS (ties into investment (b)).
- Large-message / bulk transfer over TLS, forcing partial `SSL_write`s.
- The CI TCL rep tests (whichever `.github/workflows` runs) pass with TLS on
  where a harness hook exists.
- The diagnostic `test/db` tier stays green.
- `--enable-tls` added to `test/config/option_sweep.sh` (gate G14).

## Benchmark

Replication throughput — master commits/second with clients acknowledging per
policy — TLS vs plaintext, on one host and (if time allows) two, so the TLS
overhead is a known number. Reported with medians, CV, committed-ops-only.

## Alternatives considered

- **GnuTLS / mbedTLS instead of OpenSSL.** mbedTLS is attractive for embedded
  (small, permissive), but OpenSSL is already present on every target platform,
  its non-blocking memory-BIO model matches repmgr's loop, and operators
  already have OpenSSL cert tooling. We keep the TLS calls behind
  `repmgr_tls.c` so a second backend is a later, contained addition.
- **IPsec / stunnel / an external tunnel.** Zero code, but it is not embedded,
  needs an operator-run daemon, and gives no in-process peer identity to
  repmgr. Rejected as the default answer; it remains available to anyone who
  does not enable `--enable-tls`.
- **A new encrypted wire protocol of our own** (reuse the DB_ENCRYPT AES keys
  to encrypt messages). Rejected: rolling our own transport crypto is exactly
  the mistake the review warns against (MT-IV, region password); no forward
  secrecy, no peer auth, no cert ecosystem.
- **Bump the repmgr wire-protocol version to negotiate TLS in-band.** Rejected:
  TLS must wrap the stream from the first byte; there is no plaintext phase in
  which to negotiate safely. Configuration + the first-byte peek is simpler and
  strictly safer.

## Risks & open questions

- **Loop-wiring bugs** (the WANT_READ/WANT_WRITE mapping) are the main
  correctness risk; mitigated by the single-threaded mutex-held loop and a
  partial-write stress test.
- **Private-key handling.** Keys on disk are the operator's responsibility; we
  document permissions and the passphrase source, and never log key material.
- **Windows** event-loop wiring may not be validated in CI — stated as a gap if
  so.
- **Open:** whether to offer a session-resumption / ticket cache for frequent
  reconnects (deferred; TLS 1.3 0-RTT is explicitly **not** used — it is unsafe
  for non-idempotent replication messages).

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Accepted | Rejected — YYYY-MM-DD
- **Rationale:**
- **Conditions / follow-ups:**
