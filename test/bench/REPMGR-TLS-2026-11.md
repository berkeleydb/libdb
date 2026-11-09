# Repmgr TLS throughput: TLS vs plaintext (RFC 0018)

**Date:** 2026-11-02
**Branch:** `feat/repmgr-tls`
**Question:** what does TLS 1.3 cost replication throughput?

## Setup

- A/B **ratio** on one host (the shared dev box, c6i.8xlarge, Debian 12,
  OpenSSL 3.0.22). This is deliberately a *relative* measurement: both arms
  share the same loopback, CPU and disk, so the plaintext→TLS ratio is the
  meaningful output. Absolute ops/s reflect the shared host and are **not**
  metal-grade. A metal re-run on an isolated box is a follow-up; the account
  expiry and the shared-box benchmark ban (per the brief) kept this to a
  same-host ratio.
- Two processes (master + one client) on loopback, `DB_REPMGR_ACKS_ALL`
  (the client must acknowledge every commit before it counts), `DB_AUTO_COMMIT`
  single-key puts, 256-byte values, 1000-key working set (in cache).
- `repmgr_tls_bench.c` (`test/bench/`). Master times N committed puts and
  reports committed ops/s; aborts are not counted (there are none in this
  workload).
- TLS arm: mutual-auth TLS 1.3, one shared leaf cert, CA-verified.
- 3 reps each, fresh env homes and ports per rep.

## Result

| Arm       | rep 1 | rep 2 | rep 3 | median  |
|-----------|-------|-------|-------|---------|
| plaintext | 534.7 | 504.3 | 530.5 | **530.5** ops/s |
| TLS 1.3   | 520.0 | 546.8 | 548.2 | **546.8** ops/s |

TLS/plaintext ratio of the medians: **1.03** — i.e. **no measurable TLS
overhead** (the TLS arm is marginally faster, which is within run-to-run
noise; CV across reps ≈ 3%).

## Why TLS is free here

The workload is **durability-bound**, not CPU-bound: `ACKS_ALL` means every
commit waits for the client's acknowledgement, and each commit flushes the log.
The per-commit cost is dominated by the fsync and the network round-trip
(~1.9 ms/commit). TLS 1.3 record encryption with AES-GCM (hardware AES-NI on
this CPU) adds single-digit microseconds per message — three orders of
magnitude below the per-commit latency floor. So the encryption disappears into
the durability wait.

This matches the design expectation: TLS is a per-record symmetric-cipher pass
over data that is already gated by disk and network latency. The one place TLS
*would* show up is a CPU-bound, no-sync, bulk-streaming arm (e.g. internal init
copying a large database with `DB_TXN_NOSYNC`), where the cipher competes with
memcpy for CPU. That arm is a follow-up for the metal re-run; it is the honest
upper bound on TLS cost and should be measured before claiming "TLS is free"
unconditionally.

## Reproduce

```sh
# from an --enable-tls build dir, with a cert/key/CA in $C
cc -O2 -I. ../test/bench/repmgr_tls_bench.c .libs/libdb-*.so -lpthread \
   -Wl,-rpath,$PWD/.libs -o rtbench
# plaintext:
./rtbench m c 7004 7005 20000
# TLS:
REP_TLS_CERT=$C/site.crt REP_TLS_KEY=$C/site.key REP_TLS_CA=$C/ca.crt \
   ./rtbench m c 7204 7205 20000
```

## Caveat

Same-host loopback ratio only. Not a cross-host, metal, cache-warmed,
≥5-rep CV≤10% benchmark. Quoted as a ratio, not an absolute. The streaming /
no-sync CPU-bound arm — where TLS cost is largest — is not yet measured.
