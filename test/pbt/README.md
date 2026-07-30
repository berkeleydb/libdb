# Property-Based Tests (Hegel) — `test/pbt/`

Property-based tests (PBT) for libdb, using [Hegel](https://github.com/gburd/hegel-c)
(a Hypothesis-backed PBT library for C). Hegel generates random inputs,
checks a *property* over each, and shrinks any failing case to a minimal
counterexample.

These tests are **opt-in** and **off by default**: the core library build
never depends on Hegel. When Hegel is not configured, every test still
*compiles and links against libdb* (proving the symbols under test are
reachable) and then prints `SKIP` and exits 0, so CI stays green without the
`hegel` binary installed.

## Layout

| File | Property target | Code under test |
|------|-----------------|-----------------|
| `pbt_common.h`      | runner + graceful stub mode | — |
| `pbt_log_compare.c` | LSN total order | `log_compare()` (public `db.h`; `src/log/log_compare.c` → `LOG_COMPARE` in `src/dbinc/db_int.in`) |
| `pbt_byteswap.c`    | byte-order swap involution | `M_16_SWAP` / `M_32_SWAP` / `M_64_SWAP` (`src/dbinc/db_swap.h`) |
| `pbt_defcmp.c`      | key-comparator total order | `__bam_defcmp()` (`src/btree/bt_compare.c`) |
| `pbt_put_get.c`     | put/get round-trip (end-to-end) | in-memory B-tree via `db_create` + `DB->open`/`put`/`get` |
| `pbt_hash_model.c`  | DB_HASH vs. model map (stateful) | in-memory `DB_HASH` via `db_create` + `DB->open`/`put`/`del`/`get` |
| `pbt_hash_func.c`   | key-hash determinism / length / FNV | `__ham_func2/3/4/5` (`src/hash/hash_func.c`) |
| `pbt_compint.c`     | varint codec round-trip / order-preserving | `__db_compress_int` / `__db_decompress_int` / `__db_*_count_int` / `__db_decompress_int32` (`src/common/db_compint.c`) |
| `pbt_compress.c`    | prefix-compression codec round-trip | `__bam_defcompress` / `__bam_defdecompress` (`src/btree/bt_compress.c`) |
| `pbt_recno.c`       | recno append/get + DB_RENUMBER contiguity | in-memory `DB_RECNO` (`src/btree/bt_recno.c`) |
| `pbt_getlong.c`     | string->number parse (round-trip / range / robustness) | `__db_getlong` / `__db_getulong` (`src/common/db_getlong.c`) |
| `pbt_defpfx.c`      | prefix-length separator | `__bam_defpfx()` (`src/btree/bt_compare.c`) |
| `pbt_lsn.c`         | LSN wire codec (structured record) | `LOGCOPY_TOLSN` / `LOGCOPY_FROMLSN` (`src/dbinc/db_swap.h`) |
| `pbt_chksum.c`      | page/log checksum compute + verify | `__db_chksum` / `__db_check_chksum` (`src/hmac/hmac.c`) |

## Property catalog

### `pbt_log_compare` — `log_compare()` total order
Source contract: *"Compare two LSN's; return 1, 0, -1 if first is >, == or <
second."* A `DB_LSN` is `(file, offset)` ordered lexicographically.
- **result_in_range** — result is always in `{-1, 0, 1}`.
- **reflexive** — `log_compare(a, a) == 0`.
- **antisymmetric** — `log_compare(a, b) == -log_compare(b, a)`.
- **transitive** — `a<=b ∧ b<=c ⇒ a<=c` (and the `>=` / `==` variants).

### `pbt_byteswap` — swap involution
The `M_*_SWAP` macros swap between big- and little-endian in place. Byte
reversal is an involution.
- **swap16/32/64_involution** — `swap(swap(x)) == x` for all inputs.
- **swap32_reverses_bytes** — one swap equals a manual 4-byte reversal.
- **copyswap32/16_unaligned** — `P_32_COPYSWAP` / `P_16_COPYSWAP` reverse
  bytes into an *unaligned* destination (the form used for on-page fields).
- **swap64_unaligned** — `P_64_SWAP` on an unaligned 8-byte location both
  reverses (one call) and restores (two calls), at a random byte offset.

### `pbt_defcmp` — `__bam_defcmp()` total order
Source contract: returns `< 0 / = 0 / > 0` for `a < / = / > b`, comparing byte
strings lexicographically with shorter-is-less. Returns *raw* differences (not
clamped), so we assert only sign relations.
- **reflexive**, **antisymmetric** (in sign), **transitive** (in sign).
- **matches_oracle** — agrees in sign with an independent `memcmp`
  + length-tiebreak oracle.

`__bam_defcmp` is exported from libdb (verified via `nm`). Its prototype lives
in an internal header, so the test declares it locally to avoid pulling in the
full `db_int.h` include tree.

### `pbt_put_get` — B-tree put/get round-trip (end-to-end)
Opens a real in-memory B-tree (the idiom from `test/micro/source/b_inmem.c`:
`db_create(&dbp, NULL, 0)` then `dbp->open(dbp, NULL, NULL, NULL, DB_BTREE,
DB_CREATE, 0600)`), then:
- **put_get_roundtrip** — `put(k, v)` then `get(k)` returns exactly `v`.
- **get_missing_notfound** — `get` of an absent key returns `DB_NOTFOUND`.

### `pbt_hash_model` — DB_HASH vs. model map (Tier-1 stateful)
Runs a random `put`/`del`/`get` sequence against a real in-memory `DB_HASH`
and a simple in-test array-map model, asserting they agree after every
operation (and at the end over the whole key pool). Keys are drawn from a
small fixed pool so overwrites, deletes of present/absent keys, and re-inserts
occur frequently.

### `pbt_compint` — integer-compression (varint) codec
`src/common/db_compint.c` is the self-describing varint codec (1–9 bytes,
high-bit length prefix) used for on-disk lengths/offsets in the compressed
btree. Built only when `HAVE_COMPRESSION` is defined (the default). The
source header comment gives the exact format table and states it "depends on
big-endian order".
- **roundtrip** — `decompress(compress(i)) == i` over the full `u_int64_t`
  range (drawn as signed bits reinterpreted, so 9-byte encodings are hit).
- **count_agrees** — `__db_compress_count_int(i)` equals the bytes
  `__db_compress_int` writes, `__db_decompress_count_int(buf)`, and the length
  `__db_decompress_int` reports.
- **order_preserving** — the encoding sorts like the integers: `a<=b` iff the
  encoded bytes compare `<=` (common-prefix compare, longer encoding wins
  ties). This is what lets the codec store sortable keys.
- **int32_matches** — for `i <= UINT32_MAX`, `__db_decompress_int32` agrees
  with `__db_decompress_int` on both value and byte count.

### `pbt_compress` — btree prefix-compression codec
`__bam_defcompress` writes a compressed encoding of a `(key, data)` pair
expressed relative to the preceding `(prevKey, prevData)`; `__bam_defdecompress`
reverses it. (`src/btree/bt_compress.c`, `HAVE_COMPRESSION`.)
- **compress_roundtrip** — `decompress(compress(prev, cur)) == cur`
  byte-for-byte, for arbitrary preceding and current key/data byte strings
  (sizes chosen to hit both long-shared-prefix and no-prefix paths). This is
  the invariant the whole compressed on-disk format rests on.

### `pbt_recno` — recno access method (end-to-end)
Opens a real in-memory `DB_RECNO` and drives it through the public API.
- **put_get_roundtrip** — a value appended with `DB_APPEND` reads back
  byte-for-byte at its assigned record number.
- **renumber_contiguous** — with `DB_RENUMBER`, after any generated sequence
  of appends and in-range deletes, a cursor walk (`DB_NEXT`) yields record
  numbers `1, 2, 3, …` with no gaps, and exactly `N − deletes` records
  survive (the recno renumber contract in `bt_recno.c __ramc_del`).

### `pbt_getlong` — `__db_getlong` / `__db_getulong` numeric-argument parse
String→number parsers for utility/config arguments. Returns `0` on success,
`EINVAL` on empty/trailing-garbage, `ERANGE` on overflow or out-of-`[min,max]`.
This is pure string logic the tcl suite never exercises directly
(`db_getlong.c` sits at ~47% line coverage in the tcl COV run), so the PBT
tier closes that gap.
- **roundtrip_in_range** — `%ld`-formatted value parsed back within a
  covering window returns `0` and the identical value.
- **rejects_out_of_range** — an in-range integer parsed against a window
  that *excludes* it returns `ERANGE` and leaves `*storep` untouched.
- **rejects_trailing** — digits followed by a non-digit, non-newline byte
  are `EINVAL`; a trailing `\n` is tolerated (the `end[0]` guard).
- **robust_no_crash** — arbitrary bytes never crash, always return
  `{0, EINVAL, ERANGE}`, and on success land in `[min,max]`.
- **getulong_zero_is_max** — `__db_getulong` treats `max == 0` as
  "unbounded" (documented `ULONG_MAX` substitute).

### `pbt_defpfx` — `__bam_defpfx()` prefix-length separator
The default prefix routine used during splits/compaction to pick the shortest
key that still separates two neighbours. Returns the count of leading bytes
of the larger key that must be retained.
- **bounded** — `pfx <= min(la,lb)+1` always; when the longer key is
  non-empty, `1 <= pfx <= max(la,lb)`; two empty keys give `pfx == 0`
  (a real boundary — the source returns `b->size`, asserted explicitly).
- **locates_diff** — when the keys differ inside the common region, `pfx-1`
  is exactly the index of the first differing byte, and everything before
  it is equal in both keys.
- **symmetric** — `__bam_defpfx(a,b) == __bam_defpfx(b,a)`.
- **separates** — consistency with `__bam_defcmp`: truncating the strictly
  greater key to `pfx` bytes still sorts strictly after the smaller key
  (the invariant a split relies on when it stores only the prefix).

### `pbt_lsn` — LSN wire codec (`LOGCOPY_TOLSN` / `LOGCOPY_FROMLSN`)
Extends the scalar swap coverage in `pbt_byteswap` to a whole *structured
record*: the two-field `DB_LSN` `(file, offset)` marshalled to/from an 8-byte
log-wire buffer. Logs are written little-endian on disk for portability.
- **lsn_roundtrip** — `FROMLSN` then `TOLSN` is the identity in either
  endian mode.
- **wire_is_canonical** — with an *honest* env flag (matching the host,
  detected at run time), the wire bytes are little-endian canonical
  (compared against an independent hand-marshalling). You cannot simulate
  the other endianness on one host — the swap is relative to native order.
- **field_order** — `.file` occupies wire `[0..3]`, `.offset` `[4..7]`:
  swapping the two LSN fields swaps the two 4-byte halves of the wire.
- **lsn_roundtrip_unaligned** — the round-trip holds at any byte offset
  (the codec is byte-at-a-time, so must not assume 4-byte alignment).

### `pbt_chksum` — `__db_chksum` / `__db_check_chksum` (non-crypto path)
The page/log checksum on the `mac_key == NULL, is_hmac == 0` path: a plain
4-byte hash over the data (via `__ham_func4`), plus its verifier.
- **deterministic** — the same bytes checksum identically (pure function).
- **verify_accepts** — a checksum from `__db_chksum` verifies against the
  same data.
- **detects_bitflip** — flipping any single data bit makes the stored
  checksum no longer verify (a corrupt page is caught).
- **detects_wrong_sum** — flipping a bit of the *stored checksum* makes
  verify reject.
- *Deliberate non-property:* this bare hash does **not** detect truncation
  — `__ham_func4` returns 0 for an empty buffer and for any all-zero prefix,
  so `hash("") == hash("\0\0") == 0`. Torn-page/truncation detection in real
  BDB comes from the `HDR` `prev`/`len` XOR on the log path (`hdr != NULL`),
  which this pure-hash test avoids on purpose.

## IMPORTANT — `hegel_assume` is a filter, not an assertion

Use **`PBT_CHECK(cond, msg)`** (from `pbt_common.h`) for every actual
property. It calls `hegel_fail()` when the condition is false, which marks
the case INTERESTING and lets the server shrink a counterexample.

`hegel_assume(cond)` is a **precondition filter**: a false condition marks
the case INVALID (skipped), *not* failed. Empirically confirmed against
hegel-c 0.10.0 — a property written as `hegel_assume(property)` can **never
fail**; violating inputs are silently skipped. Reserve `hegel_assume()` for
genuine domain constraints (e.g. `hegel_assume(malloc_result != NULL)`), and
assert real properties with `PBT_CHECK`.

> Known debt: the nine *pre-existing* `pbt_*.c` files (log_compare, byteswap,
> defcmp, put_get, hash_model, hash_func, compint, compress, recno) express
> their properties with `hegel_assume`, so those assertions currently skip
> rather than fail. They should be converted to `PBT_CHECK` in a follow-up.
> The four files added here (getlong, defpfx, lsn, chksum) use `PBT_CHECK`.

## Building and running

### Meson (primary path)

PBT is gated on the `hegel` feature option (default `disabled`).

```sh
# Stub mode (hegel-c not required): tests compile+link against libdb, then SKIP.
meson setup build -Dhegel=enabled
ninja -C build
meson test -C build --suite pbt
```

To run the **real** property tests you need:

1. **hegel-c** (the C library). Meson cannot `FetchContent` a CMake project the
   way CMake does, so hegel-c must be *found* at configure time. Build and make
   it discoverable:

   ```sh
   git clone https://github.com/gburd/hegel-c.git
   cmake -S hegel-c -B hegel-c/build -DHEGEL_BUILD_TESTS=OFF
   cmake --build hegel-c/build
   cmake --install hegel-c/build --prefix "$PWD/hegel-prefix"
   export PKG_CONFIG_PATH="$PWD/hegel-prefix/lib/pkgconfig:$PKG_CONFIG_PATH"
   export CMAKE_PREFIX_PATH="$PWD/hegel-prefix:$CMAKE_PREFIX_PATH"
   ```

   `test/pbt/meson.build` looks for `dependency('hegel')` (falling back to
   `cc.find_library('hegel')`) plus `libcbor` and `zlib`. If all three are
   found it compiles with `-DPBT_HAVE_HEGEL` and links `-lhegel -lcbor -lz`;
   otherwise it falls back to stub mode (still compiles+links, then SKIPs).

2. **The `hegel` server binary** (from `hegel-core`), spawned as a subprocess
   at run time:

   ```sh
   pip install hegel-core        # provides the `hegel` executable
   ```

   Override its path with `HEGEL_SERVER_COMMAND` if it is not on `PATH`:

   ```sh
   HEGEL_SERVER_COMMAND=/path/to/hegel meson test -C build --suite pbt
   ```

### Autoconf

PBT wiring is **Meson-only** for now. The Autoconf/`build_unix` tree remains
the reference build for the shipping library and language bindings; adding a
CMake-FetchContent-style dependency to it for a new opt-in test tier is not
worth the two-build-system maintenance cost. Use the Meson path above.

## Reproducing a failing seed

When a property fails, Hegel prints the shrunk counterexample and a **seed**.
The failure line looks like:

```
  [PBT] <suite>/<property> FAIL: <error / minimal counterexample>
```

The `hegel` server records the failing example in its local example database
(Hypothesis-style), so re-running the same test binary re-tries the last
failing case first. To capture full diagnostics in CI, run the test binary
directly and save its output — the failure text plus the recorded example is
the reproduction. Setting `settings.verbosity = 2` in `pbt_common.h` prints
every drawn example.

## Adding a property

1. Read the code under test first; ground the property in an actual contract
   (comment, signature, or an existing test). Do not invent contracts.
2. Add a `pbt_<name>.c` mirroring an existing file: define
   `prop_*` functions, list them in a `pbt_entry_t[]` table (both the
   `PBT_HAVE_HEGEL` and stub arms), and close with `PBT_MAIN`.
3. Add `'<name>'` to `pbt_props` in `test/pbt/meson.build`.
4. Prefer broad generators (full ranges, empty collections, boundary values);
   use `hegel_assume()` only for genuine domain constraints.
5. Assert the actual property with **`PBT_CHECK(cond, "msg")`**, never with
   `hegel_assume()` (which only *skips* the case on failure — see the
   "`hegel_assume` is a filter" note above).

## tcl-uncoverable gaps the PBT tier closes

Some pure logic cannot be reached from the tcl test harness, so it stays
uncovered in the tcl COV subset no matter how many tcl cases run. The PBT
tier targets exactly those:

- **`__db_getlong` / `__db_getulong`** (`pbt_getlong`) — `db_getlong.c` is
  ~47% line-covered by tcl; these string→number parsers have no tcl entry
  point. `pbt_getlong` drives them directly (round-trip, range rejection,
  overflow, robustness).
- **`__db_compress_int` / `__db_decompress_int` 64-bit path** (`pbt_compint`)
  — the btree compression that reaches this codec from tcl is 32-bit only,
  so the 9-byte / full-`u_int64_t` encodings are tcl-unreachable.
  `pbt_compint` draws the full 64-bit range (signed bits reinterpreted), so
  those large encodings are exercised.

The PBT tier is intentionally *not* part of the tcl COV subset (it is a
separate opt-in suite), but it is where these gaps get real coverage.
