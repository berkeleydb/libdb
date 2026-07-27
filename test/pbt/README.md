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
