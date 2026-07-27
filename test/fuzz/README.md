# Fuzz harnesses (libFuzzer / AFL++) — `test/fuzz/`

Coverage-guided fuzz harnesses for libdb, targeting the highest-bug-yield
surfaces: the on-disk **page parser**, the **recovery/log replay** path, and
the **public API call-sequence**.  This is Tier C of
`.agents/test-suite-maturity-plan.md` — the SQLite `dbsqlfuzz` / TigerBeetle
VOPR axis: continuous, mutation-driven, ASan+UBSan builds.

Each harness is a standard `LLVMFuzzerTestOneInput(const uint8_t*, size_t)`
that is:

- **deterministic per input** — same bytes ⇒ same run;
- **isolated** — a fresh scratch dir + fresh `DB_ENV` per input, removed
  before returning, so no state leaks between inputs;
- **self-cleaning** — temp files are unlinked.

## Harnesses

| File | Surface | What it does |
|------|---------|--------------|
| `fuzz_dbfile.c`  | page parser (`__db_verify`, mpool page-load, cursor walk) | Writes the fuzz bytes to a temp `.db`, opens it in a `DB_PRIVATE` env, runs `DB->verify` **and** a read-only full cursor scan.  A crash/ASan-fault on a malformed page is the bug signal. |
| `fuzz_recover.c` | recovery / log replay (`__db_apprec`, `__log_get`) | Builds a real minimal txn env, **overwrites** the first log file (`log.0000000001`) with the fuzz bytes, then opens with `DB_RECOVER` so replay parses the mutated log. |
| `fuzz_api.c`     | public API (`db.h`) call sequences | Interprets the input as a **bytecode** of DB operations (put/get/del, cursor next/prev/del, txn begin/commit/abort, sync) against a `DB_PRIVATE` env.  Finds operation-sequence edge cases. |

Shared helpers: `fuzz_util.h` (per-input scratch dir + file write) and
`fuzz_driver.h` (the standalone `main()` — see below).

## Requirements

A `clang` that supports `-fsanitize=fuzzer,address,undefined` and, for the
libFuzzer mode, the libFuzzer runtime (bundled with clang's `compiler-rt`).
Both are present in this repo's `nix develop` shell (verified with clang 21).

Everything runs from inside the dev shell:

```sh
nix develop . --command bash -c 'cd test/fuzz && ./run.sh smoke 60'
```

## Build

libdb must be built first (the harnesses link `build_unix/libdb.a`):

```sh
cd build_unix && ../dist/configure --enable-debug && make -j4
```

Then, from `test/fuzz/`:

```sh
./run.sh build          # build all three libFuzzer harnesses into build/
```

`run.sh` reads libdb's own link deps (`LIBS`, e.g. `-luring -lpthread`) from
the generated `build_unix/Makefile`, so it stays in sync with the build.

## Run (smoke fuzz)

```sh
./run.sh smoke [SECONDS]   # default 60; runs each harness bounded
```

Each harness runs for `SECONDS`, seeded from `corpus/<harness>/` (copied into
a scratch working corpus so the tracked seeds stay pristine).  Any crashing
input is written to `build/artifacts_<harness>/`.

Manual invocation for a longer run:

```sh
./build/fuzz_dbfile corpus/dbfile -max_total_time=600 -max_len=65536 \
    -artifact_prefix=build/artifacts_dbfile/
```

## Standalone mode (no libFuzzer runtime)

Define `FUZZ_STANDALONE=1` and you get a plain `main()` that reads each file
argument and feeds its bytes to `LLVMFuzzerTestOneInput` once — the OSS-Fuzz
"reproduce a testcase" contract.  This needs only `-fsanitize=address,undefined`
(no libFuzzer), so it also builds under AFL++ or a bare sanitizer toolchain.

## Reproduce a crash from a saved input

```sh
./run.sh repro <harness> crashes/<file>.seed
# <harness> = dbfile | recover | api
```

This builds the standalone driver for that harness and replays the one input
under ASan/UBSan, printing the faulting stack.

## Add a regression seed

When a run finds a crash, commit the reproducing input to `crashes/`:

```sh
cp build/artifacts_<harness>/crash-XXXX crashes/<harness>_<slug>.seed
```

See `crashes/README.md` for the naming convention and the findings from the
bootstrap run.

## Seed corpus

`corpus/<harness>/valid.*` are tiny, structurally valid inputs that bootstrap
coverage:

- `corpus/dbfile/valid.db`  — a real 2-record btree `.db` file
- `corpus/recover/valid.log` — a real first log file from a txn env
- `corpus/api/valid.ops`    — a hand-built valid op-bytecode program

Regenerate them with `make_seeds.c` (built and run by hand; not part of CI):

```sh
clang -I../../build_unix -I. make_seeds.c ../../build_unix/libdb.a \
    $(sed -n 's/^LIBS=[[:space:]]*//p' ../../build_unix/Makefile) -ldl \
    -o build/make_seeds && ./build/make_seeds .
```

## AFL++

The standalone driver (`FUZZ_STANDALONE=1`) is AFL++-compatible.  Build with
`afl-clang-fast` and run `afl-fuzz -i corpus/<h> -o findings -- ./fuzz_<h>_standalone @@`.
Not wired into CI yet.

## Future: OSS-Fuzz

Once the harnesses stabilize (Tier C2), an OSS-Fuzz `project.yaml` + `build.sh`
would compile these three `LLVMFuzzerTestOneInput` targets against `libdb.a`
with the OSS-Fuzz sanitizer flags and ship the `corpus/` here as the seed
corpus.  The harnesses are already written to the OSS-Fuzz contract (one
`LLVMFuzzerTestOneInput` per target, deterministic, self-cleaning), so that
step is mostly packaging.

## CI

`.github/workflows/fuzz.yml` (a **dedicated** workflow, separate from
`ci.yml`) builds all three harnesses with
`clang -fsanitize=fuzzer,address,undefined` and runs each for a bounded time
as an advisory smoke fuzz on every PR (`continue-on-error: true`).  Crashing
inputs are uploaded as an artifact for triage.

> **Note on placement:** if this branch is pushed by a token without the
> GitHub `workflow` scope, the workflow cannot be committed at
> `.github/workflows/fuzz.yml` directly; the identical file is then staged at
> `test/fuzz/fuzz.yml.workflow` and a maintainer moves it into place:
>
> ```sh
> git mv test/fuzz/fuzz.yml.workflow .github/workflows/fuzz.yml
> ```
