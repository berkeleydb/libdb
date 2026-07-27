# Coccinelle convention checks + libabigail/nm ABI drift for libdb

This directory holds the source-level convention scanner (Coccinelle / `spatch`)
and the ABI-drift tooling wired into `.github/workflows/cocci.yml`.

## TL;DR: what is authoritative

| Concern | Tool | Authority |
| --- | --- | --- |
| Coding conventions, dangerous patterns (relaxed atomics on refcounts, one-arg `DB_ASSERT`, lock/malloc/`t_ret` mistakes) | **Coccinelle** (`rule_*.cocci`) | Source-level **early warning**. Lint gate on NEW violations. NOT an ABI guarantee. |
| Binary ABI: `/* SHARED */` struct layout, public symbol set | **libabigail** (`abidiff`) + `nm -D` | **Authoritative** ABI contract. |
| Generated headers hand-edited | `dist/s_include` + `git diff` | Blocking drift gate. |
| Flag-bit `#define` inventory | `flagbits_inventory.sh` (awk) authoritative; `abi_flagbits.cocci` (SmPL) cross-check | Early-warning context. |

Coccinelle is complementary. The hard ABI gate is libabigail/nm.

## The working spatch invocation (and why it is not the obvious one)

A bare `spatch --sp-file r.cocci --dir src/` finds almost nothing on this tree
(measured directory-scale recall ~13% on a probe rename). Two BDB-specific
problems defeat the stock parser:

1. **Macro-generated declarators.** `typedef SH_TAILQ_HEAD(__hash_head) DB_HASHTAB;`,
   `typedef int (*H) __P((...));`, `struct { SH_TAILQ_ENTRY links; }` — spatch
   cannot expand these on its own. `bdb_defs.h` (fed with `--macro-file`) teaches
   spatch the expansions (`__P`, `SH_TAILQ_HEAD/ENTRY`, `SH_LIST_HEAD/ENTRY`,
   `SH_CHAIN_ENTRY`, and the BSD `LIST_/TAILQ_/CIRCLEQ_` families), token-for-token
   with `src/dbinc/shqueue.h` and `src/dbinc/queue.h`.

2. **K&R function definitions.** Every BDB function is K&R:

   ```c
   int
   __memp_fput(dbmfp, ip, pgaddr, priority)
           DB_MPOOLFILE *dbmfp;
           DB_THREAD_INFO *ip;
           ...
   {
   ```

   This spatch build (1.3.0, even with `--force-kr`) cannot parse the parameter
   declarations after the paren list; it desyncs and silently **skips the whole
   function body**. That is where nearly all the interesting call sites live.

   `kr2ansi.py` rewrites, on a throwaway copy, `name(args)\n <decls>\n {` into
   `name(args) {\n <decls>` — moving the `{` onto the signature line and blanking
   the old `{` line. It is **line-count preserving**, so spatch's reported line
   numbers still map 1:1 to the real source.

The resulting recipe (see `run_conventions.sh`):

```sh
# 1. shim every .c (line-preserving K&R -> ANSI) into $SHIM
python3 dist/cocci/kr2ansi.py src/foo.c "$SHIM/src/foo.c"

# 2. run spatch against the shim, real headers on -I
spatch --force-kr \
  -I build_unix -I src -I src/dbinc -I src/dbinc_auto \
  --macro-file dist/cocci/bdb_defs.h \
  --sp-file dist/cocci/rule_relaxed_refcount.cocci \
  --dir "$SHIM/src"
```

### Measured recall (probe: rename `atomic_read`)

| Scope | grep baseline | spatch matches | recall |
| --- | --- | --- | --- |
| `src/mp` | 18 | 18 | 100% |
| full `src` (all `atomic_read` call sites) | 50 | 39 | 78% |
| full `src`, **call sites only** (excluding `src/os/os_atomic.c`, whose 10 sites are inside mutually-exclusive `#ifdef` backend tiers spatch cannot all see) | 40 | 39 | **97.5%** |

Up from the ~2/15 (~13%) bare-`spatch` baseline. The single remaining call-site
miss (`src/db/db_meta.c`) sits behind an unbalanced-brace macro that trips the
parser — one of the residual `db_int.h`-class parse errors.

### `@script:python@` is broken in this build

`spatch`'s Python scripting fails here (`Py.find_library_path` cannot parse the
`pkg-config` output). So the `find` rules are written as **identity transforms**
that append a `//@TAG@` marker; the produced diff *is* the report, and violations
are counted with `... | grep '//@TAG@'`. If a future spatch fixes Python, the
rules can be reworked to `@script:python@` for cleaner output.

## The rules

| File | Tag | What it flags | Status |
| --- | --- | --- | --- |
| `rule_relaxed_refcount.cocci` | `RELAXED_REFCOUNT` | `atomic_read_relaxed(&x->ref/refcount/writers/multiversion)` — these must stay ACQUIRE (`atomic_read`); the exact regression the atomics-ordering work guards against. Stats counters (`hash_page_dirty`, `nsireaders`, `wired_pages`) are allowed and not matched. | Blocking. Tree clean (0). Verified: catches a planted `bhp->ref` relaxed read, ignores legitimate `hash_page_dirty`. |
| `rule_dbassert_arity.cocci` | `DBASSERT_ARITY` | `DB_ASSERT(expr)` with one arg (needs `env, expr`). | Blocking. Tree clean (0). Verified: catches planted one-arg, ignores two-arg. |
| `rule_tret_clobber.cocci` | `TRET_CLOBBER` | `if ((t_ret = f()) != 0) ret = t_ret;` — missing the `&& ret == 0` guard, which clobbers an earlier error. | Advisory. 15 pre-existing matches (real unguarded clobbers, e.g. `db_cam.c` `__dbc_close`). In baseline. |
| `rule_mutex_unbalanced.cocci` | `MUTEX_UNBALANCED` | a `return` between `MUTEX_LOCK(env,m)` and its `MUTEX_UNLOCK`, no unlock and no `goto` in between. | Advisory. 7 pre-existing matches (e.g. `mp_register.c` lock leak on malloc-failure path). In baseline. |
| `rule_malloc_leak.cocci` | `MALLOC_LEAK` | `__os_malloc(&p)` then `return` with no `__os_free(...,p)` and no `goto` in between. | Experimental/advisory. 0 matches on the tree; low precision expected on this goto-heavy codebase, kept out of any hard gate. |

`abi_flagbits.cocci` (tag-free, report mode `*`) inventories hex flag `#define`s;
it is the SmPL/AST cross-check only. The authoritative inventory is
`flagbits_inventory.sh` (awk, 328/328 flag bits).

## Running locally

```sh
# one-time: generate build_unix/db.h + db_int.h so -I works
cd build_unix && ../dist/configure && cd ..

# all convention rules, normalized violation list (rule|tag|path|code-signature)
sh dist/cocci/run_conventions.sh

# a single rule against the whole tree (see the recipe above), e.g.
python3 dist/cocci/kr2ansi.py src/lock/lock.c /tmp/lock.c   # or shim a whole dir
spatch --force-kr -I build_unix -I src -I src/dbinc -I src/dbinc_auto \
  --macro-file dist/cocci/bdb_defs.h \
  --sp-file dist/cocci/rule_relaxed_refcount.cocci /tmp/lock.c | grep '//@'

# flag-bit inventory
sh dist/cocci/flagbits_inventory.sh

# header-regen drift check (should print nothing / exit 0 on a clean tree)
(cd dist && sh s_include) && git diff --exit-code src/dbinc_auto/
```

## The baseline and updating it

`baseline.txt` records the convention violations that already exist on `master`,
keyed as `RULE|TAG|relpath|code-signature` — **no line numbers**, so unrelated
edits that renumber lines do not reshuffle it. CI (`conventions` job) fails only
on lines present now but absent from `baseline.txt` (NEW violations).

To update after intentionally adding/removing/fixing a flagged pattern:

```sh
sh dist/cocci/run_conventions.sh > dist/cocci/baseline.txt
git add dist/cocci/baseline.txt
git commit -m "cocci: refresh convention baseline"
```

The CI report also lists violations that were *resolved* (in baseline, gone now)
so you know when to shrink the baseline.

## CI (`.github/workflows/cocci.yml`)

> **Note on placement:** this branch was pushed by a token without the GitHub
> `workflow` scope, so the workflow could not be committed at
> `.github/workflows/cocci.yml` directly. The identical file is staged at
> `dist/cocci/cocci.yml.workflow`; a maintainer (or any push with `workflow`
> scope) moves it into place with:
>
> ```sh
> git mv dist/cocci/cocci.yml.workflow .github/workflows/cocci.yml
> ```

- **`conventions`** — installs `coccinelle` (apt), runs `configure` to generate
  the headers, runs the rules, diffs against `baseline.txt`, posts a PR comment,
  and **fails on NEW violations**.
- **`abi-diff`** (advisory / `continue-on-error`) — installs `abigail-tools` +
  `coccinelle`, then:
  - **header-regen drift gate (BLOCKING):** `cd dist && sh s_include` then
    `git diff --exit-code src/dbinc_auto/`. A diff means a generated header was
    hand-edited.
  - builds PR head and the latest `v*` release tag (`git describe`), runs
    `abidiff base.so head.so` (authoritative layout/symbol diff),
  - computes removed exported symbols via `nm -D --defined-only` with the
    `_NNNN` version-unique-name suffix normalized away (this build does not apply
    it; normalization keeps the diff honest if a build does),
  - includes the flag-bit inventory as early-warning context,
  - posts it all as a single updating PR comment.
