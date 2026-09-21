# Testing program: current coverage, and how to improve it

Measured 2026-09-20 against master, to answer four questions directly rather than by
impression.

## Q1. Does CI test all permutations of compile-time options?

**Accounted for, yes; permuted, no — and permuting them fully is not the goal.**

`test/config/option_sweep.sh --check-complete`:

```
declared in dist/aclocal/options.m4: 54
swept with a smoke run:              29
swept build-only:                     4
excluded with a reason:              21
completeness gate: OK
```

Every option is in exactly one of three lists, and an option in none of them **fails the
gate** — so adding a `configure` option now forces a testing decision. That is the
property that matters, and it is what found `U7` (`--disable-mutexsupport` had never
built, three stacked defects) and `P2` (`--enable-o_direct` built fine and was
non-functional).

What is *not* covered: **interactions**. The sweep is one-at-a-time. 2^54 is not a
target, but a small set of *known-interacting* pairs is — e.g. `diagnostic × o_direct`,
`smallbuild × statistics`, `mutexsupport × atomicsupport`, `replication × cryptography`.
Those should be a named list with reasons, not a random sample.

## Q2. Does CI test the API flags and options?

**No — this is the biggest remaining hole.**

| | count |
|---|---:|
| public API flags declared in `api_flags.in` | **229** |
| referenced anywhere under `test/` | 117 |
| **never referenced by any test** | **112 (48%)** |

Examples of the untested: `DB_AGGRESSIVE`, `DB_ARCH_DATA`, `DB_ARCH_LOG`,
`DB_ARCH_REMOVE`, `DB_BACKUP_NO_LOGS`, `DB_BACKUP_UPDATE`, `DB_CDB_ALLDB`,
`DB_CKP_INTERNAL`, `DB_CURSOR_BULK`, `DB_CURSOR_TRANSIENT`, `DB_DATABASE_LOCKING`,
`DB_DURABLE_UNKNOWN`.

And "referenced" is a weak bar. `G15` established that a flag can be *referenced*,
*counted as covered*, and still be completely broken: `cov_api_surface.c` asserted only
that `set_flags` **accepted** `DB_DIRECT_DB`, while the flag could not open a database at
all (`P2`). **A test that asserts an API call returned 0 has tested the API, not the
feature.**

So the real metric is not "is the flag mentioned" but "does a test assert the flag's
observable consequence." By that standard, coverage is well below 51%.

## Q3. Branch/function coverage

From `test/coverage/baseline.txt`:

```
line=59.4   branch=40.6   function=78.6
```

**Branch coverage at 40.6% is the number to attack.** Line coverage flatters us: a line
executed once with one outcome of a two-way branch counts as covered. Every defect found
this session — `P1`–`P5`, `U7` — lived in a branch that a test reached but never took the
other way.

## Q4. Regression protection release over release

Currently: `abidiff`, the region-signature gate, the manifest gate, the exec-bit gate,
the scaling-shape gate (nightly, big box), and 24 test tiers. That is strong on
*structural* regressions — ABI, environment compatibility, "did the test run at all."

It is weak on:

1. **Performance.** The only per-PR perf job is `continue-on-error` "informational" — a
   1,041 → 337 tpm regression would be reported by nothing (`G13`). The nightly
   scaling-shape gate helps but covers one workload.
2. **Behaviour drift under flags nobody sets.** See Q2.
3. **Coverage ratchet.** `baseline.txt` exists but nothing fails when coverage *drops*.

## Test wall time

The `Coverage` workflow is **34 minutes in a single job** — the longest thing in CI and
entirely serial. That is the first target for speed: `gcov`/`lcov` parallelises poorly as
written, but the *tiers feeding it* can run concurrently and merge `.gcda` files.

## Concrete proposals, in expected-value order

1. **Behaviour tests for the 112 untested API flags**, asserting observable consequences.
   Prioritise the durability and I/O flags (that is how `P2`/`P3` were found), then
   backup/archive (`DB_ARCH_*`, `DB_BACKUP_*` — data-loss adjacent), then the rest. A
   generated skeleton per flag with an explicit `SKIP: reason` for flags that genuinely
   cannot be asserted keeps the list honest.
2. **A branch-coverage ratchet**: fail CI when branch coverage falls below
   `baseline.txt` minus a tolerance, and require the baseline to be *raised* in the same
   commit that adds a branch. Line coverage should not gate — it is the misleading one.
3. **A known-interacting configure-option pair list**, small and justified.
4. **Parallelise coverage**: run the tiers concurrently, merge `.gcda`, and keep the
   34-minute job as a nightly full run rather than a per-push one.
5. **Make one perf gate real per-PR**: not absolute throughput (too noisy on shared
   runners) but a *shape* assertion on a small fixed workload, like the nightly gate.
