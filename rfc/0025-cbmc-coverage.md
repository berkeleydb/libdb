# RFC 0025: Expand CBMC coverage

Status: Draft · Owner: lead · Scope: `test/cbmc/`, its CI workflow, and engine fixes for whatever the harnesses find

## 1. Where we are

`test/cbmc` has six harnesses over small, self-contained functions: varint,
byte swapping, `__ham_func4`, `__db_getlong`, the deadlock detector's
`__dd_find`, and `__db_ret_okitem`. The okitem harness found a real
out-of-bounds read (`HPAGE_PTYPE` was not parenthesised), and the fix landed in
`2ca81268a`. Two problems:

- **The CI workflow was never enabled.** It sits in `test/cbmc` as
  `cbmc.yml.workflow` and has never run.
- **`run.sh` still expects the fixed bug.** Its `okitem(REAL)` arm passes only
  if the real code *fails*. Its harness carries a private, unparenthesised copy
  of the macro, so it keeps "reproducing" a bug that master no longer has.

## 2. What CBMC is good for here

CBMC proves properties of one C function over **every** input up to a bound:
no out-of-bounds access, no invalid pointer, no overflow, and any assertion we
write. It fits code that is:

- **pure or nearly pure**: its result depends on its arguments and a few
  fields, not on I/O, locks or a shared region;
- **bounded**: loops have a size we can state (a page of ≤ 64 bytes, ≤ 5 sites);
- **safety-relevant**: a wrong answer loses data, elects two masters, or reads
  past a buffer on hostile input.

Network protocols (who receives which message when) are libcmc's job, not
CBMC's (see `docs/WHY.md` in libcmc). CBMC checks the **decision each node
makes**, given its inputs. libcmc checks the **protocol** those decisions form.

## 3. Candidates, ranked

| # | Target | Property to prove | Why it matters | Bound |
|---|---|---|---|---|
| **H1** | `__rep_cmp_vote` (rep_elect.c) | **Election winner selection.** It is a strict total order on votes, so the result does not depend on vote arrival order; a site with priority 0 that is not electable never wins; a non-zero priority beats a zero one; ties fall to data_gen, then LSN, then priority, then tiebreaker | Two sites tallying the same votes in different orders must choose the same winner, or the group can elect two masters | 3–4 votes, all field values |
| **H2** | `__rep_tally` (rep_elect.c) | Each site is counted once per election; a vote from an older egen never replaces a newer one; the count never exceeds the number of distinct voters, and stays inside the tally array | A double-counted vote wins an election without a majority | tally of 5, all sequences of 6 votes |
| **H3** | election quorum: `ELECTION_MAJORITY` and the `nvotes`/`nsites` checks in `__rep_elect_int` | Any two majorities of the same `nsites` intersect; the sub-majority warning fires exactly when `ack <= nsites/2` | The safety argument for one master per generation rests on majority intersection | nsites 1–64 |
| **H4** | ack-policy decision: `got_acks` and the `needed`/`quorum` computation in `__repmgr_send` (repmgr_net.c) | **Durability under QUORUM.** If a commit is reported durable, every possible election majority of electable sites contains a site that has the commit. Also: the "skip waiting" shortcut never reports durable when `got_acks` would not | This is the property the fault-injection suite's S5 tests from outside. CBMC proves it for every group shape, including the 2-site special case and the GMDB-update paths | ≤ 5 sites, all ack/peer/membership combinations |
| **B1** | `__bam_defdecompress` (bt_compress.c) | Never reads past `compressed->size` and never writes past `destKey->ulen`/`destData->ulen`, for **any** input bytes | The decoder reads each varint before checking it has the bytes, so a truncated compressed item can over-read by up to 8 bytes. A compressed btree's pages are a trust boundary (`db_verify`, a corrupted file) | compressed ≤ 16 bytes |
| **B2** | `__bam_defcompress` + `__bam_defdecompress` | Round trip: decompress(compress(k, d)) == (k, d), given the same previous key/data | A compression bug silently corrupts every key | keys ≤ 6 bytes |
| **Q1** | queue record arithmetic: `QAM_RECNO_PAGE`, `QAM_RECNO_INDEX`, `QAM_OUTSIDE_QUEUE`, `QAM_BEFORE_FIRST`, `QAM_AFTER_CURRENT` (qam.h) | Every record number in the queue maps to a valid (page, index); the index is < records per page; the wrap-around predicates agree with a simple reference over the whole 32-bit range, including when the queue wraps | Queue wrap-around has had bugs before; these macros are arithmetic over `u_int32_t` with wrap | all `u_int32_t`, loop-free |
| **L1** | `__logc_hdrchk` (log_get.c) | A header that passes is internally consistent (`len > size`, `len <= bp_maxrec` when set) | Recovery trusts what this accepts | loop-free |
| **E1** | `__env_alloc_size` and `SET_QUEUE_FOR_SIZE` (env_alloc.c) | The size-class index stays in range for every length; the rounded size covers the request without overflow | A bad size class corrupts the region allocator for every process | all `size_t` |

Not chosen: SHA-1, HMAC and AES (well-tested reference code, and CBMC cost is
high), and the hash and btree page splitters (too stateful for CBMC; the
fuzzer and DST cover them).

## 4. Method

- **Run the real code.** Pull the function in with `#include` of its `.c`
  where possible. Where it is `static` or drags in the whole tree, copy it
  **verbatim** and add a `diff` check in `run.sh` that fails when the copy
  drifts from `src/`. The okitem harness drifted silently; that check would
  have caught it.
- **Every harness has teeth.** Each one has a `-D` sabotage build that plants
  a real bug shape (swap two comparisons, drop a bound check, an off-by-one in
  the majority) and must FAIL. `run.sh` checks both directions.
- **State the bound** in the harness's header and in the README table. A
  bounded proof is not a claim about unbounded inputs.
- **CI:** enable the workflow, and make it required once it is green and
  stable. The suite's runtime budget is under 5 minutes.

## 5. Phases

| Phase | Work | Gate |
|---|---|---|
| C0 | Enable the CI workflow. Fix `okitem(REAL)`: drop the stale private macro, test the real header, and make the arm a sabotage arm. Add the verbatim-copy drift check | CI green, and the drift check fails on a planted change |
| C1 | H1–H4, the election and ack-policy decisions | Each verifies, with a sabotage arm that fails |
| C2 | B1, B2, Q1 | Same. A real counterexample becomes a bug fix on its own branch with a failing-first test, and a backport if LTS has it |
| C3 | L1, E1 | Same |
