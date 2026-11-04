# E1 findings: __rep_cmp_vote arm D data_gen bypass

## Precondition reachability: CONFIRMED via real VOTE1 messages

Two electable sites can hold EQUAL last-commit LSN but DIFFERENT data_gen.
Reproduced on the dev box (non-diagnostic --enable-test --enable-tcl build),
instrumented engine:

  SETUP A regop=1/16402 gen=2 ; B regop=1/16402 gen=3   (EQUAL LSN, data_gen 2 vs 3)

Real VOTE1 payloads captured at the comparator:
  A: GET_DATAGEN -> 2, lsn=1/16402, tie=1014639157, flags=ELECTABLE
  B: GET_DATAGEN -> 3, lsn=1/16402, tie=2712823913, flags=ELECTABLE

Construction (symmetric fork):
  - M master gen1; A,B clients; replicate 5 txns -> all equal, data_gen 1.
  - M dies. A does one rep_start -master (writes ONE REPLSNHIST gen-2 commit,
    data_gen(A)=2) then -client. SAVE_LSN_HIST gen=2 lsn=1/16118.
  - B's egen pre-bumped by a throwaway 5-site election, then B does one
    rep_start -master (writes ONE REPLSNHIST gen-3 commit, data_gen(B)=3) then
    -client. SAVE_LSN_HIST gen=3 lsn=1/16118.
  Each site adds the IDENTICAL byte sequence (one REPLSNHIST commit + role
  records), so both last-commit (__txn_regop) LSNs land at 1/16402, while the
  REPLSNHIST gen keys differ (2 vs 3).

Facts established by reading the engine:
  - VOTE1 LSN = start LSN of last __txn_regop (rep_elect.c:396 __rep_log_backup
    REP_REC_COMMIT).
  - data_gen = highest gen key in REPLSNHIST (__rep_get_datagen, rep_util.c).
  - On sync, a client memcmp's log bytes at the verify LSN (rep_verify.c:77), so
    electing a divergent site genuinely rolls back the loser's tail -> a durable
    (acked) commit on the rolled-back site is LOST.  No invariant forbids equal
    LSN + different data_gen.

## Arm D behavior (in progress)
  - Counterexample 1 (first-vote sites==1 special case) OBSERVED live: a tally
    with rep->sites==1 adopts the incoming vote unconditionally, bypassing the
    data_gen comparison entirely.
  - To exercise arm D (sites>1, cmp==0) electing the OLDER-data site, the
    older-data site must carry the higher tiebreaker.  Tiebreaker is per-site
    random; controlling it is the next step.

## Arm D bug mechanism PRECISELY CHARACTERISED (dev box, real election)

Observed live at a sites=2 tally (equal LSN 1/16402):
  incoming B dg=3 pri=10  OVER winner A dg=2 pri=100  cmp=0  -> arm B fired (dg 3>2), B won (CORRECT).

So the bug is ORDER- and ROLE-dependent. Arm D elects the WRONG (older-data) site only when:
  - the OLDER-data site is the INCOMING vote,
  - the NEWER-data site is already the running winner,
  - cmp==0 (equal LSN), and the older site has higher priority (or, at equal priority,
    higher tiebreaker).
Then: arm B does NOT fire (older dg !> newer dg), arm C does NOT fire (dg differ), but
arm D fires on priority/tiebreaker and REPLACES the newer winner with the older site.

Consequence: at a site that tallies newer-first then older, rep->winner becomes the OLDER
(stale) site; at a site that tallies older-first then newer, arm B keeps the newer site.
=> two sites that heard the same votes disagree (order dependence, C1's Counterexample 2),
and if a majority tally newer-first-then-older the STALE site is elected and a durable
commit on the newer branch is rolled back (memcmp sync, rep_verify.c:77).

Next: construct a 2- or 3-site election where a MAJORITY tally in the newer-first order so
the stale site actually wins (durability loss), as a deterministic TCL reproducer.
