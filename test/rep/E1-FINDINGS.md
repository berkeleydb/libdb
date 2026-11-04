# E1: `__rep_cmp_vote` is not a total order (election winner selection)

Workstream E1, 2026-11.  Branch `fix/rep-elect-datagen` (master) +
`backport/2026.10.7/rep-elect-datagen` (LTS).  Reviews: PR #263 (CBMC H1
finding), lead's "Counterexample 2" note.

## The defect

`__rep_cmp_vote` (src/rep/rep_elect.c) picks the phase-1 election winner.  It
was not a consistent order, with two defects:

1. **First-vote special case.** When `rep->sites == 1` the first electable vote
   became the winner outright, so the winner depended on which vote a site
   tallied first (CBMC "Counterexample 1").

2. **data_gen bypass (the durable one).** The last arm,
   `cmp == 0 && (priority > w_priority || (priority == w_priority &&
   tiebreaker > w_tiebreaker))`, fired whenever the LSNs were equal
   **regardless of data_gen**.  So a vote with an OLDER data_gen but an equal
   LSN and a higher priority or tiebreaker could beat a vote with a NEWER
   data_gen that arm B had made the winner (CBMC "Counterexample 2").

`data_gen` is the generation of a site's most recent data.  Electing a site
with an older data_gen rolls back the newer site's tail on sync -- and because
a client compares the actual **log bytes** at its verify LSN
(`__rep_verify`, src/rep/rep_verify.c:77, `memcmp(mylog.data, rec->data, ...)`),
a wrong winner genuinely truncates the loser's log.  A commit the loser had
made durable is lost.  That is a durability bug, not just a failure to settle.

## What is PROVEN

- **Comparator order dependence / non-total order.**  CBMC harness
  `test/cbmc/harness_cmpvote.c` property (a) fails on the old code; the
  self-contained C gate `test/db/run_rep_cmpvote_order.sh` fails on the old
  code (`-DCMPVOTE_OLD`) for the captured vectors (2-site: arrival order flips
  the winner between the newer and the stale site).

- **The precondition is reachable on real sites.**  Measured against the live
  engine on the dev box (Debian 12, non-diagnostic `--enable-test
  --enable-tcl`): two/three electable sites with **EQUAL last-commit LSN**
  (1/16402) and **DIFFERENT data_gen** (A=2, B=C=3), carrying those values in
  real VOTE1 messages (confirmed via the engine's own `rep_stat` /
  `__rep_get_datagen` and instrumented `__rep_cmp_vote` traces, since removed).

  Why it is reachable: a VOTE1 carries (a) the LSN of the site's last
  `__txn_regop` (`__rep_log_backup(REP_REC_COMMIT)`, rep_elect.c:396) and
  (b) the highest gen key in its REPLSNHIST database (`__rep_get_datagen`,
  rep_util.c).  These are independent: the last-commit LSN is a byte offset,
  the data_gen is a generation counter.  A gen change writes one REPLSNHIST
  commit of a fixed shape, so two sites that each took one mastership cycle
  advance their last-commit LSN by the same amount while recording different
  gen keys -- equal LSN, different data_gen.  No invariant forbids it.

## What is ARGUED (not mechanically proven)

- **That a real network delivers the stale VOTE1 last at a majority.**  The
  stale (lower-data_gen) site is actually ELECTED only when its VOTE1 is
  tallied LAST at a majority of sites: arm B (higher data_gen wins) never gets
  to displace it, and arm D then displaces the newer winner on priority or
  tiebreaker.  VOTE1 arrival order in repmgr is arbitrary network timing, so
  "the stale vote arrives last" is an ordinary, non-zero-probability outcome;
  combined with the stale site having a higher priority (a designated primary
  often does) or simply a higher random tiebreaker (~50%), the stale site wins.
  We did not construct a fully deterministic end-to-end TCL election that
  settles on the stale site (see below); the claim rests on the proven
  precondition plus this delivery-order argument.

- **Disposition.**  A comparator that is not a total order is a defect whether
  or not a full election history reproduces it deterministically, and its
  precondition is reachable.  Fixed rather than left as liveness-only.

## The fix

`__rep_cmp_vote` rewritten as a single TOTAL ORDER applied uniformly to every
vote including the first (keyed on `rep->sites == 1`, since `rep->winner` is
not reset between elections -- `__rep_elect_done` resets `sites`/`votes`):

  1. electable beats not-electable;
  2. non-zero priority beats zero priority (mixed-version ELECTABLE preserved);
  3. higher data_gen wins;
  4. higher LSN wins;
  5. higher priority value wins;
  6. higher tiebreaker wins;
  7. higher eid wins -- the final discriminator, so each site's order is
     total.  eids are local to each site, so this does not make different
     sites agree on a full tie; the random 32-bit tiebreaker does, except on
     an exact collision, which costs at most an extra election round.

## Mixed-version (rolling-upgrade) compatibility

An election is decided by each site's OWN comparator: a site becomes master
only via `I_HAVE_WON` (its own `rep->winner` AND a majority of VOTE2s), and each
site votes VOTE2 for its own phase-1 winner.  Old and new comparators differ
ONLY in the cases where the old code was order-dependent -- the all-tied
first-vote case and the equal-LSN/different-data_gen case.  In every other case
(distinct data_gen, or same data_gen with distinct LSN) both versions pick the
same winner, so a mixed group behaves exactly as before.

In the divergence window a new site picks the higher-data_gen site while an old
site may pick a different one depending on arrival order.  Consequence:

- **No split brain.**  Two distinct candidates cannot each gather a majority of
  VOTE2s, because any two majorities of the same group intersect (H3,
  ELECTION_MAJORITY: `2*nvotes > nsites`).  This fix does not touch that.
- **Possible transient stall.**  If old and new sites split their VOTE2s in the
  divergence window, no candidate reaches a majority and the election retries
  at the next egen.  egen retries reshuffle VOTE1 arrival timing, and once a
  new-version majority exists it deterministically elects the max-data_gen
  site, so the stall is transient and self-healing -- the same liveness cost
  the old order-dependence already had, now bounded to the upgrade window.

**Upgrade note for users:** during a rolling upgrade, in the rare event that an
election has two candidates with the SAME last-commit LSN but DIFFERENT data
generations, old and new members may need an extra election round to agree on
the winner; the group stays safe (never two masters) and settles on the member
with the most recent data once a majority runs the new code.  Finish the
rolling upgrade promptly, as usual.

The fix changes WHICH site wins only in cases where the old code was
order-dependent (its winner there was not well-defined); in every
well-defined case the winner is unchanged.

## Why no deterministic end-to-end TCL gate

rep200.tcl (advisory, in this branch) reproduces the precondition against the
real engine every run (equal LSN, different data_gen, real VOTE1s) but cannot
deterministically force the stale site to WIN: making the data_gens differ
needs an egen pre-bump, which desynchronises the asynchronous election's egen
so the controlled-delivery election does not settle on demand; a construction
that keeps egens synced yields equal data_gens.  The two requirements fight in
the test harness.  The deterministic gate is therefore at the comparator level
(test/db/run_rep_cmpvote_order.sh + the CBMC harness), driving the REAL function
over the captured real vote vectors.  rep200.tcl is kept as an advisory
precondition demonstration; its header says so.
