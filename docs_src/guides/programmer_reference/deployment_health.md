---
title: "Deployment health: what to alarm on"
api-name: "Deployment health: what to alarm on"
---
## Deployment health: what to alarm on

Berkeley DB holds several of its resources in **statically-sized shared
regions**. When one of them is exhausted, the failure does not appear where the
resource was consumed: it appears later, as a bare `ENOMEM` returned by some
unrelated but perfectly legitimate operation — a `DB_ENV->txn_begin()`, a
`DB->put()`, a cursor open. By then the deployment is already broken, and the
statistics that would have explained it describe a state that no longer exists.

This section names the small set of numbers that predict that failure **before**
it happens, where to read each one, what bounds it, and what to do when it
moves. Every number below is available through the existing statistics APIs and
printed by <a href="../../api/c/db_stat.md" class="olink">db_stat</a>; none of
them requires a special build, a debug library, or a restart to observe.

### The one number per resource: utilization

For each statically-sized resource, the signal is a ratio:

    utilization = in-use / configured-maximum

Reading it requires that a maximum actually be **configured**. If you have not
set one, the resource grows on demand until the region itself is exhausted,
there is no denominator, and the health lines say so explicitly
(`no maximum configured`) rather than printing a misleading `0%`. **Configuring
the maxima is therefore step one of making a deployment alarmable**, not merely
a tuning nicety:

| Resource | Configure with |
| --- | --- |
| Lockers | <a href="../../api/c/envset_lk_max_lockers.md" class="olink">DB_ENV-&gt;set_lk_max_lockers()</a> |
| Lock objects | <a href="../../api/c/envset_lk_max_objects.md" class="olink">DB_ENV-&gt;set_lk_max_objects()</a> |
| Locks | <a href="../../api/c/envset_lk_max_locks.md" class="olink">DB_ENV-&gt;set_lk_max_locks()</a> |
| Transactions | <a href="../../api/c/envset_tx_max.md" class="olink">DB_ENV-&gt;set_tx_max()</a> |
| Mutexes | <a href="../../api/c/mutexset_max.md" class="olink">DB_ENV-&gt;mutex_set_max()</a> |

For sizing guidance see
<a href="lock_max.md" class="xref" title="Configuring locking: sizing the system">Configuring locking: sizing the system</a>
and <a href="env_size.md" class="xref" title="Sizing a database environment">Sizing a database environment</a>.

### The signals

Each row gives the programmatic source (stable, part of the statistics API), the
human-readable `db_stat` line, and the bound that makes the number meaningful.

<span class="term">Locker slots</span>
`DB_LOCK_STAT.st_nlockers` / `st_maxlockers`, from
<a href="../../api/c/lockstat.md" class="olink">DB_ENV-&gt;lock_stat()</a>.
`db_stat -c`: **`Locker slots in use`**. One locker per active transaction, per
non-transactional cursor, and per open database handle — plus, under
serializable snapshot isolation, per *committed* reader whose SIREAD markers
have not yet been garbage-collected (see below). Exhaustion returns `ENOMEM`
from the next operation that needs a locker.

<span class="term">Lock object slots</span>
`DB_LOCK_STAT.st_nobjects` / `st_maxobjects`. `db_stat -c`:
**`Lock object slots in use`**. One per distinct page (or record, for Queue)
under lock.

<span class="term">Lock slots</span>
`DB_LOCK_STAT.st_nlocks` / `st_maxlocks`. `db_stat -c`:
**`Lock slots in use`**.

<span class="term">Mutex slots</span>
`DB_MUTEX_STAT.st_mutex_inuse` / `st_mutex_max`, from
<a href="../../api/c/mutexstat.md" class="olink">DB_ENV-&gt;mutex_stat()</a>.
`db_stat -x`: **`Mutex slots in use`**. This is the resource most likely to be
exhausted *first* by the retention effects described below, because every
retained locker and every retained transaction detail holds one. Note the
denominator: `st_mutex_max` is the ceiling the region will grow to, whereas
`st_mutex_cnt` is merely what has been allocated so far — utilization against
`st_mutex_cnt` will look alarming while being perfectly healthy.

<span class="term">Active transaction slots</span>
`DB_TXN_STAT.st_nactive` / `st_maxtxns`, from
<a href="../../api/c/txnstat.md" class="olink">DB_ENV-&gt;txn_stat()</a>.
`db_stat -t`: **`Active transaction slots in use`**.

### The leading indicators: snapshot and SSI retention

The two signals below are the ones worth watching most closely, because they
move **before** the utilization ratios do, and because they are the ones that
grow as a function of *application behaviour* rather than of concurrency.

Under snapshot isolation a committed transaction's resources cannot always be
released at commit time:

- A committed transaction that **wrote** leaves MVCC versions in the cache.
  Its `TXN_DETAIL` (and the mutex it holds) must be retained until those
  versions become obsolete — that is, until no active reader's snapshot can
  still see them.
- A committed **serializable** (SSI) reader leaves SIREAD markers on the objects
  it read, which are needed for conflict detection against transactions still
  running. Its locker, its mutex, and its `TXN_DETAIL` are all retained until
  the last marker is reclaimed.

Both populations are reclaimed, and both are **bounded** — but the healthy
steady state is a **sawtooth**, not a flat line. Reclamation is triggered
(at checkpoint, and for SIREAD markers when the live marker count passes a
fraction of the lock-object table), so the population climbs, is swept, and
climbs again. **A sawtooth is health. A staircase is the problem.**

<span class="term">Retained snapshot transaction details</span>
`DB_TXN_STAT.st_nsnapshot`, with `st_maxnsnapshot` as the high-water mark.
`db_stat -t`: **`Snapshot txn details retained (MVCC/SSI)`**. This counts
transaction details parked awaiting reclamation, from either cause above. It is
reported against `st_maxtxns` because it consumes the same transaction-region
and mutex-region slots that active transactions do. **A value that rises
monotonically across checkpoints is the signature to act on**; a value that
oscillates within a band is the expected shape.

<span class="term">Live SSI SIREAD markers</span>
`db_stat -c`: **`SSI committed-reader SIREAD markers live`**. Reported against
its sweep threshold rather than a configured maximum, because that threshold —
a fraction of the allocated lock objects — *is* its bound. This is a
deliberately approximate count maintained as a garbage-collection hint, so read
it as a trend, not an exact population. Programmatically, track `st_nlockers`
and `st_nsnapshot`, which move with it and are exact.

### Recommended alarms and remedies

Sample every scrape interval (a minute is ample; these are cheap reads).
Alarm on **sustained** values, not on single samples: the sawtooth means an
instantaneous reading near a peak is normal.

| Alarm | Condition | What it means | Action |
| --- | --- | --- | --- |
| Warning | any utilization &gt; 0.8 sustained over several intervals | Headroom is nearly gone; the next burst may fail | Raise the corresponding maximum. If it is lockers or mutexes, first check for a long-lived snapshot transaction (below) |
| Critical | any utilization &gt; 0.95, or trending up across intervals | Exhaustion is imminent; `ENOMEM` is close | Raise the maximum now; plan a restart if the region cannot be grown in place |
| Critical | `st_nsnapshot` rises monotonically across three or more checkpoints | Retained details are not being reclaimed: almost always one long-lived snapshot or SSI transaction holding back the oldest-reader frontier | Find and end the long-lived transaction (see below). Checkpoint more often as mitigation, not as a fix |
| Warning | SIREAD markers sustained near or above 100% of the sweep threshold | The marker sweep is not keeping up with the SSI read rate | Increase the lock-object allocation (which raises the sweep threshold proportionally), or reduce the rate of serializable read-only transactions |
| Warning | `st_maxnlockers` / `st_maxnsnapshot` (high-water marks) approach the configured maxima even while current values look fine | A past burst nearly exhausted the region | Size for the burst, not the average |

**Finding the long-lived transaction.** This is the single most common root
cause behind retention alarms, because *one* forgotten open snapshot
transaction pins every version and marker newer than its snapshot:

    db_stat -h <home> -t

Read the `Active transactions:` list. A transaction whose `begin LSN` is far
behind the current log position, or one carrying a large `mvcc refcount`, is the
culprit. Long-running read-only reporting queries under
`DB_TXN_SNAPSHOT` or `DB_TXN_SERIALIZABLE` are the usual source. The remedy is
in the application: commit it, or break it into shorter transactions. Note that
raising the maxima only buys time against this cause — the retention is
proportional to how long the transaction stays open.

**A note on `DB_STAT_CLEAR`.** Reading these values with `DB_STAT_CLEAR` resets
the high-water marks (`st_maxnlockers`, `st_maxnsnapshot`,
`st_mutex_inuse_max`) that several of the alarms above depend on. Monitoring
should read **without** `DB_STAT_CLEAR` so that it does not destroy the history
it is meant to be watching, and so that two independent monitors do not blind
each other. The current-value counters are unaffected either way.

### Cost

These are counters the subsystems already maintain; the utilization ratios are
derived when statistics are read, not accumulated on any operation path.
Reading them is a region-lock-protected copy, the same cost as any other
statistics call, and it does not perturb what it measures. Scraping once a
minute is free at any realistic scale.
