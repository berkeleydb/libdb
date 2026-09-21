# D3 — backpressure at the API boundary: what it is and why it comes first

Explains RFC 0008's D3 proposal, with the claims re-verified in source.

## The gap, stated plainly

libdb has **no backpressure mechanism**. When storage cannot keep up with the append
rate, nothing tells the application. Verified: there is no bound anywhere on how far the
append frontier (`lp->lsn`) may run ahead of the durable frontier (`lp->s_lsn`).

What libdb has *instead* is an accident. In `__log_fill`, when the in-memory log buffer
fills, the code calls `__log_write` **synchronously, while holding the log region
latch** (`src/log/log_put.c`). So the de-facto backpressure is:

> the unlucky thread that happens to fill the buffer performs the write, and **every
> other thread queues behind the region latch until it finishes.**

That is backpressure in the sense that a traffic accident is traffic management. It is
unfair (a random victim pays), invisible (the application sees only latency), and
unbounded (nothing caps the resulting queue).

## What D3 proposes

Follow InnoDB's `log_free_check()` pattern:

1. **Track the lag** between the append frontier and the durable frontier.
2. **Act on it at a defined point** — before a transaction does work, which is InnoDB's
   placement, rather than at commit when the work is already sunk. If the lag exceeds a
   configured bound, either block *outside* the region latch, or return a retryable
   error.
3. **Expose the lag** so saturation is observable rather than inferred from latency.

`DB_LOCK_NOTGRANTED` is the precedent for a retryable "resource exhausted" return that
applications already know how to handle — this would not be a novel contract.

## Why this is the right thing to review first

**It is the only proposal in RFC 0008 with no format break, and most of it needs no new
state at all.** Verified in source: the lag is already derivable from existing public
stat fields —

| field | source | meaning |
|---|---|---|
| `st_cur_file` / `st_cur_offset` | `lp->lsn` (`log_stat.c:92`) | append frontier |
| `st_disk_file` / `st_disk_offset` | `lp->s_lsn` (`log_stat.c:94`) | durable frontier |

So **the observability half is implementable read-only, today, with zero risk**: derive
the lag, report it, add an operator signal. No struct field, so `__env_struct_sig()` is
untouched and every existing environment still attaches. Only the *enforcement* half
(block or return) needs a new flag, and a flag is additive.

Compare that to the alternatives: D1 is disqualified on correctness, D2 needs a region
break, and D0 needs a log-version bump.

**It is a robustness gap, not a throughput lever — and that distinction matters.** The
device currently sits at roughly 5% utilisation (measured: 494k IOPS / 1930 MiB/s
available; libdb peaks near 203k ops/s at t=2). So D3 will not make anything faster
today. It becomes *necessary* precisely when the other work succeeds: every design that
raises the append rate brings the system closer to the point where the absence of
backpressure turns into unbounded queueing and latency cliffs instead of a clean
"slow down" signal.

That is the argument for doing it first rather than last: it is cheap now, it is the
prerequisite for safely landing anything that raises throughput, and it is the only
piece whose value does not depend on winning a performance argument.

## The goal it serves

The stated objective is to saturate the storage subsystem and, when saturated, provide
**natural backpressure upward through the APIs**. Those are two separate problems, and
libdb currently fails the second one *worse* than the first:

- **Saturation** is blocked by the serialization ceiling (~90k txn/s), which is P5/P9
  territory and hard.
- **Backpressure** is simply absent, and is cheap.

Fixing saturation without backpressure produces a system that fails less gracefully than
today's: faster arrival into an unbounded queue. So D3 is not merely first by
convenience — it is first by dependency.

## How to validate it

Not a throughput experiment, which is what makes it easy to get wrong. Throttle the
device (cgroup `io.max`), then show:

- **without D3**: latency grows without bound while the log buffer queues, and the API
  reports nothing;
- **with D3**: the lag stays bounded and the API reports saturation.

The teeth requirement is the usual one here: the test must fail when the bound is
removed, not merely pass when it is present.

## Open question worth settling before implementing

InnoDB checks *before* a transaction does work; libdb's natural hook is
`DB_TXN->commit()`. Checking at commit means the work is already done and the only
options are block or fail-after-work — the latter being poor behaviour for an engine.
Checking at `txn_begin` is better placed but weaker, since a long transaction can still
outrun the bound. Establishing which hook libdb should use, and whether both are needed,
is the first design decision and does not require any code to answer.
