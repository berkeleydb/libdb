# `lab/` — design prototypes

Self-contained prototypes that validate control logic before it is wired into
the engine. Not built by the library; each has its own Makefile.

## `lab/lsm` — unified adaptive LSM controller

Prototype of the two-axis adaptive controller from
[`docs/design/lsm.md`](../../docs/design/lsm.md): one rolling-window + cooldown
core driving both the **structure axis** (SINGLE ⇄ HYBRID ⇄ MULTILEVEL, aether-
style) and the **per-segment policy axis** (LEVELED ⇄ TIERED, Amethyst-style).

Time is a caller-supplied "tick" so the logic is deterministic and testable.

```sh
cd lab/lsm && make check
```

`test_adaptive.c` drives an Amethyst-style phase-shifting workload and asserts:
- a write-heavy phase spawns the structure to `MULTILEVEL`; an idle phase
  collapses it back to `SINGLE`, with no oscillation;
- a sustained load reaches `MULTILEVEL` and stays (cooldown anti-flap);
- write-hot segments converge to `TIERED`, read-hot to `LEVELED`, and balanced
  segments don't churn.

Next: replace the tick with a millisecond clock, feed real op counters from the
access-method layer, and have the policy axis emit segment-rewrite decisions to
the compactor / log cleaner (ROADMAP #9, #14).
