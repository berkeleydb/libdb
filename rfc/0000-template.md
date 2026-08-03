# RFC NNNN: <title>

- **Status:** Draft
- **Type:** Prospective | Normative
- **Author:** <name>
- **Date:** YYYY-MM-DD
- **Tracking:** <phase / issue / PR, optional>
- **Prototype:** `rfc/NNNN/` (optional)

---

## Summary

One paragraph: what this proposes, in plain terms.

## Motivation

Why now? What problem does it solve, for whom? What breaks or stays slow
without it? Cite evidence where possible (benchmarks in `test/bench`, profiles,
bug reports).

## North-star check

Confirm the proposal does not break any of: embedded/no-server operation, ACID,
crash recovery, any access method (B-tree/Hash/Queue/Recno/Heap), multi-process
correctness, on-disk/log/region/ABI format stability. If it *does* touch a
format/ABI, describe the versioned, backward-compatible migration here — this is
the hard review gate.

## Design

The proposal itself. Data structures, algorithms, the seams it touches in the
engine, the config/flags it adds. Enough that someone else could implement it.

## Alternatives considered

What else could solve this, and why this over those.

## Risks & open questions

Correctness risk, performance risk, maintenance cost, what's still unknown.

## Prototype / evidence

If `rfc/NNNN/` has a spike, describe what it validated and the results.

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Accepted | Rejected — YYYY-MM-DD
- **Rationale:** why.
- **Conditions / follow-ups:** any.
