# libdb RFCs

Design proposals for libdb. An RFC is how a non-trivial change — a new access
method, an on-disk/format change, a performance subsystem, a durability model —
gets written down, reviewed, and accepted or rejected **before** large
implementation effort, so the reasoning survives and the decision is explicit.

User-facing documentation lives in [`docs_src/`](../docs_src) (published to
libdb.org). This directory is the opposite: internal design intent and the
record of what we decided and why.

## Layout

- `NNNN-short-title.md` — the RFC itself (4-digit zero-padded number).
- `NNNN/` — optional per-RFC working directory: prototypes, spikes, data,
  scratch code that validates the idea before it's wired into the engine.
- `0000-template.md` — copy this to start a new RFC.
- `INDEX.md` — the register: every RFC, its status, one line.

## RFC lifecycle / status

Each RFC carries a `Status:` in its header. The allowed states and the
transitions:

| Status | Meaning |
|--------|---------|
| **Draft** | Under active writing/discussion. Not yet decided. |
| **Accepted** | Reviewed and approved; implementation may proceed. The RFC is the spec. |
| **Rejected** | Reviewed and declined. Kept for the record (why we said no). |
| **Superseded** | Replaced by a later RFC (name it). |
| **Implemented** | Accepted **and** shipped; links to the code/PRs. |

`Type:` is `Prospective` (a future direction) or `Normative` (binds current
behavior/format).

## Review methodology (explicit accept/reject)

1. **Open**: copy `0000-template.md` to the next free `NNNN-title.md`, fill
   Summary/Motivation/Design/Alternatives/Risks, set `Status: Draft`, add a row
   to `INDEX.md`. Optionally start `NNNN/` with a prototype.
2. **Review**: the RFC is judged against libdb's north star — a change is
   **rejected outright** if it breaks any of: embedded/no-server operation, ACID
   guarantees, crash recovery, any access method, multi-process correctness, or
   on-disk/log/region/ABI format stability, *unless* the RFC explicitly argues a
   versioned, backward-compatible migration. Beyond that gate, review weighs:
   correctness risk, performance evidence (measured, not asserted — cite the
   `test/bench` microbenchmarks or a reproducible harness), maintenance cost, and
   scope.
3. **Decide**: the maintainer records the decision **in the RFC** — flip
   `Status:` to `Accepted` or `Rejected` with a dated **Decision** section
   stating the rationale and any conditions. A `Rejected` RFC is never deleted;
   the "no" and its reasons are the value.
4. **Implement**: an `Accepted` RFC drives the work. When shipped, flip to
   `Implemented` and link the PRs/commits. Material deviations from an accepted
   RFC require an amendment (a dated note) or a superseding RFC.

Small, obvious, or purely-internal changes do not need an RFC — this is for
decisions worth remembering.
