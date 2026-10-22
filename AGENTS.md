# libdb: project instructions for agents

libdb is the community continuation of Berkeley DB. It is an embedded
transactional key/value store with no server, multi-process shared regions and
a small footprint. Keep that identity in every change.

## Release lines (read before any merge, backport, version bump or tag)

There are two lines. Every change belongs to exactly one of them first.

- **`master`**: all development. Regular releases `vYYYY.0M[.MICRO]` are cut
  from here.
- **`lts/<base>`** (now `lts/2026.10.7`): one LTS line. It takes **backports
  only** and ships patch releases `v<base>.N` (`v2026.10.7.1`, …). Its formats,
  soname, ABI and region signature are frozen.

The procedure for each of these is in [`RELEASING.md`](RELEASING.md): branches,
backport eligibility, the `-x` cherry-pick rule, version numbers and the
release checklist. Follow it step by step. The version scheme and the LTS
promise are in [`VERSIONING.md`](VERSIONING.md), and the gates are in
[`test/audit/QUALIFICATION-MATRIX.md`](test/audit/QUALIFICATION-MATRIX.md).

The rules agents get wrong most:

- A fix lands on `master` first, then is cherry-picked with `-x` onto
  `lts/<base>` through a `backport/<base>/<topic>` PR, merged with a merge
  commit.
- Never merge `master` into `lts/*`, and never put features, performance work,
  `REGION-SIG-CHANGE-INTENDED` or `ABI-BREAK-INTENDED` on `lts/*`.
- An LTS patch version has four parts (`2026.10.7.1`). The next `master`
  release that month is `2026.10.8`. Do not give an LTS fix a three-part number.
- Pushing a tag or a release, merging to `master` or `lts/*`, and marking a PR
  ready for review are maintainer actions. Do them only when the user asks.

## Where work runs

- Build, test and benchmark on EC2 (AWS profile `kiwi`, FreeBSD or Debian).
  Benchmarks run on metal. The local machine is for reading source, writing
  docs, and git/gh.
- Temporary worktrees live in `/scratch/libdb/<name>`.

## Repo conventions

- `docs/` is generated from `docs_src/`. Design notes go in `rfc/` or `test/`.
  Evidence lives in `test/audit/` and `test/bench/`. `.agent/` and `.agents/`
  are gitignored, so anything moved there is deleted from the repo.
- Generated files (`src/dbinc_auto/*`, `build_windows/db.h`,
  `build_android/db.h`, `dist/configure`) come from `dist/s_*`. Regenerate
  them; do not hand-edit.
- Message ids: scan both `DB_STR(` and `DB_STR_A(` across `src`, `lang` and
  `util` before you allocate one.
- A new `test/db/run_*.sh` needs a `test/MANIFEST` entry and an executable git
  mode (`git update-index --chmod=+x`).

See .agent-steering-domains.md for domain-specific steering (local).
