# Releasing: branches, backports and release lines

How libdb ships two kinds of release at once:

- **Patch releases** of the current LTS line. These are fixes only.
- **Regular releases** from `master`. These carry new work.

[`VERSIONING.md`](VERSIONING.md) defines the version numbers and the LTS
promise. [`test/audit/QUALIFICATION-MATRIX.md`](test/audit/QUALIFICATION-MATRIX.md)
defines the gates a release must pass. This file is the procedure that uses
both.

The model is **trunk-based development with long-lived maintenance branches**,
the same shape CPython, PostgreSQL, the Linux stable kernels and Node.js use:

- every change lands on `master` first;
- a fix that an LTS line needs is **backported**, meaning cherry-picked from
  `master` onto that line's maintenance branch;
- releases are tags cut from either branch.

## Branches

| Branch | Holds | Releases cut from it |
|---|---|---|
| `master` | all development | regular releases `vYYYY.0M[.MICRO]` |
| `lts/<base>` (`lts/2026.10.7`) | one LTS line: its base tag plus backports | LTS patch releases `v<base>.N` |
| `backport/<base>/<topic>` | one backport in flight | none (merged by PR into `lts/<base>`) |
| `historical`, `vendor`, `gh-pages`, `archive/*` | archives | none |

`lts/<base>` is created **at** the LTS tag (`git branch lts/2026.10.7
v2026.10.7`). It is long-lived and is never merged back into `master`.
Backports flow one way: `master` → `lts/*`.

## Release lines and version numbers

| Line | Tag form | Example | Contents |
|---|---|---|---|
| `master` regular releases | `vYYYY.0M[.MICRO]` | `v2026.10.8`, `v2026.11` | new work |
| LTS patch release | `v<base>.N` | `v2026.10.7.1`, `v2026.10.7.2` | the LTS base plus fixes |

The fourth component is what separates an LTS patch from a regular release, so
the two lines never compete for the same number. `v2026.10.7.1` is "the
2026.10.7 LTS, first patch". A `master` release cut in the same month is
`v2026.10.8`. Without the fourth component, the LTS fix and the next regular
release would both claim `v2026.10.8`. `DB_CALVER` in `dist/RELEASE` carries the
same string (`2026.10.7.1`), and so does `db_version()`.

An LTS patch release never moves the compatibility triplet, the soname, the
public ABI, the on-disk or log format, or the region signature. Those are
frozen for the line (`VERSIONING.md`, "LTS lines").

## What may be backported

A change is eligible for an LTS line when **all** of these hold:

1. It is a bug fix, a security fix, or a portability fix. That includes the test
   that proves it and any CI or docs needed to keep the line building and
   correct. It is not a feature, a performance change, or a refactor.
2. It changes no frozen interface. The CI gates prove this: `region-sig`, the
   ABI gate, `run_handle_sizes` and the header-regen drift gate. None of the
   escape tokens (`REGION-SIG-CHANGE-INTENDED`, `ABI-BREAK-INTENDED`) may appear
   on an `lts/*` branch.
3. It has already landed on `master`. The exception is a fix for code that
   `master` no longer has. That fix goes straight to the LTS branch, and its PR
   says why.

Label the master PR **`backport-lts`** when you open it, so the decision is
made while the change is fresh. Remove the label if review finds it ineligible.

## Doing a backport

1. Branch from the LTS branch, not from `master`:
   `git switch -c backport/2026.10.7/<topic> origin/lts/2026.10.7`.
2. Cherry-pick the **original** commits from `master`, oldest first, with `-x`.
   The `(cherry picked from commit …)` trailer is the audit trail: it maps every
   LTS commit to its `master` original. Pick the individual commits, not the
   merge commit.
   `env GIT_EDITOR=true git cherry-pick -x <sha>...`
3. Resolve conflicts against the LTS code as it is. Do not drag in unrelated
   `master` changes to make a pick apply; if a fix needs a prerequisite, the
   prerequisite must pass the eligibility rules too. A resolved pick keeps its
   `-x` trailer. Say what you resolved in the PR.
4. Open a PR with base `lts/2026.10.7` and the title prefix `[2026.10.7]`.
   Link the master PR. CI runs on any base branch.
5. Merge with a **merge commit**. A squash merge would lose the per-commit `-x`
   trailers.

To list what `master` has that an LTS line does not, use
`git cherry -v origin/lts/2026.10.7 origin/master`. It compares patch content,
so a backported commit shows as present even though its hash differs.

## Cutting a release

The same steps apply to both lines. Only the branch, the version and the gate
set differ.

1. **Pick the version.** For `master`: the next `vYYYY.0M[.MICRO]` for this
   month. For an LTS line: `v<base>.N`, one more than the line's last patch.
   Check that the tag does not already exist (`git tag -l`).
2. **Make a release commit** on a `release/<version>` branch cut from the line's
   branch. It touches only the version sites below, the upgrade note and the
   release notes:
   - `dist/RELEASE`: `DB_CALVER` and `DB_RELEASE_DATE`;
   - regenerate: `cd dist && sh ./s_config && sh ./s_include && sh ./s_windows
     && sh ./s_android`. Keep only the version lines in the `build_android/`
     output and revert unrelated churn;
   - by hand: `meson.build`, `dist/meson.build`, `dist/meson/db_subs.json`;
   - `docs_src/guides/upgrading/upgrade_<v>.md`, plus its entry in that
     directory's `_meta.toml`;
   - `docs_src/_data/releases.toml`: a one-line note.
3. **Qualify** against the gates in `QUALIFICATION-MATRIX.md`. A regular release
   runs gates 1–7. An LTS patch runs gates 1–5 and 7, plus these LTS checks:
   - the region signature equals the line's frozen value on every platform;
   - the default library's `.text` and the handle sizes equal the base tag's,
     apart from the fixed code;
   - every fixed bug has a test that FAILs on the base tag and PASSes on the
     release.

   Gate 6, the cross-release metal benchmark, is skipped for an LTS patch unless
   a fix touches a hot path. Gate 8, the soak, is skipped unless a fix touches
   resource lifetime. Record which gates ran and why any were skipped.
4. **Merge the release PR**, then tag the merge commit with an annotated tag,
   and push it:
   `env GIT_EDITOR=true git tag -a v2026.10.7.1 -F <notes-file>` then
   `git push origin v2026.10.7.1`.
5. **Publish** a GitHub release with that tag and the release notes. A regular
   release on `master` is marked *latest*. An LTS patch is marked *latest* only
   if it is newer than every regular release; otherwise publish it with
   `--latest=false`, so the newest `master` release stays the default download.
6. **Verify** from a fresh clone of the **published** tag (gate 7): it builds,
   `db_version()` prints the new `DB_CALVER`, and the region signature is the
   expected one.
7. **Bring the release records forward to `master`.** An LTS release changes
   `dist/RELEASE` only on its own branch; `master` keeps its own version. Do
   carry forward to `master` the release notes, the `releases.toml` line and
   the upgrade note, so the docs site lists every release. Do this in a small
   PR.

## Release notes

Write them for the person upgrading. For each fixed bug, say:

- what went wrong, in the user's terms;
- which releases have it;
- what to do, if anything.

Name the PR and the test that proves the fix. An LTS patch release's notes also
state that nothing frozen changed, and back that with the measured signature.

## Support window

An LTS line receives backports until the next LTS is declared, plus 12 months
(`VERSIONING.md`). When a line ends, its `lts/*` branch stays as an archive. No
further tags are cut from it.
