<!-- PRs target `master` (the living fork), never `historical` or version tags. -->

## What & why

<!-- What does this change do, and why? Link any issue. -->

## How tested

<!-- Configure flags, platforms, and which tests you ran (e.g. ssi001, run_std).
     Note: after editing any src/dbinc/*.h struct, do a clean rebuild. -->

## Checklist

- [ ] Targets `master`
- [ ] Builds clean (`make clean && make`) on at least one platform
- [ ] Relevant tests pass; new behavior has a test
- [ ] Generated files regenerated via `dist/s_*` (not hand-edited)
- [ ] On-disk / log format and public `db.h` ABI preserved (or change justified)
