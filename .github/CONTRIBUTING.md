# Contributing to libdb

`libdb` is both a **historical archive** of Berkeley DB and a **living fork**
under active development on `master`. Contributions to the living fork are
welcome.

## Ground rules

- **Target `master`.** The `historical` branch and the version tags
  (`v1.85` … `v5.1.29`, `v4.6.21-SSI`, …) are an immutable archive — do not
  open PRs against them.
- **One logical change per PR**, with a clear description of *what* changed and
  *why*, and how you tested it.
- **Conventional Commits** for messages (`feat:`, `fix:`, `test:`, `docs:`,
  `perf:`, `refactor:`, `chore:`, `ci:`).

## Building and testing

```sh
cd build_unix
../dist/configure --enable-debug --enable-test --with-tcl=/path/to/tcl
make -j

# TCL test suite (from build_unix):
#   tclsh> source ../test/tcl/test.tcl
#   tclsh> run_test <name>      ;# e.g. ssi001, lock001, txn001
```

Notes for engine work:

- The `build_unix` Makefile under-tracks header dependencies. After editing any
  `src/dbinc/*.h` struct, do a **clean rebuild** (`make clean && make`) or you
  will get stale objects with mismatched struct layouts (silent memory
  corruption).
- Generated files (`src/dbinc_auto/*`, `build_*/db.h`, `test/tcl/TESTS`) are
  produced by `dist/s_*` scripts and `db.in` — edit the sources, then
  regenerate. Do not hand-edit generated output.
- New public flags go through `dist/api_flags` + `dist/s_apiflags`.

## Code review

Pull requests are reviewed by maintainers and by **OCR** (Open Code Review), an
automated AI reviewer. OCR posts inline comments; treat them as suggestions, not
gates. Re-run it with a `/open-code-review` comment on the PR.

## CI

Every PR is built across the supported matrix (Linux/macOS/Windows, multiple
compilers and configure options) and runs the test suite. Keep CI green.
