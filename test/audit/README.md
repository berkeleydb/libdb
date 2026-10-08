<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# test/audit — investigation reports

Full reports from investigations whose conclusions are summarised elsewhere
(usually a tracker row in `test/KNOWN-ISSUES.md` or a commit message). They are
kept because a summary records *what* was decided and these record *how it was
measured* — including the attempts that were wrong.

Everything here was recovered from somewhere it would not have survived: the
gitignored `.agent/` directory, or `/tmp` on a host that has since been
terminated. That is the point of the directory. A measured result that lives
only on one machine is one `rm` away from being an unsupported claim, and this
project has already had to retract conclusions that were reasoned about rather
than tested — losing the evidence makes that failure mode permanent.

| file | what it records |
|---|---|
| `AUDIT-REPO-2026-10.md` | repository audit, hard-fork baseline to HEAD: 8,127 files changed, and the finding that 72% of the insertions are committed lcov output, so the fork's authored addition is ~240k lines rather than ~880k |
| `AUDIT-PKG-2026-10.md` | packaging audit: `make install` shipped 0 of 5,245 doc files, the release-tarball mechanism does not work at either point, and the AGPL dual-licence was asserted in exactly one shipped file |
| `FREEBSD-PORT-2026-10.md` | FreeBSD 14.5: the S5 root cause, the `queue.h` defect that meant the kqueue AIO backend had never compiled, ten test runners that assume an in-tree build dir, and the G15 flag measurements |
| `T7-SHQUEUE-UB-2026-10.md` | why `TestQueue` failed: the `shqueue.h` macros were correct and the test was undefined behaviour, which gcc acts on and clang does not |

Reports that are already first-class documents live elsewhere and are not
duplicated here — `test/bench/` for measured performance results (including the
negative ones) and `rfc/` for designs.
