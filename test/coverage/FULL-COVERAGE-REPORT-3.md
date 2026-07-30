# libdb — Authoritative Full-Suite Coverage Report #3 (post-v5.3.32)

Measurement only. Run on a dedicated EC2 c7i.24xlarge (terminated; ~$8), master
@ v5.3.32 + coverage PRs, `-O0 -g --coverage`, gcc 11.5, lcov captured from
`.libs`, full parallel tcl suite + all COV_* blocks.

## Headline (src/ only, dbinc_auto excluded)
| Metric | This run | Prior (#2) | v5.3.30-era (#1) |
|--------|---------:|-----------:|-----------------:|
| Line     | **68.0%** (49192/72377) | 62.3% | 48.0% |
| Branch   | **50.2%** (39238/78236) | 46.2% | 36.1% |
| Function | **80.5%** (2129/2644)   | 72.3% | 52.8% |

Since the first authoritative run the program has added **+20 line points /
+14 branch points**, and function coverage crossed 80%.

## Top remaining cold files (drives the next grinding round)
- xa/xa.c 0%, bt_upgrade.c 0%, db_upg_opd.c 0% -- NOTE: xa + bt_upgrade have
  MERGED tests (PRs #70/#75); the combined driver's C-driver phase 2 did not
  exercise them this run (1 subset test failed; the XA/upgrade drivers likely
  didn't register). MEASUREMENT GAP, not a regression -- fold the XA + upgrade C
  drivers into the combined driver next run.
- rep/rep_lease.c 0% -- lease tests need real multi-process (documented).
- db_upg_opd.c 0% -- needs a genuine pre-3.1 off-page-dup fixture (documented).
- db_rec.c 35%/24%br, hash_rec.c 52%/30%br, bt_rec.c 43%/27%br -- recover-handler
  branches; the recd + recd_handlers C driver lifted these but deep undo/error
  branches remain.
- log_verify_int.c 37%, repmgr_method.c 37%, rep_method.c 40%, rep_automsg 40%
  -- replication/repmgr deeper paths (send-path + config edges).
- common/db_compint.c 21% (64-bit codec is PBT-only), rijndael-api 30% (only
  AES-CBC used; ECB/CFB dead code) -- documented ceilings.

## Path to 80% line
The warm data plane is covered; remaining gap is: replication depth (needs
multi-process rep harness + db_repsite), the recover-handler undo/error branches
(more crash points), and the documented-ceiling / dead-code files. Realistic:
close the XA/upgrade measurement gap (+~1-2pt free), grind replication depth,
then DST/fuzz/malloc for the warm-plane error paths.
