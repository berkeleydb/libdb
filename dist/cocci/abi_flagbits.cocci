// abi_flagbits.cocci -- inventory of flag-bit #defines as an ABI early warning.
//
// Every public flag bit (DB_*, and the internal *_ flags in src/dbinc/*.h) is
// part of the on-disk / on-the-wire / API contract: reusing or renumbering a
// bit is an ABI break.  This rule reports each single-constant #define so a
// reviewer can eyeball added/removed/renumbered bits in a PR.
//
// HONEST SCOPE NOTE: Coccinelle's #define matching in this spatch build
// (1.3.0) is incomplete -- it binds a single `constant` and misses flag
// defines whose value carries extra tokens or unusual spacing, so it reports
// ~131 of the 328 hex flag bits.  The AUTHORITATIVE flag inventory in CI is
// therefore produced by a one-line awk (see dist/cocci/flagbits_inventory.sh
// and README.md), which is exact.  This .cocci is kept as the SmPL/AST view
// and a sanity cross-check, NOT as the source of truth.
//
// Report mode (`*`): run with
//   spatch --sp-file dist/cocci/abi_flagbits.cocci --dir src/dbinc --include-headers
// and post-process the `-#define NAME VALUE` lines into NAME=VALUE.

@flagbit@
identifier N;
constant C =~ "^0[xX][0-9a-fA-F]+$";
@@
* #define N C
