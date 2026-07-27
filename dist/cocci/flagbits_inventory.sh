#!/bin/sh
# flagbits_inventory.sh -- authoritative NAME=VALUE inventory of flag-bit
# #defines in src/dbinc/*.h.  Exact (awk-based); the abi_flagbits.cocci rule is
# the AST cross-check only.  Output is sorted for stable diffing.
#
# Usage: sh dist/cocci/flagbits_inventory.sh [srcdir]
set -eu
SRC="${1:-src/dbinc}"
awk '
  /^#[ \t]*define[ \t]+[A-Z0-9_]+[ \t]+0[xX][0-9a-fA-F]+/ {
    name=$2
    val=$3
    printf "%s=%s\n", name, val
  }
' "$SRC"/*.h | sort -u
