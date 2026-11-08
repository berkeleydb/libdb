#!/bin/sh
#
# $Id$
#
# check_rep_cmpvote_copy.sh -- the verbatim copy of __rep_cmp_vote embedded in
# rep_cmpvote_order.c must still match the function in src/rep/rep_elect.c.  A
# gate that copies a static function proves a property of the COPY; if src/
# changes and the copy does not, the gate silently tests code that no longer
# ships.  Same discipline as test/cbmc/check_copies.sh.
#
# The copy is marked:
#   /* --- BEGIN verbatim copy of __rep_cmp_vote from src/rep/rep_elect.c --- */
#   ...
#   /* --- END verbatim copy --- */
# Comments, blank lines and whitespace differences are ignored; every code
# token is compared.  Exit 0 if it matches, 1 otherwise.

set -u
_HERE=$(cd "$(dirname "$0")" && pwd)
root="$_HERE/../.."
h="$_HERE/rep_cmpvote_order.c"
func=__rep_cmp_vote
src="src/rep/rep_elect.c"
tmp=$(mktemp -d)
rc=0

[ -f "$root/$src" ] || { echo "MISSING source: $root/$src"; exit 1; }

# The function in src/ runs from the line "<func>(" to the next line that is
# exactly "}".
awk -v f="$func" 'index($0, f "(") == 1 {p=1} p {print} p && $0 == "}" {exit}' \
    "$root/$src" > "$tmp/src"

# The copy runs from its BEGIN marker's following "<func>(" line to the next
# line that is exactly "}".
ln=$(grep -n 'BEGIN verbatim copy of __rep_cmp_vote' "$h" | head -1 | cut -d: -f1)
[ -n "$ln" ] || { echo "no BEGIN marker in $h"; exit 1; }
awk -v start="$ln" -v f="$func" 'NR > start && index($0, f "(") == 1 {p=1}
    p {print} p && $0 == "}" {exit}' "$h" > "$tmp/cpy"

# Strip C comments and blank lines from both sides; compare code tokens.
for x in src cpy; do
	sed -e ':a' -e '/\/\*/!b' -e '/\*\//!{N;ba' -e '}' -e 's:/\*.*\*/::' \
	    "$tmp/$x" | grep -v '^[[:space:]]*$' > "$tmp/$x.nc"
	mv "$tmp/$x.nc" "$tmp/$x"
done

if [ ! -s "$tmp/src" ]; then
	echo "NOT FOUND in $src: $func"; rc=1
elif diff -bB "$tmp/src" "$tmp/cpy" > "$tmp/diff"; then
	:
else
	echo "DRIFT: embedded $func differs from $src:"
	sed 's/^/    /' "$tmp/diff" | head -40
	rc=1
fi

rm -rf "$tmp"
exit "$rc"
