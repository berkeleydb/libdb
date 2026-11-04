#!/bin/sh
#
# test/cbmc/check_copies.sh -- every verbatim copy in a harness must still
# match the function in src/.
#
# A harness that copies a static function (rather than #include-ing its .c)
# proves a property of the COPY.  If src/ changes and the copy does not, the
# harness keeps passing about code that no longer ships.  That happened: the
# okitem harness kept a private copy of the unparenthesised HPAGE_PTYPE macro
# and "reproduced" a bug for weeks after 2ca81268a fixed it.
#
# Each copy is marked in its harness:
#   /* --- BEGIN verbatim copy of <func> from <src/path.c> --- */
#   ...
#   /* --- END verbatim copy --- */
# The function in src/ runs from the line "<func>(" to the next line that is
# exactly "}".  The copy is compared from its own "<func>(" line, and the
# line before that ("static int" etc.) is ignored, since the copy may add
# "static".  Comments, blank lines and whitespace differences are ignored;
# every code token is compared.
#
# Exit 0 if every copy matches, 1 otherwise.
set -u
cd "$(dirname "$0")"
root=../..
rc=0
n=0
for h in harness_*.c; do
	grep -n 'BEGIN verbatim copy of ' "$h" | while IFS=: read -r ln rest; do
		func=$(echo "$rest" | sed -n 's/.*verbatim copy of \([A-Za-z_0-9]*\) from \([^ ]*\) .*/\1/p')
		src=$(echo "$rest" | sed -n 's/.*verbatim copy of [A-Za-z_0-9]* from \([^ ]*\) .*/\1/p')
		[ -n "$func" ] && [ -n "$src" ] || { echo "MALFORMED marker: $h:$ln"; echo x >> .copies.bad; continue; }
		[ -f "$root/$src" ] || { echo "MISSING source: $h names $src"; echo x >> .copies.bad; continue; }
		awk -v f="$func" 'index($0, f "(") == 1 {p = 1} p {print} p && $0 == "}" {exit}' \
		    "$root/$src" > .copies.src
		awk -v start="$ln" -v f="$func" 'NR > start && index($0, f "(") == 1 {p = 1}
		    p {print} p && $0 == "}" {exit}' "$h" > .copies.cpy
		# Compare the CODE: strip C comments from both sides, since the copies
		# may omit them.  A change to any token still shows.
		for x in .copies.src .copies.cpy; do
			sed -e ':a' -e '/\/\*/!b' -e '/\*\//!{N;ba' -e '}' \
			    -e 's:/\*.*\*/::' "$x" | grep -v '^[[:space:]]*$' > "$x.nc"
			mv "$x.nc" "$x"
		done
		if [ ! -s .copies.src ]; then
			echo "NOT FOUND in $src: $func (named by $h)"; echo x >> .copies.bad
		elif diff -bB .copies.src .copies.cpy > .copies.diff; then
			echo "ok    $h: $func matches $src"
		else
			echo "DRIFT $h: $func differs from $src:"; sed 's/^/        /' .copies.diff | head -20
			echo x >> .copies.bad
		fi
	done
done
[ -s .copies.bad ] && rc=1
rm -f .copies.src .copies.cpy .copies.diff .copies.bad
exit $rc
