#!/bin/sh
# run_conventions.sh -- run the rule_*.cocci convention checks over the libdb
# source tree and emit a stable, line-number-free violation list.
#
# Each violation is keyed as:   RULE|TAG|relpath|code-signature
# where code-signature is the matched source line with leading indentation and
# the //@TAG@ marker stripped.  No file:line is used, so unrelated edits that
# renumber lines do not reshuffle the baseline (see README.md).
#
# Why the K&R->ANSI shim: this spatch (1.3.0) cannot parse Berkeley DB's K&R
# function definitions (name(a,b)\n TYPE a; ... {), which silently drops whole
# function bodies from analysis (measured directory recall ~13% without it).
# The shim collapses "name(args)\n <decls>\n {" to "name(args) {\n <decls>" on a
# throwaway copy, PRESERVING line count, which lifts recall to ~97% of call
# sites.  spatch runs against the shimmed copy; -I still points at the real
# headers so macros/typedefs resolve.
#
# Usage: sh dist/cocci/run_conventions.sh [repo-root]
#   Env: SPATCH (default spatch), COCCI_DIR (default dist/cocci)
set -eu

ROOT="${1:-$(pwd)}"
cd "$ROOT"
SPATCH="${SPATCH:-spatch}"
COCCI_DIR="${COCCI_DIR:-dist/cocci}"
SHIM="$(mktemp -d)"
trap 'rm -rf "$SHIM"' EXIT

# 1. Build the line-preserving K&R->ANSI shim of every src/**/*.c.
find src -name '*.c' | while read -r f; do
	mkdir -p "$SHIM/$(dirname "$f")"
	python3 "$COCCI_DIR/kr2ansi.py" "$f" "$SHIM/$f"
done

INCLUDES="-I $ROOT/build_unix -I $ROOT/src -I $ROOT/src/dbinc -I $ROOT/src/dbinc_auto"

# 2. Run each rule, map shim paths back to real paths, strip line numbers.
for rule in "$COCCI_DIR"/rule_*.cocci; do
	rname=$(basename "$rule" .cocci)
	"$SPATCH" --force-kr $INCLUDES \
		--macro-file "$COCCI_DIR/bdb_defs.h" \
		--sp-file "$rule" --dir "$SHIM/src" 2>/dev/null |
	awk -v rule="$rname" '
		/^\+\+\+ / { next }
		/^--- / {
			# --dir mode emits git-style "--- a/<path-under-dir>";
			# our --dir is <shim>/src, so a/foo -> src/foo.
			f=$2
			sub(/^a\//,"src/",f)
			sub(/^.*\/src\//,"src/",f)
			next
		}
		/\/\/@[A-Z_]+@/ {
			line=$0
			sub(/^\+/,"",line)                 # drop diff marker
			match(line,/\/\/@[A-Z_]+@/)
			tag=substr(line,RSTART+3,RLENGTH-4) # NAME between //@ and @
			sig=line
			sub(/[ \t]*\/\/@[A-Z_]+@[ \t]*/," ",sig)  # remove marker
			gsub(/^[ \t]+/,"",sig); gsub(/[ \t]+$/,"",sig)
			gsub(/[ \t]+/," ",sig)
			printf "%s|%s|%s|%s\n", rule, tag, f, sig
		}
	'
done | sort -u
