#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# test/db/typed-sabotage.sh -- teeth for the RFC 0014 tests (spec §7.1, §7.2).
#
# Each ARM re-introduces one defect (the exact revert named in §7.2), rebuilds
# the given build dir, runs the one test that must catch it, REQUIRES that test
# to FAIL, then restores the tree and rebuilds.  An arm whose test still passes
# means the TEST is wrong -- that is the vacuous-green shape this project has
# shipped eleven times -- so this exits non-zero then.
#
# Not in CI: every arm is red by design.  The implementer runs every arm before
# each commit and pastes the FAIL lines into the commit message (§9).  Follows
# test/c/flagapi-sabotage.sh.
#
# Usage:  sh test/db/typed-sabotage.sh <build-dir> [arm ...]     (default: all)
#         sh test/db/typed-sabotage.sh -l                        (list arms)
# Exit 0 only when every requested arm made its test FAIL.
#
# An arm is three functions: arm_<id>_apply (edit the tree; non-zero if the
# anchor text is gone, so a moved anchor cannot pass vacuously), the test to
# run (arm_<id>_test), and the FAIL text it must print (arm_<id>_expect, a
# grep -E pattern).  The tree is restored from a copy of every file an arm
# touched, whatever happens.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$HERE/../.." && pwd)
MAKE=${MAKE:-$(command -v gmake || command -v make)}
PY=${PYTHON:-python3}

# edit FILE OLD NEW -- replace exactly one occurrence of OLD (a literal) or fail.
edit() {
	"$PY" - "$REPO/$1" "$2" "$3" <<'EOF'
import sys
p, old, new = sys.argv[1], sys.argv[2], sys.argv[3]
s = open(p).read()
n = s.count(old)
if n != 1:
    sys.exit("sabotage anchor found %d times in %s (want 1): the code moved, "
             "update test/db/typed-sabotage.sh" % (n, p))
open(p, 'w').write(s.replace(old, new, 1))
EOF
}

# ---------------------------------------------------------------------------
# Arms.  ARMS lists them in run order.  Each test command runs in the build
# dir with BUILD set; TIER is set to the test's MANIFEST tier.
# ---------------------------------------------------------------------------
ARMS="footprint_canary footprint_sig handle_sizes_typed typed_api_memcmp typed_api_vecorder typed_api_shallow typed_refusals_c6 typed_refusals_compress typed_refusals_cb typed_old_reader_mask typed_codec_mismatch_fp typed_catalog_lifecycle_remove typed_catalog_lifecycle_rename typed_catalog_format_version typed_rdonly_noschema_rule"

# §7.2 typed_footprint (a): an unguarded symbol in an engine file must move
# the default build's .text.
arm_footprint_canary_files="src/db/db_cam.c"
arm_footprint_canary_apply() {
	printf '\nint __typed_canary(void);\nint\n__typed_canary(void)\n{\n\treturn (1);\n}\n' \
	    >> "$REPO/src/db/db_cam.c"
}
arm_footprint_canary_test="TIER=db sh $HERE/run_typed_footprint.sh"
arm_footprint_canary_expect='^VERDICT typed_footprint FAIL.* 1\(\.text'
arm_footprint_canary_rebuild=0	# the runner builds its own trees

# §7.2 typed_footprint (b): make the typed structs re-enter the signature,
# the exact failure dropping the four s_sig exclusions (C2) would cause.  Done
# surgically on the committed env_sig.c rather than by rerunning s_sig: s_sig
# regenerates the WHOLE file, which on this tree also re-adds pre-existing
# structs env_sig.c's include set cannot see (a separate, pre-existing
# staleness, spec §11 V7), breaking the build before the signature can be
# compared.  Adding the four __ADD lines and bumping the count isolates the
# one thing the exclusions prevent: the typed structs changing the sig.  The
# footprint test builds its own trees from $REPO; the committed f13921ff7 base
# has neither the structs nor the __ADD lines, so item 2 (base vs head) moves.
arm_footprint_sig_files="src/env/env_sig.c"
arm_footprint_sig_apply() {
	awk '
		/^#define[\t ]+__STRUCTURE_COUNT[\t ]+\(42 \+ 104\)/ {
			sub(/104/, "108"); print; next
		}
		/^\t__ADD\(__db\);/ {
			print
			print "\t__ADD(__db_codec);"
			print "\t__ADD(__db_record_codec);"
			print "\t__ADD(__db_constraint);"
			print "\t__ADD(__db_schema);"
			done = 1; next
		}
		{ print }
		END { if (!done) exit 3 }
	' "$REPO/src/env/env_sig.c" > "$REPO/src/env/env_sig.c.sab" || return 1
	mv "$REPO/src/env/env_sig.c.sab" "$REPO/src/env/env_sig.c"
}
arm_footprint_sig_test="TIER=db sh $HERE/run_typed_footprint.sh"
arm_footprint_sig_expect='^VERDICT typed_footprint FAIL.*[24]\(sig'
arm_footprint_sig_rebuild=0	# the runner builds its own trees

# §7.2 handle_sizes (typed): a field added to struct __db under HAVE_TYPED
# grows sizeof(DB) in a typed build, so handle_sizes (typed tier) must FAIL.
# Run this arm against a --enable-typed BUILD dir.  db.h is generated from
# db.in by config.status (not by make), so the apply step edits db.in and
# regenerates db.h in $BUILD; the test then recompiles handle_sizes against
# the grown struct.  $B is the build dir, exported by the runner.
arm_handle_sizes_typed_files="src/dbinc/db.in"
arm_handle_sizes_typed_apply() {
	# Insert the guarded field just before struct __db's closing brace,
	# which immediately follows the flags2 line.  awk keeps real tabs.
	awk '
		/^\tu_int32_t flags2;/ && !done {
			print; getline nl
			if (nl == "};") {
				print "#ifdef HAVE_TYPED"
				print "\tvoid *typed_internal;"
				print "#endif"
				done = 1
			}
			print nl; next
		}
		{ print }
		END { if (!done) exit 3 }
	' "$REPO/src/dbinc/db.in" > "$REPO/src/dbinc/db.in.sab" || return 1
	mv "$REPO/src/dbinc/db.in.sab" "$REPO/src/dbinc/db.in"
	# Regenerate db.h (and the other config files) from the edited db.in.
	( cd "$B" && sh ./config.status ) >/dev/null 2>&1
}
arm_handle_sizes_typed_test='TIER=typed sh "$HERE"/run_handle_sizes.sh "$B"'
arm_handle_sizes_typed_expect='VERDICT handle_sizes FAIL'

# §7.2 typed_api (a): delete the "compare == NULL => MEMCMP" rule (§3.4 rule
# 5).  A codec with no compare and no MEMCMP must then be accepted, so
# typed_api's "no-compare non-MEMCMP codec -> EINVAL" check FAILs.
arm_typed_api_memcmp_files="src/typed/typed.c"
arm_typed_api_memcmp_apply() {
	edit src/typed/typed.c \
	    'if (c->compare == NULL && !memcmp_ok) {' \
	    'if (0 && c->compare == NULL && !memcmp_ok) {'
}
arm_typed_api_memcmp_test='TIER=typed sh "$HERE"/run_typed_api.sh "$B"'
arm_typed_api_memcmp_expect='VERDICT typed_api FAIL|FAIL \[7\]'

# §7.2 typed_api (b): delete the vector-order self-check (§5.1).  An ascending
# vector list for a descending (reverse) codec must then be accepted, so the
# "unordered vectors -> EINVAL" check FAILs.
arm_typed_api_vecorder_files="src/typed/typed_catalog.c"
arm_typed_api_vecorder_apply() {
	edit src/typed/typed_catalog.c \
	    'if (cmp > 0) {' \
	    'if (0 && cmp > 0) {'
}
arm_typed_api_vecorder_test='TIER=typed sh "$HERE"/run_typed_api.sh "$B"'
arm_typed_api_vecorder_expect='VERDICT typed_api FAIL|FAIL \[8\]'

# §7.2 typed_api (c): make the schema copy shallow -- point the stored record
# codec at the CALLER's types[] array instead of the deep copy.  Scribbling
# the caller's array after db_set_schema then changes what db_get_schema
# returns, so the "types[] survives scribble" check FAILs.
arm_typed_api_shallow_files="src/typed/typed.c"
arm_typed_api_shallow_apply() {
	edit src/typed/typed.c \
	    'ts->key_rc.types = ts->key_types;' \
	    'ts->key_rc.types = schema->key->types;'
}
arm_typed_api_shallow_test='TIER=typed sh "$HERE"/run_typed_api.sh "$B"'
arm_typed_api_shallow_expect='VERDICT typed_api FAIL|FAIL \[1[34]\]'

# §7.2 typed_refusals (a): delete the C6 block in __db_open_arg.  fname==NULL
# and dname==NULL typed opens are then no longer refused there.
arm_typed_refusals_c6_files="src/db/db_iface.c"
arm_typed_refusals_c6_apply() {
	edit src/db/db_iface.c \
	    'if (F2_ISSET(dbp, DB2_AM_TYPED) && (fname == NULL || dname == NULL)) {' \
	    'if (0 && F2_ISSET(dbp, DB2_AM_TYPED) && (fname == NULL || dname == NULL)) {'
}
arm_typed_refusals_c6_test='TIER=typed sh "$HERE"/run_typed_refusals.sh "$B"'
arm_typed_refusals_c6_expect='VERDICT typed_refusals FAIL|FAIL \[[246]\]'

# §7.2 typed_refusals (b): delete the compression check in __typed_open.
# set_bt_compress and a pre-existing BTM_COMPRESS subdatabase are then
# accepted.
arm_typed_refusals_compress_files="src/typed/typed.c"
arm_typed_refusals_compress_apply() {
	edit src/typed/typed.c \
	    '	if (dbp->type == DB_BTREE && dbp->bt_internal != NULL &&' \
	    '	if (0 && dbp->type == DB_BTREE && dbp->bt_internal != NULL &&'
}
arm_typed_refusals_compress_test='TIER=typed sh "$HERE"/run_typed_refusals.sh "$B"'
arm_typed_refusals_compress_expect='VERDICT typed_refusals FAIL|FAIL \[1[47]\]'

# §7.2 typed_refusals (c): delete the callback-identity check in __typed_open.
# A set_bt_compare after db_set_schema is then accepted at open.
arm_typed_refusals_cb_files="src/typed/typed.c"
arm_typed_refusals_cb_apply() {
	edit src/typed/typed.c \
	    '	if (ts != NULL && dbp->type == DB_BTREE && dbp->bt_internal != NULL) {' \
	    '	if (0 && ts != NULL && dbp->type == DB_BTREE && dbp->bt_internal != NULL) {'
}
arm_typed_refusals_cb_test='TIER=typed sh "$HERE"/run_typed_refusals.sh "$B"'
arm_typed_refusals_cb_expect='VERDICT typed_refusals FAIL|FAIL \[16\]'

# §7.2 typed_old_reader: drop the #ifdef HAVE_TYPED guard on the widened mask
# by making the DEFAULT (#else) fchk accept BTM_TYPED too, so a build without
# the typed layer opens the fixture instead of refusing it (BDB0055).  Run
# against the DEFAULT build dir ($B must be the default build for this arm).
arm_typed_old_reader_mask_files="src/btree/bt_open.c src/hash/hash_open.c"
arm_typed_old_reader_mask_apply() {
	edit src/btree/bt_open.c \
	    '	    __db_fchk(env, "DB->open", btm->dbmeta.flags, BTM_MASK)) != 0)' \
	    '	    __db_fchk(env, "DB->open", btm->dbmeta.flags, BTM_MASK | BTM_TYPED)) != 0)' &&
	edit src/hash/hash_open.c \
	    '	    DB_HASH_DUP | DB_HASH_SUBDB | DB_HASH_DUPSORT)) != 0)' \
	    '	    DB_HASH_DUP | DB_HASH_SUBDB | DB_HASH_DUPSORT | DB_HASH_TYPED)) != 0)'
}
arm_typed_old_reader_mask_test='TIER=db sh "$HERE"/run_typed_old_reader.sh "$B"'
arm_typed_old_reader_mask_expect='VERDICT typed_old_reader FAIL|FAIL \['

# §7.2 typed_codec_mismatch: skip the fingerprint comparison in the catalog
# check.  A v1 schema with a REVERSED comparator (same name/version/flags,
# different fingerprint) is then accepted instead of DB_CODEC_MISMATCH.
arm_typed_codec_mismatch_fp_files="src/typed/typed_catalog.c"
arm_typed_codec_mismatch_fp_apply() {
	edit src/typed/typed_catalog.c \
	    '		    memcmp(p, ts->fp[i], 20) != 0) {' \
	    '		    0) {'
}
arm_typed_codec_mismatch_fp_test='TIER=typed sh "$HERE"/run_typed_codec_mismatch.sh "$B"'
arm_typed_codec_mismatch_fp_expect='VERDICT typed_codec_mismatch FAIL|FAIL \[3\]'

# §7.2 typed_catalog_lifecycle (a): delete the remove hook -> the catalog
# record is left behind after DB->remove.
arm_typed_catalog_lifecycle_remove_files="src/db/db_remove.c"
arm_typed_catalog_lifecycle_remove_apply() {
	edit src/db/db_remove.c \
	    '	if (F2_ISSET(sdbp, DB2_AM_TYPED_FILE) && (ret = __typed_catalog_remove(' \
	    '	if (0 && F2_ISSET(sdbp, DB2_AM_TYPED_FILE) && (ret = __typed_catalog_remove('
}
arm_typed_catalog_lifecycle_remove_test='TIER=typed sh "$HERE"/run_typed_catalog_lifecycle.sh "$B"'
arm_typed_catalog_lifecycle_remove_expect='VERDICT typed_catalog_lifecycle FAIL|FAIL \[4\]'

# §7.2 typed_catalog_lifecycle (b): delete the rename hook -> the record does
# not move, so the new name opens "no catalog record" and the old name lingers.
arm_typed_catalog_lifecycle_rename_files="src/db/db_rename.c"
arm_typed_catalog_lifecycle_rename_apply() {
	edit src/db/db_rename.c \
	    '	if (typed_subdb && (ret = __typed_catalog_rename(' \
	    '	if (0 && typed_subdb && (ret = __typed_catalog_rename('
}
arm_typed_catalog_lifecycle_rename_test='TIER=typed sh "$HERE"/run_typed_catalog_lifecycle.sh "$B"'
arm_typed_catalog_lifecycle_rename_expect='VERDICT typed_catalog_lifecycle FAIL|FAIL \[[89]\]|FAIL \[1[01]\]'

# §7.2 typed_catalog_format: delete the version check -> a format-version-2
# record is accepted (until a later field trips, so the version row FAILs).
arm_typed_catalog_format_version_files="src/typed/typed_catalog.c"
arm_typed_catalog_format_version_apply() {
	edit src/typed/typed_catalog.c \
	    '	if (ver > TYPED_CAT_VERSION) {' \
	    '	if (0 && ver > TYPED_CAT_VERSION) {'
}
arm_typed_catalog_format_version_test='TIER=typed sh "$HERE"/run_typed_catalog_format.sh "$B"'
arm_typed_catalog_format_version_expect='VERDICT typed_catalog_format FAIL|FAIL \[[67]\]'

# §7.2 typed_rdonly_noschema: delete the §4.1 read-only rule -> a schema-less
# read-write open of a typed subdatabase is then allowed.
arm_typed_rdonly_noschema_rule_files="src/typed/typed.c"
arm_typed_rdonly_noschema_rule_apply() {
	edit src/typed/typed.c \
	    '		if (!F_ISSET(dbp, DB_AM_RDONLY) && !LF_ISSET(DB_WRITEOPEN)) {' \
	    '		if (0 && !F_ISSET(dbp, DB_AM_RDONLY) && !LF_ISSET(DB_WRITEOPEN)) {'
}
arm_typed_rdonly_noschema_rule_test='TIER=typed sh "$HERE"/run_typed_rdonly_noschema.sh "$B"'
arm_typed_rdonly_noschema_rule_expect='VERDICT typed_rdonly_noschema FAIL|FAIL \[5\]'

# ---------------------------------------------------------------------------

[ "${1:-}" = -l ] && { for a in $ARMS ; do echo "$a" ; done ; exit 0 ; }
BUILD=${1:?usage: typed-sabotage.sh <build-dir> [arm ...]}
BUILD=$(cd "$BUILD" && pwd) || exit 1
shift
want=${*:-$ARMS}
B=$BUILD				# arms reference $B (build dir) and $HERE
export B HERE

save=$(mktemp -d) || exit 1
rc=0
for a in $want ; do
	eval "files=\${arm_${a}_files:-}"
	eval "tcmd=\${arm_${a}_test:-}"
	eval "expect=\${arm_${a}_expect:-}"
	eval "rebuild=\${arm_${a}_rebuild:-1}"
	[ -n "$tcmd" ] || { echo "ARM $a: unknown arm" ; rc=1 ; continue ; }

	for f in $files ; do
		mkdir -p "$save/$(dirname "$f")"
		cp "$REPO/$f" "$save/$f"
	done
	restore() {
		for f in $files ; do cp "$save/$f" "$REPO/$f" ; done
		# db.h is generated from db.in by config.status; if the arm
		# edited db.in, regenerate db.h so the restored build matches.
		case " $files " in
		*" src/dbinc/db.in "*)
			( cd "$B" && sh ./config.status ) >/dev/null 2>&1 ;;
		esac
	}
	trap 'restore ; exit 1' INT TERM

	if ! "arm_${a}_apply" ; then
		echo "ARM $a: could not apply (anchor missing)"
		restore ; rc=1 ; continue
	fi
	if [ "$rebuild" = 1 ] &&
	    ! (cd "$BUILD" && $MAKE -j4) > "$BUILD/sabotage_$a.build.log" 2>&1 ; then
		echo "ARM $a: the sabotaged tree did not BUILD; proves nothing"
		echo "       see $BUILD/sabotage_$a.build.log"
		restore ; rc=1 ; continue
	fi
	out=$(cd "$BUILD" && BUILD="$BUILD" eval "$tcmd" 2>&1)
	trc=$?
	restore
	trap - INT TERM
	fl=$(echo "$out" | grep -E "$expect" | head -3)
	if [ "$trc" -ne 0 ] && [ -n "$fl" ] ; then
		echo "ARM $a: FAIL as required (rc=$trc):"
		echo "$fl" | sed 's/^/    /'
	else
		echo "ARM $a: SABOTAGE NOT DETECTED (rc=$trc) -- the test is wrong"
		echo "$out" | grep VERDICT | head -5 | sed 's/^/    /'
		rc=1
	fi
done

# Put the build back in the restored tree's state.
if [ -n "$want" ] ; then
	(cd "$BUILD" && $MAKE -j4) > "$BUILD/sabotage_restore.build.log" 2>&1 ||
	    { echo "restore rebuild FAILED: see $BUILD/sabotage_restore.build.log" ; rc=1 ; }
fi
[ -d "$save" ] && find "$save" -mindepth 1 -delete && rmdir "$save"
exit $rc
