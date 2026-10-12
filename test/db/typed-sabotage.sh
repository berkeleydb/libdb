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
ARMS="footprint_canary footprint_sig handle_sizes_typed typed_api_memcmp typed_api_vecorder typed_api_shallow typed_refusals_c6 typed_refusals_compress typed_refusals_cb typed_old_reader_mask typed_codec_mismatch_fp typed_catalog_lifecycle_remove typed_catalog_lifecycle_rename typed_catalog_format_version typed_rdonly_noschema_rule typed_notnull_hook typed_maxlen_boundary typed_valid_mapping typed_partial_step1 typed_unique_counter typed_unique_counter_mp typed_unique_d6 typed_unique_d6uncond typed_unique_nulls_donotindex typed_unique_nulls_prefix typed_unique_closed_step2 typed_btree_cmp_noinstall typed_hash_nohash typed_cmp_fuzz_bounds typed_optread_gate typed_recover_guard"

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

# §7.2 typed_unique (a) / typed_unique_mp: the RFC's arm -- __typed_unique_cb
# appends a per-call counter to the key, so no two secondary keys ever
# collide and both inserts of the same value commit.
arm_typed_unique_counter_files="src/typed/typed_gen.c"
arm_typed_unique_counter_apply() {
	edit src/typed/typed_gen.c \
'		if (!prefixed) {
			skey->data = (void *)p;
			skey->size = flen;
			return (0);
		}' \
'		if (!prefixed) {
			u_int8_t *sb;
			if (__os_umalloc(env, flen + pkey->size, &sb) != 0)
				return (ENOMEM);
			if (flen > 0) memcpy(sb, p, flen);
			if (pkey->size > 0)
				memcpy(sb + flen, pkey->data, pkey->size);
			skey->data = sb; skey->size = flen + pkey->size;
			F_SET(skey, DB_DBT_APPMALLOC);
			return (0);
		}'
}
arm_typed_unique_counter_test='TIER=typed sh "$HERE"/run_typed_unique.sh "$B"'
arm_typed_unique_counter_expect='VERDICT typed_unique FAIL|FAIL \[[147]\]'

arm_typed_unique_counter_mp_files="src/typed/typed_gen.c"
arm_typed_unique_counter_mp_apply() { arm_typed_unique_counter_apply ; }
arm_typed_unique_counter_mp_test='TIER=typed sh "$HERE"/run_typed_unique_mp.sh "$B"'
arm_typed_unique_counter_mp_expect='VERDICT typed_unique_mp FAIL'

# §7.2 typed_unique (b): revert the D6 edit (DB_KEYEXIST on a typed primary),
# so the loser sees EINVAL instead of DB_KEYEXIST.
arm_typed_unique_d6_files="src/db/db_cam.c"
arm_typed_unique_d6_apply() {
	edit src/db/db_cam.c \
	    '						ret = DB_KEYEXIST;' \
	    '						ret = EINVAL;'
}
arm_typed_unique_d6_test='TIER=typed sh "$HERE"/run_typed_unique.sh "$B"'
arm_typed_unique_d6_expect='VERDICT typed_unique FAIL|FAIL \[[147]\]'

# §7.2 typed_unique (c): make D6 unconditional (always DB_KEYEXIST), so the
# untyped-secondary CONTROL case gets DB_KEYEXIST instead of EINVAL.
arm_typed_unique_d6uncond_files="src/db/db_cam.c"
arm_typed_unique_d6uncond_apply() {
	edit src/db/db_cam.c \
'					if (sdbp->s_primary != NULL &&
					    F2_ISSET(sdbp->s_primary,
					    DB2_AM_TYPED))' \
'					if (1)'
}
arm_typed_unique_d6uncond_test='TIER=typed sh "$HERE"/run_typed_unique.sh "$B"'
arm_typed_unique_d6uncond_expect='VERDICT typed_unique FAIL|FAIL \[10\]'

# §7.2 typed_unique_nulls (a): return a key instead of DB_DONOTINDEX for a
# NULL field under NULLS DISTINCT, so the two NULL rows collide.
arm_typed_unique_nulls_donotindex_files="src/typed/typed_gen.c"
arm_typed_unique_nulls_donotindex_apply() {
	edit src/typed/typed_gen.c \
	    '			return (DB_DONOTINDEX);	/* NULLS DISTINCT */' \
	    '			;	/* NULLS DISTINCT (sabotage: fall through) */'
}
arm_typed_unique_nulls_donotindex_test='TIER=typed sh "$HERE"/run_typed_unique_nulls.sh "$B"'
arm_typed_unique_nulls_donotindex_expect='VERDICT typed_unique_nulls FAIL|FAIL \[[234]\]'

# §7.2 typed_unique_nulls (b): drop the 0x01 prefix on non-NULL images under
# NULLS_NOT_DISTINCT, so a non-NULL 0x00 value collides with the NULL image.
arm_typed_unique_nulls_prefix_files="src/typed/typed_gen.c"
arm_typed_unique_nulls_prefix_apply() {
	# Both EQUALIMAGE/MEMCMP and sortkey prefix sites.
	"$PY" - "$REPO/src/typed/typed_gen.c" <<'PYEOF'
import sys
p=sys.argv[1]; s=open(p).read()
n=s.count("out[0] = 0x01;")
if n < 1: sys.exit("prefix anchor missing")
s=s.replace("out[0] = 0x01;", "out[0] = 0x00;")
open(p,"w").write(s)
PYEOF
}
arm_typed_unique_nulls_prefix_test='TIER=typed sh "$HERE"/run_typed_unique_nulls.sh "$B"'
arm_typed_unique_nulls_prefix_expect='VERDICT typed_unique_nulls FAIL|FAIL \[9\]'

# §7.2 typed_unique_closed: delete step 2 (the closed-unique-index check), so
# a put after the unique index is closed is silently accepted.
arm_typed_unique_closed_step2_files="src/typed/typed.c"
arm_typed_unique_closed_step2_apply() {
	edit src/typed/typed.c \
	    '		if (nfound < ts->nunique) {' \
	    '		if (0 && nfound < ts->nunique) {'
}
arm_typed_unique_closed_step2_test='TIER=typed sh "$HERE"/run_typed_unique_closed.sh "$B"'
arm_typed_unique_closed_step2_expect='VERDICT typed_unique_closed FAIL|FAIL \[[34]\]'

# §7.2 typed_notnull: delete the __typed_put_check call in __dbc_put -> no
# constraint is checked, so a NOT NULL violation is stored.
arm_typed_notnull_hook_files="src/db/db_cam.c"
arm_typed_notnull_hook_apply() {
	edit src/db/db_cam.c \
	    '	if (F2_ISSET(dbp, DB2_AM_TYPED) && flags != DB_UPDATE_SECONDARY &&' \
	    '	if (0 && F2_ISSET(dbp, DB2_AM_TYPED) && flags != DB_UPDATE_SECONDARY &&'
}
arm_typed_notnull_hook_test='TIER=typed sh "$HERE"/run_typed_notnull.sh "$B"'
arm_typed_notnull_hook_expect='VERDICT typed_notnull FAIL|FAIL \[1\]'

# §7.2 typed_maxlen (b): change "len > maxlen" to "len > maxlen + 1" at both
# the field and whole-DBT sites, so a maxlen+1 field/DBT is wrongly accepted.
arm_typed_maxlen_boundary_files="src/typed/typed.c"
arm_typed_maxlen_boundary_apply() {
	edit src/typed/typed.c \
	    '			if (flen > con->maxlen)' \
	    '			if (flen > con->maxlen + 1)' &&
	edit src/typed/typed.c \
	    '			if (dbt->size > con->maxlen)' \
	    '			if (dbt->size > con->maxlen + 1)'
}
arm_typed_maxlen_boundary_test='TIER=typed sh "$HERE"/run_typed_maxlen.sh "$B"'
arm_typed_maxlen_boundary_expect='VERDICT typed_maxlen FAIL|FAIL \[[24]\]'

# §7.2 typed_valid: delete the DB_CODEC_MALFORMED -> DB_CONSTRAINT_VIOLATION
# mapping in the codec-validate path, so a MALFORMED leaks out as -30964.
arm_typed_valid_mapping_files="src/typed/typed.c"
arm_typed_valid_mapping_apply() {
	edit src/typed/typed.c \
	    '				if (ret == DB_CODEC_MALFORMED)
					goto violated;' \
	    '				if (0 && ret == DB_CODEC_MALFORMED)
					goto violated;'
}
arm_typed_valid_mapping_test='TIER=typed sh "$HERE"/run_typed_valid.sh "$B"'
arm_typed_valid_mapping_expect='VERDICT typed_valid FAIL|FAIL \[[23]\]'

# §7.2 typed_partial: delete step 1 (the DB_DBT_PARTIAL refusal), so a partial
# put on a typed database is accepted.
arm_typed_partial_step1_files="src/typed/typed.c"
arm_typed_partial_step1_apply() {
	edit src/typed/typed.c \
	    '	if ((key != NULL && F_ISSET(key, DB_DBT_PARTIAL)) ||' \
	    '	if ((key != NULL && 0) ||' &&
	edit src/typed/typed.c \
	    '	    (data != NULL && F_ISSET(data, DB_DBT_PARTIAL))) {' \
	    '	    (data != NULL && 0)) {'
}
arm_typed_partial_step1_test='TIER=typed sh "$HERE"/run_typed_partial.sh "$B"'
arm_typed_partial_step1_expect='VERDICT typed_partial FAIL|FAIL \[[12]\]'

# §7.2 typed_btree_cmp: do not install __typed_bt_compare -> the Btree uses
# __bam_defcmp, so a reverse-order key scans bytewise (1,2,3) instead of codec
# order (3,2,1), and the all-MEMCMP control is unaffected.
arm_typed_btree_cmp_noinstall_files="src/typed/typed.c"
arm_typed_btree_cmp_noinstall_apply() {
	edit src/typed/typed.c \
	    '		bt->bt_compare = __typed_bt_compare;' \
	    '		bt->bt_compare = __bam_defcmp;	/* sabotage: no install */'
}
arm_typed_btree_cmp_noinstall_test='TIER=typed sh "$HERE"/run_typed_btree_cmp.sh "$B"'
arm_typed_btree_cmp_noinstall_expect='VERDICT typed_btree_cmp FAIL|FAIL \[[0-9]+\]'

# §7.2 typed_hash: do not install the generated h_hash -> "abc" and "AbC"
# hash to different buckets (default __ham_func5 is byte-sensitive), so the
# case-insensitive get misses.
arm_typed_hash_nohash_files="src/typed/typed.c"
arm_typed_hash_nohash_apply() {
	edit src/typed/typed.c \
	    '		ht->h_hash = __typed_h_hash;' \
	    '		/* sabotage: no h_hash install (keeps __ham_func5) */ ;'
}
arm_typed_hash_nohash_test='TIER=typed sh "$HERE"/run_typed_hash.sh "$B"'
arm_typed_hash_nohash_expect='VERDICT typed_hash FAIL|FAIL \[[0-9]+\]'

# §7.2 typed_cmp_fuzz: delete the §6.3 slice-bounds check in __typed_walk_field
# -> a lying field() that returns a slice past the end is handed to a type
# codec, which reads off the end of the exact-sized heap block: ASan reports a
# heap-buffer-overflow and the test dies on a signal / exits non-zero.
arm_typed_cmp_fuzz_bounds_files="src/typed/typed_gen.c"
arm_typed_cmp_fuzz_bounds_apply() {
	edit src/typed/typed_gen.c \
	    '	if (p < d || (size_t)(p - d) > size || len > size - (size_t)(p - d))
		return (DB_CODEC_MALFORMED);' \
	    '	if (0)	/* sabotage: §6.3 slice-bounds check deleted */
		return (DB_CODEC_MALFORMED);'
}
arm_typed_cmp_fuzz_bounds_test='TIER=typed sh "$HERE"/run_typed_cmp_fuzz.sh "$B"'
arm_typed_cmp_fuzz_bounds_expect='VERDICT typed_cmp_fuzz FAIL|heap-buffer-overflow|FAIL \[[0-9]+\]|died on signal'

# §7.2 typed_optread: delete the §6.2 record-codec TOTAL check -> a non-TOTAL
# record codec is no longer refused with DB_OPTREAD on, so cases 1 and 2 open
# instead of returning EINVAL.
arm_typed_optread_gate_files="src/typed/typed.c"
arm_typed_optread_gate_apply() {
	edit src/typed/typed.c \
	    '		if (!FLD_ISSET(ts->schema.key->flags, DB_CODEC_TOTAL)) {' \
	    '		if (0 && !FLD_ISSET(ts->schema.key->flags, DB_CODEC_TOTAL)) {' &&
	edit src/typed/typed.c \
	    '			if (!FLD_ISSET(ts->schema.key->types[i]->flags,
			    DB_CODEC_TOTAL)) {' \
	    '			if (0 && !FLD_ISSET(ts->schema.key->types[i]->flags,
			    DB_CODEC_TOTAL)) {'
}
arm_typed_optread_gate_test='TIER=typed sh "$HERE"/run_typed_optread.sh "$B"'
arm_typed_optread_gate_expect='VERDICT typed_optread FAIL|FAIL \[[12]\]'

# §7.2 typed_recover: delete the DB_AM_RECOVER guard from the §3.5 open hook
# dispatch -> recovery's schema-less, read-write opens of the typed
# subdatabases now reach __typed_open, which refuses a schema-less read-write
# open ("db_set_schema required to open for writing"), so db_recover exits
# non-zero and the test's "db_recover exited 0" check FAILs.
arm_typed_recover_guard_files="src/db/db_open.c"
arm_typed_recover_guard_apply() {
	edit src/db/db_open.c \
	    '	if (F2_ISSET(dbp, DB2_AM_TYPED | DB2_AM_TYPED_FILE) &&
	    !F_ISSET(dbp, DB_AM_RECOVER) &&' \
	    '	if (F2_ISSET(dbp, DB2_AM_TYPED | DB2_AM_TYPED_FILE) &&
	    1 &&	/* sabotage: DB_AM_RECOVER guard removed */'
}
arm_typed_recover_guard_test='TIER=typed sh "$HERE"/run_typed_recover.sh "$B"'
arm_typed_recover_guard_expect='VERDICT typed_recover FAIL|FAIL \[[0-9]+\]'

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
