/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_int.h --
 *	RFC 0014 internal structures for the typed-data and constraint layer.
 *
 * This header lives OUTSIDE src/dbinc/, so dist/s_sig does not scan it and
 * none of these structures enter the region signature (spec C2, C3).  It is
 * included only by the src/typed .c files, all whole-file guarded on
 * HAVE_TYPED.
 */

#ifndef	_DB_TYPED_INT_H_
#define	_DB_TYPED_INT_H_

#if defined(__cplusplus)
extern "C" {
#endif

/*
 * TYPED_STATE -- one per typed DB handle, hung off the process-global side
 * table __typed_slot[] (spec §3.1), reached by the slot number in the handle's
 * flags2 (DB2_TYPED_SLOT, db.in).  Never a field of ENV, __env or
 * __db_globals, which env_sig.c hashes.
 *
 * db_set_schema deep-copies the caller's DB_SCHEMA into this one allocation:
 * the schema, both record codecs, their types[] arrays, the constraint array
 * and all strings.  The DB_CODEC structs themselves and any unique_db handles
 * are NOT copied -- the caller keeps them alive to DB->close, as with any
 * libdb callback.
 */
typedef struct __typed_state {
	DB		*dbp;		/* Owner; DIAGNOSTIC asserts it. */
	DB_SCHEMA	 schema;	/* Deep copy; key/data point at the */
	DB_RECORD_CODEC	 key_rc, data_rc;	/* two record codecs below, */
	const DB_CODEC	**key_types;	/* whose types[] point at these */
	const DB_CODEC	**data_types;	/* arrays, never into caller memory. */
	DB_CONSTRAINT	*constraints;	/* nconstraints copies. */
	char		*strings;	/* All names, one buffer. */

	u_int32_t	 nunique;	/* # of UNIQUE constraints (§3.7). */
	u_int32_t	 key_memcmp;	/* every key field is MEMCMP. */
	u_int32_t	 key_equalimage;/* every key field is EQUALIMAGE. */

	u_int32_t	 ncodecs;	/* distinct DB_CODECs (§4.2 order). */
	const DB_CODEC	**codecs;	/* ncodecs entries, caller-owned. */
	u_int8_t	(*fp)[20];	/* one SHA-1 per codec (§5). */
} TYPED_STATE;

/*
 * The side table.  Slot 0 means "untyped": db_create zeroes flags2, so every
 * handle that never called db_set_schema has DB2_TYPED_SLOT(dbp) == 0.  Only
 * db_set_schema (claim) and __typed_release (free) ever write it, and both are
 * cold paths; readers do a plain indexed load (spec §3.2 -- no mutex on any
 * hot path).  32 KB of .bss, present only in an --enable-typed build.
 */
#define	TYPED_NSLOTS	4096	/* ponytail: fixed table; 16 slot bits allow
				 * 65535 if a process ever needs more. */

/*
 * Cross-file internal prototypes for the typed layer.  These are declared
 * here, not via s_include's PUBLIC markers, because they take TYPED_STATE *
 * and TYPED_STATE lives only in this header -- a generated typed_ext.h
 * prototype naming it would be visible (and unresolvable) in every TU that
 * includes db_int.h.  The three typed .c files all include this header.
 */
struct __env;
struct __db_thread_info;
struct __db_txn;

int __typed_catalog_write __P((DB *, struct __db_thread_info *,
    struct __db_txn *, const char *, const char *, TYPED_STATE *));
int __typed_catalog_check __P((DB *, struct __db_thread_info *,
    struct __db_txn *, const char *, const char *, TYPED_STATE *));

#if defined(__cplusplus)
}
#endif
#endif /* !_DB_TYPED_INT_H_ */
