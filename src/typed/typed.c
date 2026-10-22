/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed.c --
 *	RFC 0014 typed data and constraints: the typed-data API, the side
 *	table (D2), the put check and the open/close hooks.
 *
 * Whole-file guarded on HAVE_TYPED (--enable-typed / meson -Dtyped=enabled),
 * the os_aio_uring.c pattern: the file is in every build's source list and
 * compiles to nothing unless the option is on.
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/btree.h"
#include "dbinc/hash.h"
#include "typed_int.h"

#ifdef HAVE_TYPED
/*
 * Public API prototypes.  dist/s_include scans this file for the EXTERN
 * markers below (it is preprocessor-blind, so the HAVE_TYPED guard does not
 * hide them) and generates the ext_prot.in / ext_def.in entries.  The
 * DB_SCHEMA / DB_RECORD_CODEC / DB_CODEC types are declared unconditionally in
 * db.h, so these prototypes are valid in every build; the symbols exist only
 * in an --enable-typed library, so a caller of a default build fails at link
 * (fail-closed, §1.1).
 *
 * EXTERN: int db_set_schema __P((DB *, const DB_SCHEMA *, u_int32_t));
 *
 * EXTERN: int db_get_schema __P((DB *, const DB_SCHEMA **));
 *
 * EXTERN: int db_record_fixed __P((DB_RECORD_CODEC *, const char *, u_int32_t,
 * EXTERN:     u_int32_t, const u_int32_t *, const u_int32_t *,
 * EXTERN:     const DB_CODEC **));
 */

/*
 * The process-global side table (spec §3.1-§3.2).  Written only by
 * db_set_schema (claim) and __typed_release (free), both cold; read by the
 * put check and the generated callbacks with a plain indexed load, because
 * the slot is owned by the handle from db_set_schema to DB->close and using a
 * handle concurrently with its own close is already undefined in libdb.  No
 * mutex is taken on any path -- that is D2's non-negotiable (§3.2).  [0] is
 * never used: slot 0 means "untyped".
 */
static TYPED_STATE * volatile __typed_slot[TYPED_NSLOTS];
static db_atomic_t __typed_hint_a;	/* rotating start for the claim scan. */

static int  __typed_validate __P((DB *, const DB_SCHEMA *));
static int  __typed_validate_rc __P((DB *, const DB_RECORD_CODEC *));
static int  __typed_validate_codec __P((DB *, const DB_CODEC *));
static int  __typed_collect_codecs __P((DB *, const DB_SCHEMA *,
		const DB_CODEC **, u_int32_t *));
static int  __typed_claim __P((DB *, TYPED_STATE *, u_int32_t *));
static int  __typed_build_state __P((DB *, const DB_SCHEMA *, TYPED_STATE **));
static int  __typed_copy_schema __P((DB *, const DB_SCHEMA *, TYPED_STATE *));
static void __typed_free_state __P((DB *, TYPED_STATE *));
static int  __typed_check_side __P((DBC *, TYPED_STATE *, u_int32_t,
		const DB_RECORD_CODEC *, const DBT *));

/*
 * __typed_free_state --
 *	Free a TYPED_STATE and everything it owns (the one "strings" buffer,
 *	the constraint copies, the two types[] arrays, the codec list and the
 *	fingerprints).  The DB_CODEC structs and unique_db handles are caller-
 *	owned and never freed here.
 */
static void
__typed_free_state(dbp, ts)
	DB *dbp;
	TYPED_STATE *ts;
{
	ENV *env;

	if (ts == NULL)
		return;
	env = dbp->env;
	if (ts->strings != NULL)
		__os_free(env, ts->strings);
	if (ts->constraints != NULL)
		__os_free(env, ts->constraints);
	if (ts->key_types != NULL)
		__os_free(env, ts->key_types);
	if (ts->data_types != NULL)
		__os_free(env, ts->data_types);
	if (ts->codecs != NULL)
		__os_free(env, ts->codecs);
	if (ts->fp != NULL)
		__os_free(env, ts->fp);
	__os_free(env, ts);
}

/*
 * __typed_copy_schema --
 *	Deep-copy the caller's DB_SCHEMA into ts (spec §1.5).  Copies the
 *	schema struct, both record codecs, their types[] arrays, the
 *	constraint array, and every name string into one "strings" buffer.
 *	The DB_CODEC structs are referenced, not copied; unique_db handles are
 *	referenced too.
 */
static int
__typed_copy_schema(dbp, schema, ts)
	DB *dbp;
	const DB_SCHEMA *schema;
	TYPED_STATE *ts;
{
	ENV *env;
	size_t slen;
	char *sp;
	const DB_CONSTRAINT *scon;
	u_int32_t i;
	int ret;

	env = dbp->env;

	/* Size the string buffer: both record-codec names + constraint names. */
	slen = 0;
	if (schema->key != NULL)
		slen += strlen(schema->key->name) + 1;
	if (schema->data != NULL)
		slen += strlen(schema->data->name) + 1;
	for (i = 0; i < schema->nconstraints; i++)
		slen += strlen(schema->constraints[i].name) + 1;
	if (slen == 0)
		slen = 1;
	if ((ret = __os_malloc(env, slen, &ts->strings)) != 0)
		return (ret);
	sp = ts->strings;

	ts->schema = *schema;			/* struct copy; fix pointers. */
	ts->schema.key = ts->schema.data = NULL;
	ts->schema.constraints = NULL;

	if (schema->key != NULL) {
		ts->key_rc = *schema->key;
		if ((ret = __os_calloc(env, schema->key->nfields,
		    sizeof(DB_CODEC *), &ts->key_types)) != 0)
			return (ret);
		memcpy(ts->key_types, schema->key->types,
		    schema->key->nfields * sizeof(DB_CODEC *));
		ts->key_rc.types = ts->key_types;
		(void)strcpy(sp, schema->key->name);
		ts->key_rc.name = sp;
		sp += strlen(sp) + 1;
		ts->schema.key = &ts->key_rc;
	}
	if (schema->data != NULL) {
		ts->data_rc = *schema->data;
		if ((ret = __os_calloc(env, schema->data->nfields,
		    sizeof(DB_CODEC *), &ts->data_types)) != 0)
			return (ret);
		memcpy(ts->data_types, schema->data->types,
		    schema->data->nfields * sizeof(DB_CODEC *));
		ts->data_rc.types = ts->data_types;
		(void)strcpy(sp, schema->data->name);
		ts->data_rc.name = sp;
		sp += strlen(sp) + 1;
		ts->schema.data = &ts->data_rc;
	}
	if (schema->nconstraints > 0) {
		if ((ret = __os_calloc(env, schema->nconstraints,
		    sizeof(DB_CONSTRAINT), &ts->constraints)) != 0)
			return (ret);
		for (i = 0; i < schema->nconstraints; i++) {
			scon = &schema->constraints[i];
			ts->constraints[i] = *scon;
			(void)strcpy(sp, scon->name);
			ts->constraints[i].name = sp;
			sp += strlen(sp) + 1;
		}
		ts->schema.constraints = ts->constraints;
	}
	return (0);
}

/*
 * __typed_state --
 *	Return the TYPED_STATE for a handle, or NULL if untyped.  The hot-path
 *	readers (put check, callbacks) use this; it is just the indexed load.
 *	Declared in typed_int.h (not the generated typed_ext.h): it names
 *	TYPED_STATE, which C2/C3 keep out of any header s_sig scans and thus
 *	out of a header every engine TU includes.
 */
TYPED_STATE *
__typed_state(dbp)
	DB *dbp;
{
	u_int32_t slot;

	slot = DB2_TYPED_SLOT(dbp);
	if (slot == 0 || slot >= TYPED_NSLOTS)
		return (NULL);
	return (__typed_slot[slot]);
}

/*
 * __typed_claim --
 *	Install a state into a free slot with a lock-free CAS and return the
 *	slot number.  Scans from a rotating hint; retries on CAS failure.
 */
static int
__typed_claim(dbp, ts, slotp)
	DB *dbp;
	TYPED_STATE *ts;
	u_int32_t *slotp;
{
	u_int32_t i, start, slot;

	start = (atomic_read_relaxed(&__typed_hint_a) %
	    (TYPED_NSLOTS - 1)) + 1;
	for (i = 0; i < TYPED_NSLOTS - 1; i++) {
		slot = start + i;
		if (slot >= TYPED_NSLOTS)
			slot -= (TYPED_NSLOTS - 1);
		/*
		 * Acquire load (not a plain read): pairs with the claiming
		 * CAS below so skipping an occupied slot is not a C11 data
		 * race.  The CAS remains the authoritative claim; a stale
		 * value only costs this pass a retry.
		 */
		if (__os_atomic_read_ptr(
		    (void *volatile *)&__typed_slot[slot]) != NULL)
			continue;
		if (__os_atomic_cas_ptr(dbp->env,
		    (void *volatile *)&__typed_slot[slot], NULL, ts)) {
			/* benign hint; relaxed store clears the TSan race */
			atomic_init(&__typed_hint_a, (atomic_value_t)slot);
			*slotp = slot;
			return (0);
		}
	}
	__db_errx(dbp->env, DB_STR("5382",
	    "more than 4095 typed DB handles open in this process"));
	return (ENOMEM);
}

/*
 * __typed_release --
 *	Free a handle's state and release its slot.  Cold; called from
 *	__db_close.  The NULL-ing CAS cannot race a reader (ownership
 *	argument, §3.2), so a plain store would do; the CAS keeps the
 *	table's writes symmetric and asserts the slot held what we expect.
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: void __typed_release __P((DB *));
 * PUBLIC: #endif
 */
void
__typed_release(dbp)
	DB *dbp;
{
	TYPED_STATE *ts;
	u_int32_t slot;

	slot = DB2_TYPED_SLOT(dbp);
	DB_ASSERT(dbp->env, F2_ISSET(dbp, DB2_AM_TYPED));
	DB_ASSERT(dbp->env, slot != 0 && slot < TYPED_NSLOTS);
	if (slot == 0 || slot >= TYPED_NSLOTS)
		return;
	ts = __typed_slot[slot];
	DB_ASSERT(dbp->env, ts != NULL && ts->dbp == dbp);
	(void)__os_atomic_cas_ptr(dbp->env,
	    (void *volatile *)&__typed_slot[slot], ts, NULL);
	F2_CLR(dbp, DB2_AM_TYPED);
	dbp->flags2 &= ~(((u_int32_t)~0) << DB2_TYPED_SLOT_SHIFT);
	__typed_free_state(dbp, ts);
}

/*
 * __typed_validate_codec --
 *	Rule 5 of §3.4 for one DB_CODEC.
 */
static int
__typed_validate_codec(dbp, c)
	DB *dbp;
	const DB_CODEC *c;
{
	ENV *env;
	u_int32_t memcmp_ok;

	env = dbp->env;
	if (c->name == NULL || strnlen(c->name, 256) >= 256) {
		__db_errx(env, DB_STR("5383",
		    "typed schema: codec name is NULL or too long"));
		return (EINVAL);
	}
	if ((c->flags & ~(u_int32_t)(DB_CODEC_TOTAL | DB_CODEC_MEMCMP |
	    DB_CODEC_EQUALIMAGE | DB_CODEC_FIXED)) != 0) {
		__db_errx(env, DB_STR("5318",
		    "typed schema: codec %s has unknown flag bits"), c->name);
		return (EINVAL);
	}
	if (FLD_ISSET(c->flags, DB_CODEC_FIXED) && c->fixed_len == 0) {
		__db_errx(env, DB_STR("5319",
		    "typed schema: codec %s is FIXED but fixed_len is 0"),
		    c->name);
		return (EINVAL);
	}
	memcmp_ok = FLD_ISSET(c->flags, DB_CODEC_MEMCMP);
	if (c->compare == NULL && !memcmp_ok) {
		__db_errx(env, DB_STR("5320",
	    "typed schema: codec %s has no compare and is not MEMCMP"), c->name);
		return (EINVAL);
	}
	if (c->hash == NULL &&
	    !FLD_ISSET(c->flags, DB_CODEC_EQUALIMAGE) && !memcmp_ok) {
		__db_errx(env, DB_STR("5321",
	    "typed schema: codec %s has no hash and is not EQUALIMAGE"),
		    c->name);
		return (EINVAL);
	}
	if (c->nvectors > 256) {
		__db_errx(env, DB_STR("5322",
		    "typed schema: codec %s has more than 256 vectors"),
		    c->name);
		return (EINVAL);
	}
	if (c->nvectors < 2 && !memcmp_ok) {
		__db_errx(env, DB_STR("5323",
	    "typed schema: codec %s needs at least 2 vectors unless MEMCMP"),
		    c->name);
		return (EINVAL);
	}
	if ((c->nvectors > 0) != (c->vectors != NULL)) {
		__db_errx(env, DB_STR("5324",
		    "typed schema: codec %s vectors/nvectors disagree"),
		    c->name);
		return (EINVAL);
	}
	return (0);
}

/*
 * __typed_validate_rc --
 *	Rule 4 of §3.4 for one DB_RECORD_CODEC.
 */
static int
__typed_validate_rc(dbp, rc)
	DB *dbp;
	const DB_RECORD_CODEC *rc;
{
	ENV *env;
	u_int32_t i;
	int ret;

	env = dbp->env;
	if (rc->name == NULL || strnlen(rc->name, 256) >= 256) {
		__db_errx(env, DB_STR("5325",
		    "typed schema: record codec name is NULL or too long"));
		return (EINVAL);
	}
	if (rc->nfields < 1 || rc->nfields > 65535) {
		__db_errx(env, DB_STR("5326",
		    "typed schema: record codec %s has %lu fields (1..65535)"),
		    rc->name, (u_long)rc->nfields);
		return (EINVAL);
	}
	if (rc->field == NULL) {
		__db_errx(env, DB_STR("5327",
		    "typed schema: record codec %s has no field()"), rc->name);
		return (EINVAL);
	}
	if ((rc->flags & ~(u_int32_t)DB_CODEC_TOTAL) != 0) {
		__db_errx(env, DB_STR("5328",
	    "typed schema: record codec %s may set only DB_CODEC_TOTAL"),
		    rc->name);
		return (EINVAL);
	}
	if (rc->types == NULL) {
		__db_errx(env, DB_STR("5329",
		    "typed schema: record codec %s has no types[]"), rc->name);
		return (EINVAL);
	}
	for (i = 0; i < rc->nfields; i++) {
		if (rc->types[i] == NULL) {
			__db_errx(env, DB_STR("5330",
		    "typed schema: record codec %s field %lu codec is NULL"),
			    rc->name, (u_long)i);
			return (EINVAL);
		}
		if ((ret = __typed_validate_codec(dbp, rc->types[i])) != 0)
			return (ret);
	}
	return (0);
}

/*
 * __typed_collect_codecs --
 *	Build the distinct-codec list in §4.2 order (key fields, then data
 *	fields, first-use).  Enforces rule 5's "same name+version => same
 *	pointer".  Caller passes a buffer sized for the worst case
 *	(key->nfields + data->nfields).
 */
static int
__typed_collect_codecs(dbp, schema, out, noutp)
	DB *dbp;
	const DB_SCHEMA *schema;
	const DB_CODEC **out;
	u_int32_t *noutp;
{
	const DB_RECORD_CODEC *rc;
	const DB_CODEC *c;
	u_int32_t n, i, j, s;
	int found;

	n = 0;
	for (s = 0; s < 2; s++) {
		rc = (s == 0) ? schema->key : schema->data;
		if (rc == NULL)
			continue;
		for (i = 0; i < rc->nfields; i++) {
			c = rc->types[i];
			found = 0;
			for (j = 0; j < n; j++) {
				if (out[j] == c) {
					found = 1;
					break;
				}
				/* Same name+version must be the same pointer. */
				if (out[j]->version == c->version &&
				    strcmp(out[j]->name, c->name) == 0) {
					__db_errx(dbp->env, DB_STR("5331",
"typed schema: two different codecs share name %s version %lu"),
					    c->name, (u_long)c->version);
					return (EINVAL);
				}
			}
			if (!found)
				out[n++] = c;
		}
	}
	*noutp = n;
	return (0);
}

/*
 * __typed_validate --
 *	The §3.4 validation that does not need the collected codec list or the
 *	fingerprint.  Rules 1-8 (callback rules 7-8 for UNIQUE / overridden
 *	callbacks land with the UNIQUE and open commits; the parts testable
 *	before open are here).
 */
static int
__typed_validate(dbp, schema)
	DB *dbp;
	const DB_SCHEMA *schema;
{
	ENV *env;
	const DB_CONSTRAINT *con;
	const DB_RECORD_CODEC *rc;
	BTREE *bt;
	HASH *ht;
	u_int32_t i, k;
	int ret;

	env = dbp->env;

	/* Rule 1: flags, after-open, not already typed. */
	if (schema->flags != 0) {
		__db_errx(env, DB_STR("5332",
		    "typed schema: schema->flags must be 0 in Phase 1"));
		return (EINVAL);
	}
	if (F2_ISSET(dbp, DB2_AM_TYPED)) {
		__db_errx(env, DB_STR("5333",
		    "db_set_schema: a schema is already set on this handle"));
		return (EINVAL);
	}

	/* Rule 2: type is BTREE or HASH (D4). */
	if (schema->type != DB_BTREE && schema->type != DB_HASH) {
		__db_errx(env, DB_STR("5334",
	    "typed schema: type must be DB_BTREE or DB_HASH"));
		return (EINVAL);
	}

	/* Rule 3: at least one side typed. */
	if (schema->key == NULL && schema->data == NULL) {
		__db_errx(env, DB_STR("5335",
	    "typed schema: at least one of key, data must be non-NULL"));
		return (EINVAL);
	}

	/* Rules 4, 5: each record codec and its codecs. */
	if (schema->key != NULL &&
	    (ret = __typed_validate_rc(dbp, schema->key)) != 0)
		return (ret);
	if (schema->data != NULL &&
	    (ret = __typed_validate_rc(dbp, schema->data)) != 0)
		return (ret);

	/* Rule 6: constraints. */
	if (schema->nconstraints > 64) {
		__db_errx(env, DB_STR("5336",
		    "typed schema: more than 64 constraints"));
		return (EINVAL);
	}
	if ((schema->nconstraints > 0) != (schema->constraints != NULL)) {
		__db_errx(env, DB_STR("5337",
	    "typed schema: constraints/nconstraints disagree"));
		return (EINVAL);
	}
	for (i = 0; i < schema->nconstraints; i++) {
		con = &schema->constraints[i];
		if (con->kind < DB_CONSTRAINT_NOT_NULL ||
		    con->kind > DB_CONSTRAINT_UNIQUE) {
			__db_errx(env, DB_STR("5338",
			    "typed schema: constraint %lu has unknown kind"),
			    (u_long)i);
			return (EINVAL);
		}
		if (con->side == DB_TYPED_KEY)
			rc = schema->key;
		else if (con->side == DB_TYPED_DATA)
			rc = schema->data;
		else {
			__db_errx(env, DB_STR("5339",
		    "typed schema: constraint %lu has unknown side"), (u_long)i);
			return (EINVAL);
		}
		if (rc == NULL) {
			__db_errx(env, DB_STR("5340",
	    "typed schema: constraint %lu names a side with no record codec"),
			    (u_long)i);
			return (EINVAL);
		}
		if (con->field == DB_TYPED_WHOLE) {
			if (con->kind != DB_CONSTRAINT_MAXLEN) {
				__db_errx(env, DB_STR("5341",
	    "typed schema: constraint %lu uses DB_TYPED_WHOLE without MAXLEN"),
				    (u_long)i);
				return (EINVAL);
			}
		} else if (con->field >= rc->nfields) {
			__db_errx(env, DB_STR("5342",
		    "typed schema: constraint %lu field out of range"),
			    (u_long)i);
			return (EINVAL);
		}
		if ((con->flags & ~(u_int32_t)DB_CONSTRAINT_NULLS_NOT_DISTINCT)
		    != 0 || (FLD_ISSET(con->flags,
		    DB_CONSTRAINT_NULLS_NOT_DISTINCT) &&
		    con->kind != DB_CONSTRAINT_UNIQUE)) {
			__db_errx(env, DB_STR("5343",
		    "typed schema: constraint %lu has bad flags"), (u_long)i);
			return (EINVAL);
		}
		if (con->name == NULL || strnlen(con->name, 256) >= 256) {
			__db_errx(env, DB_STR("5344",
		    "typed schema: constraint %lu name is NULL or too long"),
			    (u_long)i);
			return (EINVAL);
		}
		for (k = 0; k < i; k++)
			if (strcmp(schema->constraints[k].name, con->name)
			    == 0) {
				__db_errx(env, DB_STR("5345",
			    "typed schema: duplicate constraint name %s"),
				    con->name);
				return (EINVAL);
			}

		/*
		 * Rule 7: UNIQUE (RFC 0014 §3.4 rule 7).  side must be data (a
		 * key is unique already); unique_db must be an open, non-DUP,
		 * Btree, same-env handle, not already a secondary; the field's
		 * codec must be EQUALIMAGE (or MEMCMP) or have a sortkey so a
		 * secondary key can express equality; no two UNIQUE constraints
		 * may share a unique_db.
		 */
		if (con->kind == DB_CONSTRAINT_UNIQUE) {
			DB *udb = con->unique_db;
			const DB_CODEC *fc;

			if (con->side != DB_TYPED_DATA) {
				__db_errx(env, DB_STR("5375",
		    "typed schema: UNIQUE constraint %s must be on the data side"),
				    con->name);
				return (EINVAL);
			}
			if (udb == NULL ||
			    !F_ISSET(udb, DB_AM_OPEN_CALLED) ||
			    udb->type != DB_BTREE ||
			    F_ISSET(udb, DB_AM_DUP) ||
			    udb->env != env || udb->s_primary != NULL) {
				__db_errx(env, DB_STR("5376",
		    "typed schema: UNIQUE %s needs an open non-DUP Btree in the same env, not already a secondary"),
				    con->name);
				return (EINVAL);
			}
			fc = rc->types[con->field];
			if (!FLD_ISSET(fc->flags,
			    DB_CODEC_EQUALIMAGE | DB_CODEC_MEMCMP) &&
			    fc->sortkey == NULL) {
				__db_errx(env, DB_STR("5377",
		    "typed schema: UNIQUE %s needs an EQUALIMAGE/MEMCMP codec or a sortkey"),
				    con->name);
				return (EINVAL);
			}
			for (k = 0; k < i; k++)
				if (schema->constraints[k].kind ==
				    DB_CONSTRAINT_UNIQUE &&
				    schema->constraints[k].unique_db == udb) {
					__db_errx(env, DB_STR("5378",
		    "typed schema: two UNIQUE constraints share a unique_db (%s)"),
					    con->name);
					return (EINVAL);
				}
		}
	}

	/*
	 * Rule 8: the handle has no application callback already set.  The
	 * defaults are __bam_defcmp / __bam_defpfx for Btree and NULL h_hash /
	 * h_compare for Hash; a set_bt_compare before db_set_schema nulls
	 * bt_prefix, so test both.  UNIQUE / callback-identity-after-open are
	 * commits 7 and 4.
	 */
	if (F_ISSET(dbp, DB_AM_DUP)) {
		__db_errx(env, DB_STR("5346",
	    "typed schema: typed databases may not use DB_DUP (Phase 1, C12)"));
		return (EINVAL);
	}
	if (dbp->dup_compare != NULL) {
		__db_errx(env, DB_STR("5347",
	    "typed schema: a dup_compare is already set"));
		return (EINVAL);
	}
	if (schema->type == DB_BTREE && dbp->bt_internal != NULL) {
		bt = dbp->bt_internal;
		if (bt->bt_compare != __bam_defcmp ||
		    bt->bt_prefix != __bam_defpfx) {
			__db_errx(env, DB_STR("5348",
		    "typed schema: a bt_compare/bt_prefix is already set"));
			return (EINVAL);
		}
	}
	if (schema->type == DB_HASH && dbp->h_internal != NULL) {
		ht = dbp->h_internal;
		if (ht->h_hash != NULL || ht->h_compare != NULL) {
			__db_errx(env, DB_STR("5349",
		    "typed schema: an h_hash/h_compare is already set"));
			return (EINVAL);
		}
	}
	return (0);
}

/*
 * __typed_build_state --
 *	Allocate and deep-copy the schema into a TYPED_STATE, compute the
 *	per-codec fingerprints (§5.1), and fill the derived flags.  On any
 *	error frees everything and returns.  Does not touch the side table.
 */
static int
__typed_build_state(dbp, schema, tsp)
	DB *dbp;
	const DB_SCHEMA *schema;
	TYPED_STATE **tsp;
{
	ENV *env;
	TYPED_STATE *ts;
	const DB_CODEC **codecs;
	const DB_CONSTRAINT *con;
	u_int32_t maxcodecs, i;
	int ret;

	env = dbp->env;
	*tsp = NULL;

	maxcodecs = 0;
	if (schema->key != NULL)
		maxcodecs += schema->key->nfields;
	if (schema->data != NULL)
		maxcodecs += schema->data->nfields;

	if ((ret = __os_calloc(env, 1, sizeof(TYPED_STATE), &ts)) != 0)
		return (ret);
	ts->dbp = dbp;

	if ((ret = __os_calloc(env, maxcodecs == 0 ? 1 : maxcodecs,
	    sizeof(DB_CODEC *), &codecs)) != 0) {
		__os_free(env, ts);
		return (ret);
	}
	ts->codecs = codecs;
	if ((ret = __typed_collect_codecs(dbp, schema, codecs,
	    &ts->ncodecs)) != 0)
		goto err;

	if (ts->ncodecs > 0 &&
	    (ret = __os_calloc(env, ts->ncodecs, 20, &ts->fp)) != 0)
		goto err;

	/* Deep-copy the schema (one copy, pointers kept into the state). */
	if ((ret = __typed_copy_schema(dbp, schema, ts)) != 0)
		goto err;

	/* Derived key-side flags, used by the comparator and open gate. */
	ts->key_memcmp = ts->key_equalimage = 0;
	if (schema->key != NULL) {
		ts->key_memcmp = ts->key_equalimage = 1;
		for (i = 0; i < schema->key->nfields; i++) {
			if (!FLD_ISSET(schema->key->types[i]->flags,
			    DB_CODEC_MEMCMP))
				ts->key_memcmp = 0;
			/* C11: MEMCMP implies EQUALIMAGE. */
			if (!FLD_ISSET(schema->key->types[i]->flags,
			    DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE))
				ts->key_equalimage = 0;
		}
	}

	/* Count UNIQUE constraints (§3.7 step 2). */
	ts->nunique = 0;
	for (i = 0; i < schema->nconstraints; i++) {
		con = &schema->constraints[i];
		if (con->kind == DB_CONSTRAINT_UNIQUE)
			ts->nunique++;
	}

	/* Fingerprint each distinct codec and run the §5.1 self-checks. */
	for (i = 0; i < ts->ncodecs; i++)
		if ((ret = __typed_fingerprint(dbp, ts->codecs[i],
		    ts->fp[i])) != 0)
			goto err;

	*tsp = ts;
	return (0);

err:	__typed_free_state(dbp, ts);
	return (ret);
}

/*
 * db_set_schema --
 *	Attach a schema to a handle before DB->open (spec §1.5, §3.3).  Deep-
 *	copies the schema, validates every §3.4 rule, computes the fingerprint
 *	(§5), claims a side-table slot and sets DB2_AM_TYPED.  Callback
 *	installation and the open-time checks are later commits.
 */
int
db_set_schema(dbp, schema, flags)
	DB *dbp;
	const DB_SCHEMA *schema;
	u_int32_t flags;
{
	TYPED_STATE *ts;
	u_int32_t slot;
	int ret;

	if (flags != 0) {
		__db_errx(dbp->env, DB_STR("5350",
		    "db_set_schema: flags must be 0"));
		return (EINVAL);
	}
	DB_ILLEGAL_AFTER_OPEN(dbp, "db_set_schema");
	if (schema == NULL) {
		__db_errx(dbp->env, DB_STR("5351",
		    "db_set_schema: schema is NULL"));
		return (EINVAL);
	}

	if ((ret = __typed_validate(dbp, schema)) != 0)
		return (ret);
	if ((ret = __typed_build_state(dbp, schema, &ts)) != 0)
		return (ret);

	/*
	 * Restrict the handle to the schema's access method (rule 2): a later
	 * DB->open with another type must fail DB_ILLEGAL_METHOD.
	 */
	FLD_CLR(dbp->am_ok,
	    ~(u_int32_t)(schema->type == DB_BTREE ? DB_OK_BTREE : DB_OK_HASH));

	if ((ret = __typed_claim(dbp, ts, &slot)) != 0) {
		__typed_free_state(dbp, ts);
		return (ret);
	}

	/*
	 * Store the slot in flags2 bits 16-31 and set DB2_AM_TYPED.  Every
	 * other write to flags2 is before open or in recovery (§1.4), so this
	 * races with nothing; the CAS in __typed_claim published the state's
	 * contents before its pointer.
	 */
	dbp->flags2 = (dbp->flags2 & ((1U << DB2_TYPED_SLOT_SHIFT) - 1)) |
	    (slot << DB2_TYPED_SLOT_SHIFT);
	F2_SET(dbp, DB2_AM_TYPED);

	return (0);
}

/*
 * db_get_schema --
 *	Return the libdb-owned deep copy (spec §1.5).  EINVAL if the handle has
 *	no schema.  The returned schema and everything it points at (names,
 *	types[], constraints) is the copy, so scribbling the caller's original
 *	arrays after db_set_schema cannot change it.
 */
int
db_get_schema(dbp, schemap)
	DB *dbp;
	const DB_SCHEMA **schemap;
{
	TYPED_STATE *ts;

	if (schemap == NULL)
		return (EINVAL);
	if (!F2_ISSET(dbp, DB2_AM_TYPED) ||
	    (ts = __typed_state(dbp)) == NULL) {
		__db_errx(dbp->env, DB_STR("5352",
		    "db_get_schema: no schema is set on this handle"));
		return (EINVAL);
	}
	*schemap = &ts->schema;
	return (0);
}

/*
 * __typed_open --
 *	The DB->open hook for typed databases (spec §3.5).  Runs after the
 *	access-method and partition opens succeed and before DB_TEST_POSTOPEN,
 *	but not for recovery handles (DB_AM_RECOVER, checked by the caller).
 *	In commit 4 it enforces the open-time Phase 1 refusals: compression,
 *	DUP carried by an existing file, and a callback overridden after
 *	db_set_schema.  The catalog/fingerprint check (§5.2), the read-only
 *	schema-less rule (§4.1) and the UNIQUE associate (§3.6) are later
 *	commits.
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_open __P((DB *, DB_THREAD_INFO *, DB_TXN *,
 * PUBLIC:     const char *, const char *, u_int32_t));
 * PUBLIC: #endif
 */
int
__typed_open(dbp, ip, txn, fname, dname, flags)
	DB *dbp;
	DB_THREAD_INFO *ip;
	DB_TXN *txn;
	const char *fname, *dname;
	u_int32_t flags;
{
	ENV *env;
	TYPED_STATE *ts;
	BTREE *bt;
	HASH *ht;
	const DB_CONSTRAINT *con;
	u_int32_t i;
	int ret;

	env = dbp->env;

	ts = __typed_state(dbp);

	/*
	 * Compression (set_bt_compress or a BTM_COMPRESS file) is refused:
	 * a compressed value's bytes are not the record the codec reads.  Only
	 * a post-AM-open check sees both sources (bt_open.c sets DB_AM_COMPRESS
	 * from the file).
	 */
#ifdef HAVE_COMPRESSION
	if (dbp->type == DB_BTREE && dbp->bt_internal != NULL &&
	    DB_IS_COMPRESSED(dbp)) {
		__db_errx(env, DB_STR("5359",
	    "typed database: compression is not supported in Phase 1"));
		return (EINVAL);
	}
#endif

	/*
	 * DB_DUP / DB_DUPSORT (set after db_set_schema, or carried by the file
	 * through __bam_metachk / __ham_metachk) is refused (C12): a duplicate
	 * set makes "the record" of a put ambiguous.
	 */
	if (F_ISSET(dbp, DB_AM_DUP)) {
		__db_errx(env, DB_STR("5360",
	    "typed database: DB_DUP is not supported in Phase 1 (C12)"));
		return (EINVAL);
	}

	/*
	 * A callback overridden after db_set_schema is refused: set_bt_compare
	 * and friends are existing methods that cannot be intercepted.  Commit
	 * 3 installs no generated callback, so a typed handle must still carry
	 * the defaults (Btree: __bam_defcmp / __bam_defpfx; Hash: NULL h_hash
	 * before open-time filling, NULL h_compare).  Commit 8 compares against
	 * the generated callbacks instead.
	 */
	if (ts != NULL && dbp->type == DB_BTREE && dbp->bt_internal != NULL) {
		bt = dbp->bt_internal;
		if (bt->bt_compare != __bam_defcmp ||
		    bt->bt_prefix != __bam_defpfx) {
			__db_errx(env, DB_STR("5361",
		    "typed database: a comparator was set after db_set_schema"));
			return (EINVAL);
		}
	}
	if (ts != NULL && dbp->type == DB_HASH && dbp->h_internal != NULL) {
		ht = dbp->h_internal;
		/*
		 * __ham_open fills a NULL h_hash with __ham_func5 at open, so by
		 * the time this runs h_hash is non-NULL; only an application
		 * h_compare (which open never sets) betrays an override here.
		 */
		if (ht->h_compare != NULL) {
			__db_errx(env, DB_STR("5362",
		    "typed database: an h_compare was set after db_set_schema"));
			return (EINVAL);
		}
	}

	/*
	 * DB_OPTREAD gate (spec §6, D7).  The optimistic Btree read path calls
	 * bt_compare on unvalidated interior-page bytes, so a non-MEMCMP typed
	 * key's generated comparator must be total over arbitrary bytes -- it
	 * calls the record codec's field() and each type codec on torn bytes.
	 * Refuse the open if DB_OPTREAD is on and any key-side codec lacks
	 * DB_CODEC_TOTAL.  Data-side codecs run only on the DBT being put, never
	 * on page bytes, so they are not gated; MEMCMP keys use the bounded
	 * default comparator, so they are not gated either.  Checked per open
	 * (the setting is a process env var, cached in __bam_opt_enabled).
	 */
	if (ts != NULL && dbp->type == DB_BTREE && ts->schema.key != NULL &&
	    !ts->key_memcmp && __bam_optread_on()) {
		if (!FLD_ISSET(ts->schema.key->flags, DB_CODEC_TOTAL)) {
			__db_errx(env, DB_STR("5380",
	    "DB_OPTREAD is set and record codec %s is not DB_CODEC_TOTAL"),
			    ts->schema.key->name);
			return (EINVAL);
		}
		for (i = 0; i < ts->schema.key->nfields; i++)
			if (!FLD_ISSET(ts->schema.key->types[i]->flags,
			    DB_CODEC_TOTAL)) {
				__db_errx(env, DB_STR("5381",
		    "DB_OPTREAD is set and codec %s is not DB_CODEC_TOTAL"),
				    ts->schema.key->types[i]->name);
				return (EINVAL);
			}
	}

	/*
	 * Install the generated callbacks (spec §3.6).  Done here, not through
	 * db_set_schema's setters: bt_internal/h_internal may not exist at
	 * db_set_schema time (they are allocated at open), and the setters
	 * refuse post-open with DB_ILLEGAL_AFTER_OPEN.  The identity checks
	 * above proved the slots still hold the defaults, so writing them
	 * directly is exactly what set_bt_compare/set_h_compare would do, minus
	 * the open guard they cannot pass here.  A MEMCMP key (Btree) or an
	 * all-EQUALIMAGE key (Hash) keeps the default byte path and installs
	 * nothing, so get_bt_compare still returns __bam_defcmp (tested).
	 */
	if (ts != NULL && dbp->type == DB_BTREE && ts->schema.key != NULL &&
	    !ts->key_memcmp) {
		bt = dbp->bt_internal;
		bt->bt_compare = __typed_bt_compare;
		bt->bt_prefix = NULL;	/* a custom comparator cannot prefix. */
	}
	if (ts != NULL && dbp->type == DB_HASH && ts->schema.key != NULL &&
	    !ts->key_equalimage) {
		ht = dbp->h_internal;
		ht->h_hash = __typed_h_hash;
		ht->h_compare = __typed_h_compare;
	}

	/*
	 * Schema-less open of a typed FILE (DB2_AM_TYPED_FILE but no schema):
	 * allowed only read-only, or for the internal DB_WRITEOPEN probe that
	 * __db_subdb_remove uses (§4.1 row 3).  This gives D8's read-only
	 * db_dump/db_stat.  Such an open installs no codec and writes nothing,
	 * so it cannot misorder anything; the catalog is not checked.
	 */
	if (ts == NULL) {
		if (!F_ISSET(dbp, DB_AM_RDONLY) && !LF_ISSET(DB_WRITEOPEN)) {
			__db_errx(env, DB_STR("5370",
	    "typed database: db_set_schema required to open for writing"));
			return (EINVAL);
		}
		return (0);
	}

	/*
	 * Schema given but the subdatabase was not created typed (no meta bit):
	 * Phase 1 does not convert (§4.1 row 4).
	 */
	if (!F_ISSET(dbp, DB_AM_CREATED) &&
	    !F2_ISSET(dbp, DB2_AM_TYPED_FILE)) {
		__db_errx(env, DB_STR("5371",
	    "typed database %s: database was not created typed"),
		    dname == NULL ? "" : dname);
		return (EINVAL);
	}

	/*
	 * Catalog (§4.3, §5.2): write the record when the subdatabase was just
	 * created, else parse it and compare the schema and fingerprints.  Both
	 * in the caller's txn, on a short-lived internal __db_typed handle.
	 */
	if (F_ISSET(dbp, DB_AM_CREATED)) {
		if ((ret = __typed_catalog_write(dbp, ip, txn, fname, dname,
		    ts)) != 0)
			return (ret);
	} else if ((ret = __typed_catalog_check(dbp, ip, txn, fname, dname,
	    ts)) != 0)
		return (ret);

	/*
	 * Associate each UNIQUE constraint's unique_db to this primary with
	 * __typed_unique_cb and no flags (RFC 0014 §3.6): no DB_CREATE populate,
	 * because a non-empty primary with an empty unique index is a catalog
	 * mismatch (§4).  A read-only or schema-less open never reaches here.
	 */
	if (ts->nunique > 0) {
		for (i = 0; i < ts->schema.nconstraints; i++) {
			con = &ts->schema.constraints[i];
			if (con->kind != DB_CONSTRAINT_UNIQUE)
				continue;
			if ((ret = __db_associate(dbp, ip, txn,
			    con->unique_db, __typed_unique_cb, 0)) != 0)
				return (ret);
		}
	}
	return (0);
}

/*
 * __typed_check_side --
 *	Run the record-codec validate (if VALID is asked) and every constraint
 *	on one side (key or data) of a put (spec §3.7 steps 3-5).  A codec
 *	DB_CODEC_MALFORMED becomes DB_CONSTRAINT_VIOLATION; any other non-zero
 *	callback return is passed through.
 */
static int
__typed_check_side(dbc, ts, side, rc, dbt)
	DBC *dbc;
	TYPED_STATE *ts;
	u_int32_t side;
	const DB_RECORD_CODEC *rc;
	const DBT *dbt;
{
	ENV *env;
	const DB_CONSTRAINT *con;
	const DB_CODEC *codec;
	const u_int8_t *p;
	u_int32_t i, flen;
	int isnull, ret;

	env = dbc->dbp->env;

	for (i = 0; i < ts->schema.nconstraints; i++) {
		con = &ts->schema.constraints[i];
		if (con->side != side)
			continue;

		/* MAXLEN on the whole DBT needs no field(). */
		if (con->kind == DB_CONSTRAINT_MAXLEN &&
		    con->field == DB_TYPED_WHOLE) {
			if (dbt->size > con->maxlen)
				goto violated;
			continue;
		}

		/* UNIQUE is enforced by the generated secondary, not here. */
		if (con->kind == DB_CONSTRAINT_UNIQUE)
			continue;

		/* VALID over the whole record is handled once below. */
		if (con->kind == DB_CONSTRAINT_VALID &&
		    con->field == DB_TYPED_WHOLE)
			continue;

		/* Locate the field. */
		p = NULL; flen = 0; isnull = 0;
		if ((ret = rc->field(rc, dbt, con->field, &p, &flen,
		    &isnull)) != 0) {
			if (ret == DB_CODEC_MALFORMED)
				goto violated;
			return (ret);
		}

		switch (con->kind) {
		case DB_CONSTRAINT_NOT_NULL:
			if (isnull)
				goto violated;
			break;
		case DB_CONSTRAINT_MAXLEN:
			if (flen > con->maxlen)
				goto violated;
			break;
		case DB_CONSTRAINT_VALID:
			codec = rc->types[con->field];
			if (!isnull && codec->validate != NULL &&
			    (ret = codec->validate(codec, p, flen)) != 0) {
				if (ret == DB_CODEC_MALFORMED)
					goto violated;
				return (ret);
			}
			break;
		}
		continue;

violated:
		__db_errx(env, DB_STR("5372",
		    "typed database: constraint %s violated"), con->name);
		return (DB_CONSTRAINT_VIOLATION);
	}

	/*
	 * Record-codec validate: run it if this side has any VALID constraint
	 * (whole-record or a field), per §3.7 step 5 ("VALID => the record
	 * codec's validate and every field's codec validate return 0").
	 */
	if (rc->validate != NULL) {
		for (i = 0; i < ts->schema.nconstraints; i++) {
			con = &ts->schema.constraints[i];
			if (con->side == side &&
			    con->kind == DB_CONSTRAINT_VALID) {
				if ((ret = rc->validate(rc, dbt)) != 0) {
					if (ret == DB_CODEC_MALFORMED) {
						__db_errx(env, DB_STR("5373",
			    "typed database: record failed VALID (%s)"),
						    con->name);
						return (DB_CONSTRAINT_VIOLATION);
					}
					return (ret);
				}
				break;
			}
		}
	}
	return (0);
}

/*
 * __typed_put_check --
 *	The put-path constraint hook (spec §3.7), called from __dbc_put for a
 *	typed handle.  Refuses partial puts, then runs the key- and data-side
 *	constraints.  A DB_CURRENT put keeps the stored key, so the key side
 *	is skipped for it.
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_put_check __P((DBC *, DBT *, DBT *, u_int32_t));
 * PUBLIC: #endif
 */
int
__typed_put_check(dbc, key, data, flags)
	DBC *dbc;
	DBT *key, *data;
	u_int32_t flags;
{
	DB *dbp;
	TYPED_STATE *ts;
	u_int32_t nfound;
	int ret;
	DB *sdbp;

	dbp = dbc->dbp;
	if ((ts = __typed_state(dbp)) == NULL)
		return (0);	/* DIAGNOSTIC: flag set but no state. */
	DB_ASSERT(dbp->env, F2_ISSET(dbp, DB2_AM_TYPED));

	/* Step 1: partial puts are not supported on typed databases. */
	if ((key != NULL && F_ISSET(key, DB_DBT_PARTIAL)) ||
	    (data != NULL && F_ISSET(data, DB_DBT_PARTIAL))) {
		__db_errx(dbp->env, DB_STR("5374",
	    "partial puts are not supported on typed databases"));
		return (EINVAL);
	}

	/*
	 * Step 2: every UNIQUE index must still be associated (RFC 0014 §3.7).
	 * Closing a unique_db just unlinks it from s_secondaries, which would
	 * silently stop enforcing UNIQUE; count the associated __typed_unique_cb
	 * secondaries under the handle's own mutex (per handle, MUTEX_INVALID on
	 * a non-DB_THREAD handle) and refuse the put if any is gone.
	 * ponytail: list walk of <= 64 entries per put, per-handle mutex only;
	 * cache a generation number if it ever shows in a profile.
	 */
	if (ts->nunique > 0) {
		nfound = 0;
		MUTEX_LOCK(dbp->env, dbp->mutex);
		for (sdbp = LIST_FIRST(&dbp->s_secondaries); sdbp != NULL;
		    sdbp = LIST_NEXT(sdbp, s_links))
			if (sdbp->s_callback == __typed_unique_cb)
				++nfound;
		MUTEX_UNLOCK(dbp->env, dbp->mutex);
		if (nfound < ts->nunique) {
			__db_errx(dbp->env, DB_STR("5379",
	    "typed database: a UNIQUE index was closed before its primary"));
			return (EINVAL);
		}
	}

	/*
	 * Step 3: key side, unless DB_CURRENT (which keeps the stored key,
	 * already checked when it was written).
	 */
	if (ts->schema.key != NULL && flags != DB_CURRENT && key != NULL &&
	    (ret = __typed_check_side(dbc, ts, DB_TYPED_KEY,
	    ts->schema.key, key)) != 0)
		return (ret);

	/* Step 4: data side. */
	if (ts->schema.data != NULL && data != NULL &&
	    (ret = __typed_check_side(dbc, ts, DB_TYPED_DATA,
	    ts->schema.data, data)) != 0)
		return (ret);

	return (0);
}
#endif /* HAVE_TYPED */
