/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_gen.c --
 *	RFC 0014 typed data and constraints: the generated bt_compare /
 *	h_hash / h_compare / associate callbacks, and db_record_fixed.
 *
 * Whole-file guarded on HAVE_TYPED (--enable-typed / meson -Dtyped=enabled),
 * the os_aio_uring.c pattern: the file is in every build's source list and
 * compiles to nothing unless the option is on.
 */

#include "db_config.h"

#include "db_int.h"
#include "typed_int.h"

#ifdef HAVE_TYPED
/*
 * db_record_fixed_field --
 *	The field() callback db_record_fixed installs: field i is at
 *	(rec + off[i], len[i]).  Total and bounded by rec->size; a slice that
 *	runs off the end makes the record malformed.  off[]/len[] are the two
 *	app-owned arrays stashed after the DB_RECORD_CODEC via app_private
 *	(see db_record_fixed); isnull is always 0 for a fixed layout.
 */
struct __fixed_layout {
	const u_int32_t *off;
	const u_int32_t *len;
};

static int
db_record_fixed_field(rc, rec, i, pp, lenp, isnull)
	const DB_RECORD_CODEC *rc;
	const DBT *rec;
	u_int32_t i;
	const u_int8_t **pp;
	u_int32_t *lenp;
	int *isnull;
{
	const struct __fixed_layout *lay;
	u_int32_t off, len;

	lay = rc->app_private;
	if (i >= rc->nfields)
		return (DB_CODEC_MALFORMED);
	off = lay->off[i];
	len = lay->len[i];
	/* Overflow-safe: len > size || off > size - len. */
	if (len > rec->size || off > rec->size - len)
		return (DB_CODEC_MALFORMED);
	*pp = (const u_int8_t *)rec->data + off;
	*lenp = len;
	*isnull = 0;
	return (0);
}

/*
 * db_record_fixed --
 *	Fill an application-owned DB_RECORD_CODEC describing a fixed layout
 *	(spec §1.5): field i is (rec + off[i], len[i]).  off[]/len[]/types[]
 *	are referenced, not copied (same lifetime contract as any callback's
 *	data).  app_private holds a small heap block carrying off/len to
 *	field(); it is owned by the codec and freed by the application, like
 *	any app_private.
 */
int
db_record_fixed(rc, name, version, nfields, off, len, types)
	DB_RECORD_CODEC *rc;
	const char *name;
	u_int32_t version, nfields;
	const u_int32_t *off, *len;
	const DB_CODEC **types;
{
	struct __fixed_layout *lay;

	if (rc == NULL || name == NULL || off == NULL || len == NULL ||
	    types == NULL || nfields < 1 || nfields > 65535)
		return (EINVAL);

	/*
	 * app_private carries off[]/len[] to field().  __os_malloc(NULL)
	 * uses the global allocator; the block references the caller's off/len
	 * arrays (not copied), so both must outlive the codec, same contract
	 * as types[].
	 */
	if (__os_malloc(NULL, sizeof(*lay), &lay) != 0)
		return (ENOMEM);
	lay->off = off;
	lay->len = len;

	memset(rc, 0, sizeof(*rc));
	rc->name = name;
	rc->version = version;
	rc->flags = DB_CODEC_TOTAL;
	rc->nfields = nfields;
	rc->field = db_record_fixed_field;
	rc->validate = NULL;
	rc->types = types;
	rc->app_private = lay;
	return (0);
}
#endif /* HAVE_TYPED */
