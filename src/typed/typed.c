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

#ifdef HAVE_TYPED
/*
 * Public API prototypes.  dist/s_include scans this file for the EXTERN
 * markers below (it is preprocessor-blind, so the HAVE_TYPED guard does not
 * hide them) and generates src/dbinc_auto/typed_ext.h plus the ext_prot.in /
 * ext_def.in entries.  The functions themselves land in commit 3 (spec §3);
 * until then a default build simply lacks the symbols and a caller fails at
 * link time (fail-closed, §1.1).  The DB_SCHEMA / DB_RECORD_CODEC / DB_CODEC
 * types are declared unconditionally in db.h, so these prototypes are valid in
 * every build.
 *
 * EXTERN: int db_set_schema __P((DB *, const DB_SCHEMA *, u_int32_t));
 *
 * EXTERN: int db_get_schema __P((DB *, const DB_SCHEMA **));
 *
 * EXTERN: int db_record_fixed __P((DB_RECORD_CODEC *, const char *, u_int32_t,
 * EXTERN:     u_int32_t, const u_int32_t *, const u_int32_t *,
 * EXTERN:     const DB_CODEC **));
 */
#endif /* HAVE_TYPED */
