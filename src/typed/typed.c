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
#endif /* HAVE_TYPED */
