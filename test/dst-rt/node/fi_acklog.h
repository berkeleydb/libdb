/*
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * test/dst-rt/node/fi_acklog.h -- the client-side ACK log writer (RFC 0024
 * §2.7), shared by rtload and rtnode's HA mode.
 *
 * The log records every operation the client believes it committed.  The
 * oracle (rtcheck) later compares it against the surviving database.  Two
 * properties matter:
 *
 *   1. The line is on stable storage BEFORE the op is reported done, so a
 *      crash cannot hide an op the client already treated as acked.  We
 *      O_APPEND-write the whole line in one call and fdatasync.
 *
 *   2. The write must NOT be seen by libfiu's POSIX I/O fault plans.  libfiu's
 *      preload wraps write(2)/fdatasync(2) by symbol and has no path filter,
 *      so we bypass the wrapper with a raw syscall(SYS_write)/syscall(
 *      SYS_fdatasync) on the ack-log fd.  (RFC 0024 §2.7 offers this or an
 *      fd-aware failinfo; the raw-syscall path is the simpler, and we take it.)
 *      The ack log also lives OUTSIDE every env home, so a path-filtered plan
 *      would exclude it too, but the raw syscall makes it robust to the
 *      unfiltered preload.
 */
#ifndef FI_ACKLOG_H
#define FI_ACKLOG_H

#include <sys/syscall.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <errno.h>

/* 64-bit FNV-1a of a value, as the ack log's vhash (RFC 0024 §2.7). */
static uint64_t
fi_fnv1a(const void *buf, size_t len)
{
	const unsigned char *p = (const unsigned char *)buf;
	uint64_t h = 1469598103934665603ULL;	/* FNV offset basis */
	size_t i;
	for (i = 0; i < len; i++) {
		h ^= (uint64_t)p[i];
		h *= 1099511628211ULL;		/* FNV prime */
	}
	return (h);
}

typedef struct {
	int fd;
} fi_acklog;

/* fi_acklog_open -- O_APPEND the ack log at PATH (outside every env home). */
static int
fi_acklog_open(fi_acklog *al, const char *path)
{
	al->fd = open(path, O_WRONLY | O_CREAT | O_APPEND, 0644);
	return (al->fd < 0 ? errno : 0);
}

/* fi_acklog_raw_write -- one atomic, wrapper-bypassing append + durable sync.
 * syscall(SYS_write) and syscall(SYS_fdatasync) are invisible to libfiu's
 * symbol-level POSIX wrappers, so an io-* plan never fails the oracle's own
 * log write. */
static int
fi_acklog_raw_write(fi_acklog *al, const char *line, size_t len)
{
	ssize_t n;
	size_t off = 0;
	while (off < len) {
		n = syscall(SYS_write, al->fd, line + off, len - off);
		if (n < 0) {
			if (errno == EINTR)
				continue;
			return (errno);
		}
		off += (size_t)n;
	}
#ifdef SYS_fdatasync
	(void)syscall(SYS_fdatasync, al->fd);
#else
	(void)syscall(SYS_fsync, al->fd);
#endif
	return (0);
}

/* fi_acklog_printf -- format a line and raw-append it.  The caller supplies
 * the full RFC 0024 §2.7 line (no trailing newline; we add it). */
static int
fi_acklog_printf(fi_acklog *al, const char *fmt, ...)
{
	char buf[512];
	int n;
	va_list ap;
	va_start(ap, fmt);
	n = vsnprintf(buf, sizeof(buf) - 1, fmt, ap);
	va_end(ap);
	if (n < 0)
		return (EINVAL);
	if ((size_t)n >= sizeof(buf) - 1)
		n = (int)sizeof(buf) - 2;
	buf[n++] = '\n';
	return (fi_acklog_raw_write(al, buf, (size_t)n));
}

static void
fi_acklog_close(fi_acklog *al)
{
	if (al->fd >= 0) {
		(void)close(al->fd);
		al->fd = -1;
	}
}

#endif /* FI_ACKLOG_H */
