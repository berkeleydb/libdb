/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * wirecap.c -- an LD_PRELOAD shim that records every byte handed to the
 * socket send primitives (send/sendto/write/writev) into a capture file named
 * by $WIRECAP_FILE.  Used by run_repmgr_tls_wirecap.sh to prove that a marker
 * value crosses the replication wire in cleartext WITHOUT TLS and does NOT
 * appear WITH TLS.
 *
 * We log unconditionally (all fds): the capture is grepped for a unique marker
 * string, so unrelated traffic is harmless.  We never alter the data.
 */
#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

static ssize_t (*real_send)(int, const void *, size_t, int);
static ssize_t (*real_sendto)(int, const void *, size_t, int,
    const struct sockaddr *, socklen_t);
static ssize_t (*real_write)(int, const void *, size_t);
static ssize_t (*real_writev)(int, const struct iovec *, int);
static FILE *capf;

static void
wirecap_init(void)
{
	const char *path;

	if (real_send == NULL)
		real_send = dlsym(RTLD_NEXT, "send");
	if (real_sendto == NULL)
		real_sendto = dlsym(RTLD_NEXT, "sendto");
	if (real_write == NULL)
		real_write = dlsym(RTLD_NEXT, "write");
	if (real_writev == NULL)
		real_writev = dlsym(RTLD_NEXT, "writev");
	if (capf == NULL && (path = getenv("WIRECAP_FILE")) != NULL)
		capf = fopen(path, "ab");
}

static void
wirecap_record(const void *buf, size_t len)
{
	if (capf != NULL && len > 0) {
		(void)fwrite(buf, 1, len, capf);
		(void)fflush(capf);
	}
}

ssize_t
send(int fd, const void *buf, size_t len, int flags)
{
	wirecap_init();
	wirecap_record(buf, len);
	return (real_send(fd, buf, len, flags));
}

ssize_t
sendto(int fd, const void *buf, size_t len, int flags,
    const struct sockaddr *to, socklen_t tolen)
{
	wirecap_init();
	wirecap_record(buf, len);
	return (real_sendto(fd, buf, len, flags, to, tolen));
}

ssize_t
write(int fd, const void *buf, size_t len)
{
	wirecap_init();
	/*
	 * write() is used for files and the signalling pipe too; recording
	 * those is harmless because the capture is grepped for a unique
	 * marker.  Only socket writes can carry replicated data.
	 */
	wirecap_record(buf, len);
	return (real_write(fd, buf, len));
}

ssize_t
writev(int fd, const struct iovec *iov, int iovcnt)
{
	int i;

	wirecap_init();
	for (i = 0; i < iovcnt; i++)
		wirecap_record(iov[i].iov_base, iov[i].iov_len);
	return (real_writev(fd, iov, iovcnt));
}
