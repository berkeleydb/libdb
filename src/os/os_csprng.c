/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 The libdb contributors.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

/*
 * __os_csprng --
 *	Fill a buffer with cryptographically strong random bytes from the
 *	operating system.
 *
 *	This is the entropy source for security-sensitive values (currently
 *	encryption initialization vectors).  It is deliberately separate from
 *	the Mersenne Twister in src/crypto/mersenne: that generator is fine for
 *	sequence quality but is not a CSPRNG and was historically seeded from
 *	hashed wall-clock seconds, which is guessable.
 *
 *	Returns 0 on success, or a non-zero error if no OS entropy source is
 *	available; callers MUST handle failure rather than silently falling back
 *	to a weak source.
 *
 * PUBLIC: int __os_csprng __P((ENV *, void *, size_t));
 */
int
__os_csprng(env, buf, len)
	ENV *env;
	void *buf;
	size_t len;
{
	u_int8_t *p;
	size_t need;
	ssize_t n;
	int fd, ret;

	p = buf;
	need = len;

#ifdef HAVE_GETRANDOM
	/*
	 * Linux (glibc 2.25+) / others: getrandom(2).  It can return a short
	 * read, and can be interrupted, so loop.  GRND_NONBLOCK is NOT used:
	 * we would rather block briefly at first use than fail or fall back to
	 * a weak source.
	 */
	while (need > 0) {
		n = getrandom(p, need, 0);
		if (n < 0) {
			if (__os_get_syserr() == EINTR)
				continue;
			break;			/* Fall through to /dev/urandom. */
		}
		p += n;
		need -= (size_t)n;
	}
	if (need == 0)
		return (0);
	/* Reset and try the device. */
	p = buf;
	need = len;
#endif

#ifdef HAVE_ARC4RANDOM_BUF
	/*
	 * The BSDs and macOS: arc4random_buf() cannot fail and needs no fd.
	 */
	arc4random_buf(p, need);
	return (0);
#else
	/*
	 * Portable fallback: read /dev/urandom.  Use the OS layer so the file
	 * handling matches the rest of the library.
	 */
	if ((ret = __os_open(env, "/dev/urandom", 0,
	    DB_OSO_RDONLY, DB_MODE_600, &fd)) != 0)
		return (ret);

	while (need > 0) {
		if ((ret = __os_read(env, fd, p, need, &n)) != 0) {
			(void)__os_closehandle(env, fd);
			return (ret);
		}
		if (n == 0)			/* Unexpected EOF. */
			break;
		p += n;
		need -= (size_t)n;
	}
	(void)__os_closehandle(env, fd);

	if (need != 0) {
		__db_errx(env, DB_STR("0213",
		    "Unable to obtain random bytes from the operating system"));
		return (EIO);
	}
	return (0);
#endif
}
