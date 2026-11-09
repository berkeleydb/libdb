/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * TLS (OpenSSL) transport for the Replication Manager (RFC 0018).
 *
 * This file is the ONLY repmgr file that includes the OpenSSL headers.  Everything
 * here is process-local: an SSL or SSL_CTX object never lives in the shared
 * region.  The rest of repmgr calls these functions through the prototypes in
 * repmgr.h and routes their WANT-read/WANT-write results into the single
 * select()/poll() loop; see repmgr_net.c, repmgr_sel.c and repmgr_posix.c.
 *
 * Return convention (so callers never see an OpenSSL type):
 *   0            progress made / operation complete
 *   WOULDBLOCK   would block -- the caller must re-select this connection;
 *                the direction is in conn->ssl_flags (WANT_WRITE_TO_READ /
 *                WANT_READ_TO_WRITE), which prepare_io() consults.
 *   DB_REP_UNAVAIL  clean EOF / peer closed / verification failure
 *   <errno>      a real error
 */

#include "db_config.h"
#include "db_int.h"

#ifdef HAVE_REPMGR_SSL

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509.h>
#include <openssl/evp.h>
#include <pthread.h>

static int __repmgr_ssl_error __P((ENV *, REPMGR_CONNECTION *, int));
static int __repmgr_ssl_verify_cb __P((int, X509_STORE_CTX *));

/*
 * The verify callback reaches the ENV (for the pin list) through the SSL
 * object's ex_data slot, set once in __repmgr_ssl_conn_init.  Handshakes run
 * concurrently -- outbound ones in each site's connector thread and in the
 * channel API, without db_rep->mutex; inbound ones in the select() thread --
 * so a process-global "current connection" would race: a handshake clearing it
 * while another verifies made the callback see NULL and skip pinning.
 * ssl_env_idx is allocated once per process (SSL_get_ex_new_index is
 * thread-safe in OpenSSL 1.1+); -1 means not yet allocated.
 */
static int ssl_env_idx = -1;
static pthread_once_t ssl_env_idx_once = PTHREAD_ONCE_INIT;

static void
__repmgr_ssl_env_idx_init()
{
	ssl_env_idx = SSL_get_ex_new_index(0, NULL, NULL, NULL, NULL);
}

/*
 * __repmgr_ssl_handshake_block --
 *	Drive the TLS handshake to completion on a (blocking) socket, pumping
 *	the socket with select() between WANT_READ/WANT_WRITE steps.  Used by
 *	the connect thread, where blocking does not stall the main loop.
 *
 * PUBLIC: #ifdef HAVE_REPMGR_SSL
 * PUBLIC: int __repmgr_ssl_handshake_block __P((ENV *, REPMGR_CONNECTION *));
 * PUBLIC: #endif
 */
int
__repmgr_ssl_handshake_block(env, conn)
	ENV *env;
	REPMGR_CONNECTION *conn;
{
	fd_set fds;
	struct timeval tv;
	int n, ret, want_write;

	for (;;) {
		switch (ret = __repmgr_ssl_handshake(env, conn)) {
		case 0:
			return (0);		/* handshake complete */
		case WOULDBLOCK:
			break;			/* need to wait, below */
		default:
			return (ret);
		}
		want_write = FLD_ISSET(conn->ssl_flags, REPMGR_SSL_WANT_WRITE_TO_READ);
		FD_ZERO(&fds);
		FD_SET((u_int)conn->fd, &fds);
		/* 30s cap so a dead peer cannot hang the connect thread. */
		tv.tv_sec = 30;
		tv.tv_usec = 0;
		if (want_write)
			n = select((int)conn->fd + 1,
			    NULL, &fds, NULL, &tv);
		else
			n = select((int)conn->fd + 1,
			    &fds, NULL, NULL, &tv);
		if (n == 0) {
			__db_errx(env, DB_STR("5458",
			    "repmgr TLS: handshake timed out"));
			return (DB_REP_UNAVAIL);
		}
		if (n < 0) {
			if (errno == EINTR)
				continue;
			return (errno);
		}
	}
}

/*
 * __repmgr_ssl_init --
 *	Create the process-local SSL_CTX from the configured cert/key/CA.
 *	Idempotent: a no-op if no cert is configured (plaintext operation) or
 *	if the context already exists.
 *
 * PUBLIC: #ifdef HAVE_REPMGR_SSL
 * PUBLIC: int __repmgr_ssl_init __P((ENV *));
 * PUBLIC: #endif
 */
int
__repmgr_ssl_init(env)
	ENV *env;
{
	DB_REP *db_rep;
	SSL_CTX *ctx;

	db_rep = env->rep_handle;

	if (db_rep->tls_ctx != NULL)		/* already built */
		return (0);
	if (db_rep->tls_cert_file == NULL)	/* TLS not configured */
		return (0);

	(void)pthread_once(&ssl_env_idx_once, __repmgr_ssl_env_idx_init);
	if (ssl_env_idx < 0)
		return (__repmgr_ssl_error(env, NULL, 0));
	if ((ctx = SSL_CTX_new(TLS_method())) == NULL)
		return (__repmgr_ssl_error(env, NULL, 0));

	/* TLS 1.3 only: no downgrade, no legacy ciphers. */
	if (SSL_CTX_set_min_proto_version(ctx, TLS1_3_VERSION) != 1 ||
	    SSL_CTX_set_max_proto_version(ctx, TLS1_3_VERSION) != 1) {
		SSL_CTX_free(ctx);
		return (__repmgr_ssl_error(env, NULL, 0));
	}

	if (SSL_CTX_use_certificate_chain_file(ctx,
	    db_rep->tls_cert_file) != 1 ||
	    SSL_CTX_use_PrivateKey_file(ctx,
	    db_rep->tls_key_file, SSL_FILETYPE_PEM) != 1 ||
	    SSL_CTX_check_private_key(ctx) != 1) {
		__db_errx(env, DB_STR("5450",
		    "repmgr TLS: cannot load certificate or private key"));
		SSL_CTX_free(ctx);
		return (__repmgr_ssl_error(env, NULL, 0));
	}

	if (db_rep->tls_ca_file != NULL &&
	    SSL_CTX_load_verify_locations(ctx,
	    db_rep->tls_ca_file, NULL) != 1) {
		__db_errx(env, DB_STR("5451",
		    "repmgr TLS: cannot load CA bundle"));
		SSL_CTX_free(ctx);
		return (__repmgr_ssl_error(env, NULL, 0));
	}

	/*
	 * Mutual authentication: both ends present a certificate and both
	 * verify the peer's.  SSL_VERIFY_FAIL_IF_NO_PEER_CERT makes the server
	 * refuse a client that sends none.  Our callback adds optional pinning.
	 */
	if (FLD_ISSET(db_rep->tls_flags, REPMGR_TLS_VERIFY_PEER))
		SSL_CTX_set_verify(ctx,
		    SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT,
		    __repmgr_ssl_verify_cb);
	else
		__db_errx(env, DB_STR("5452",
    "repmgr TLS: peer verification DISABLED (DB_REPMGR_TLS_NO_VERIFY)"));

	db_rep->tls_ctx = ctx;
	return (0);
}

/*
 * __repmgr_ssl_destroy --
 *	Free the process-local SSL_CTX.
 *
 * PUBLIC: #ifdef HAVE_REPMGR_SSL
 * PUBLIC: void __repmgr_ssl_destroy __P((ENV *));
 * PUBLIC: #endif
 */
void
__repmgr_ssl_destroy(env)
	ENV *env;
{
	DB_REP *db_rep;

	db_rep = env->rep_handle;
	if (db_rep->tls_ctx != NULL) {
		SSL_CTX_free((SSL_CTX *)db_rep->tls_ctx);
		db_rep->tls_ctx = NULL;
	}
}

/*
 * __repmgr_ssl_conn_init --
 *	Attach a new SSL object to a connection and set it handshaking.  Does
 *	nothing (leaves conn->ssl == NULL, i.e. plaintext) when TLS is not
 *	configured.  is_client selects connect vs accept role.
 *
 * PUBLIC: #ifdef HAVE_REPMGR_SSL
 * PUBLIC: int __repmgr_ssl_conn_init __P((ENV *, REPMGR_CONNECTION *, int));
 * PUBLIC: #endif
 */
int
__repmgr_ssl_conn_init(env, conn, is_client)
	ENV *env;
	REPMGR_CONNECTION *conn;
	int is_client;
{
	DB_REP *db_rep;
	SSL *ssl;

	db_rep = env->rep_handle;
	if (db_rep->tls_ctx == NULL)		/* plaintext operation */
		return (0);

	if ((ssl = SSL_new((SSL_CTX *)db_rep->tls_ctx)) == NULL)
		return (__repmgr_ssl_error(env, conn, 0));
	if (ssl_env_idx < 0 || SSL_set_ex_data(ssl, ssl_env_idx, env) != 1) {
		SSL_free(ssl);
		return (__repmgr_ssl_error(env, conn, 0));
	}
	if (SSL_set_fd(ssl, (int)conn->fd) != 1) {
		SSL_free(ssl);
		return (__repmgr_ssl_error(env, conn, 0));
	}
	if (is_client)
		SSL_set_connect_state(ssl);
	else
		SSL_set_accept_state(ssl);

	conn->ssl = ssl;
	FLD_SET(conn->ssl_flags, REPMGR_SSL_HANDSHAKING);
	conn->reading_phase = HANDSHAKE_PHASE;
	return (0);
}

/*
 * __repmgr_ssl_conn_destroy --
 *	Free a connection's SSL object.
 *
 * PUBLIC: #ifdef HAVE_REPMGR_SSL
 * PUBLIC: void __repmgr_ssl_conn_destroy __P((REPMGR_CONNECTION *));
 * PUBLIC: #endif
 */
void
__repmgr_ssl_conn_destroy(conn)
	REPMGR_CONNECTION *conn;
{
	if (conn->ssl != NULL) {
		/*
		 * Best-effort unidirectional close_notify; we do not wait for
		 * the peer's, since the socket is about to be closed anyway.
		 */
		(void)SSL_shutdown((SSL *)conn->ssl);
		SSL_free((SSL *)conn->ssl);
		conn->ssl = NULL;
	}
}

/*
 * Map an SSL_get_error result into our return convention, recording the
 * direction OpenSSL needs in conn->ssl_flags so the select loop can wait on
 * the correct fd_set.  'phase' names whether we were handshaking, reading or
 * writing, so WANT_READ/WANT_WRITE become the right want bit.
 */
static int
ssl_get_error(env, conn, rc, writing)
	ENV *env;
	REPMGR_CONNECTION *conn;
	int rc;
	int writing;
{
	int err;

	err = SSL_get_error((SSL *)conn->ssl, rc);
	switch (err) {
	case SSL_ERROR_WANT_READ:
		/*
		 * OpenSSL needs to read TLS bytes.  If our caller wanted to
		 * write, this is a write-blocked-on-read; otherwise it is a
		 * plain read that can proceed when the socket is readable.
		 */
		if (writing)
			FLD_SET(conn->ssl_flags, REPMGR_SSL_WANT_READ_TO_WRITE);
		return (WOULDBLOCK);
	case SSL_ERROR_WANT_WRITE:
		/*
		 * OpenSSL needs to write TLS bytes.  If our caller wanted to
		 * read, this is a read-blocked-on-write -> select for write.
		 */
		if (!writing)
			FLD_SET(conn->ssl_flags, REPMGR_SSL_WANT_WRITE_TO_READ);
		return (WOULDBLOCK);
	case SSL_ERROR_ZERO_RETURN:
		return (DB_REP_UNAVAIL);	/* clean TLS close */
	case SSL_ERROR_SYSCALL:
		if (rc == 0)
			return (DB_REP_UNAVAIL); /* EOF before close_notify */
		return (__repmgr_ssl_error(env, conn, err));
	default:
		return (__repmgr_ssl_error(env, conn, err));
	}
}

/*
 * __repmgr_ssl_handshake --
 *	Drive SSL_do_handshake one step.  Returns 0 and clears HANDSHAKING when
 *	the handshake completes; WOULDBLOCK (with a want bit set) to be called
 *	again; or an error.
 *
 * PUBLIC: #ifdef HAVE_REPMGR_SSL
 * PUBLIC: int __repmgr_ssl_handshake __P((ENV *, REPMGR_CONNECTION *));
 * PUBLIC: #endif
 */
int
__repmgr_ssl_handshake(env, conn)
	ENV *env;
	REPMGR_CONNECTION *conn;
{
	int rc, ret;

	FLD_CLR(conn->ssl_flags,
	    REPMGR_SSL_WANT_READ_TO_WRITE | REPMGR_SSL_WANT_WRITE_TO_READ);

	rc = SSL_do_handshake((SSL *)conn->ssl);

	if (rc == 1) {
		FLD_CLR(conn->ssl_flags, REPMGR_SSL_HANDSHAKING);
		conn->reading_phase = SIZES_PHASE;
		RPRINT(env, (env, DB_VERB_REPMGR_MISC,
		    "repmgr TLS handshake complete (%s)",
		    SSL_get_version((SSL *)conn->ssl)));
		return (0);
	}
	/*
	 * A handshake can block in either direction.  ssl_get_error with
	 * writing=0 records WANT_WRITE as WANT_WRITE_TO_READ; but a handshake
	 * that needs to write should select for write regardless, so also set
	 * that bit explicitly on WANT_WRITE.
	 */
	ret = ssl_get_error(env, conn, rc, 0 /* treat like reading */);
	if (ret == WOULDBLOCK &&
	    SSL_want((SSL *)conn->ssl) == SSL_WRITING)
		FLD_SET(conn->ssl_flags, REPMGR_SSL_WANT_WRITE_TO_READ);
	return (ret);
}

/*
 * __repmgr_ssl_read --
 *	Read up to len bytes of application data.  *nrp gets the count read.
 *
 * PUBLIC: #ifdef HAVE_REPMGR_SSL
 * PUBLIC: int __repmgr_ssl_read
 * PUBLIC:     __P((ENV *, REPMGR_CONNECTION *, void *, size_t, size_t *));
 * PUBLIC: #endif
 */
int
__repmgr_ssl_read(env, conn, buf, len, nrp)
	ENV *env;
	REPMGR_CONNECTION *conn;
	void *buf;
	size_t len;
	size_t *nrp;
{
	int rc;

	FLD_CLR(conn->ssl_flags,
	    REPMGR_SSL_WANT_READ_TO_WRITE | REPMGR_SSL_WANT_WRITE_TO_READ);
	*nrp = 0;
	rc = SSL_read((SSL *)conn->ssl, buf, (int)len);
	if (rc > 0) {
		*nrp = (size_t)rc;
		return (0);
	}
	return (ssl_get_error(env, conn, rc, 0 /* reading */));
}

/*
 * __repmgr_ssl_write --
 *	Write up to len bytes of application data.  *nwp gets the count
 *	written.  The caller must retry a WOULDBLOCK with the SAME buffer and
 *	length (we leave SSL_MODE_ENABLE_PARTIAL_WRITE off), which repmgr's
 *	saved iovec offset guarantees.
 *
 * PUBLIC: #ifdef HAVE_REPMGR_SSL
 * PUBLIC: int __repmgr_ssl_write
 * PUBLIC:     __P((ENV *, REPMGR_CONNECTION *, const void *, size_t, size_t *));
 * PUBLIC: #endif
 */
int
__repmgr_ssl_write(env, conn, buf, len, nwp)
	ENV *env;
	REPMGR_CONNECTION *conn;
	const void *buf;
	size_t len;
	size_t *nwp;
{
	int rc;

	FLD_CLR(conn->ssl_flags,
	    REPMGR_SSL_WANT_READ_TO_WRITE | REPMGR_SSL_WANT_WRITE_TO_READ);
	*nwp = 0;
	rc = SSL_write((SSL *)conn->ssl, buf, (int)len);
	if (rc > 0) {
		*nwp = (size_t)rc;
		return (0);
	}
	return (ssl_get_error(env, conn, rc, 1 /* writing */));
}

/*
 * Verify callback: OpenSSL has already done path/expiry/CA checks; preverify_ok
 * reflects that.  On the leaf cert (depth 0) we additionally enforce optional
 * SHA-256 pinning.  Returns 1 to accept, 0 to reject.
 */
static int
__repmgr_ssl_verify_cb(preverify_ok, store_ctx)
	int preverify_ok;
	X509_STORE_CTX *store_ctx;
{
	ENV *env;
	DB_REP *db_rep;
	SSL *ssl;
	X509 *cert;
	unsigned char md[EVP_MAX_MD_SIZE];
	char hex[2 * EVP_MAX_MD_SIZE + 1];
	unsigned int n, i;
	char *p;

	if (!preverify_ok)
		return (0);			/* CA/expiry/path failed */

	/*
	 * Fail closed: a handshake whose SSL object carries no ENV cannot
	 * check the pin list, so it must not pass on the CA check alone.
	 */
	if ((ssl = X509_STORE_CTX_get_ex_data(store_ctx,
	    SSL_get_ex_data_X509_STORE_CTX_idx())) == NULL ||
	    (env = SSL_get_ex_data(ssl, ssl_env_idx)) == NULL)
		return (0);
	db_rep = env->rep_handle;
	if (db_rep->tls_pins == NULL)		/* no pinning configured */
		return (1);
	if (X509_STORE_CTX_get_error_depth(store_ctx) != 0)
		return (1);			/* pin only the leaf */

	if ((cert = X509_STORE_CTX_get_current_cert(store_ctx)) == NULL)
		return (0);
	if (X509_digest(cert, EVP_sha256(), md, &n) != 1)
		return (0);
	for (i = 0; i < n; i++)
		(void)snprintf(&hex[2 * i], 3, "%02x", md[i]);

	/* tls_pins is a NUL-separated, double-NUL-terminated list of hexes. */
	for (p = db_rep->tls_pins; *p != '\0'; p += strlen(p) + 1)
		if (strcasecmp(p, hex) == 0)
			return (1);		/* pinned */
	__db_errx(env, DB_STR("5453",
	    "repmgr TLS: peer certificate fingerprint not in pin list"));
	return (0);
}

/*
 * Report the current OpenSSL error stack and return a non-zero errno-like
 * code.  Logs the human-readable reason.
 */
static int
__repmgr_ssl_error(env, conn, ssl_err)
	ENV *env;
	REPMGR_CONNECTION *conn;
	int ssl_err;
{
	unsigned long e;
	char ebuf[256];

	COMPQUIET(conn, NULL);
	COMPQUIET(ssl_err, 0);
	while ((e = ERR_get_error()) != 0) {
		ERR_error_string_n(e, ebuf, sizeof(ebuf));
		__db_errx(env, DB_STR_A("5454",
		    "repmgr TLS: %s", "%s"), ebuf);
	}
	return (DB_REP_UNAVAIL);
}

#endif /* HAVE_REPMGR_SSL */
