/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * ex_rep_tls.c -- a worked example of adding TLS confidentiality to a
 * BASE-API replication application (DB_ENV->rep_set_transport).
 *
 * WHY THIS EXISTS
 *
 * The Replication Manager (DB_ENV->repmgr_*) can be built with built-in TLS
 * (configure --enable-tls / meson -Dtls=enabled, then
 * DB_ENV->repmgr_set_tls()).  But an application that supplies its OWN
 * transport via DB_ENV->rep_set_transport() owns the wire.  libdb hands that
 * application the marshalled control+rec DBTs and calls its send function; the
 * engine never sees a socket.  CONFIDENTIALITY OF THAT TRAFFIC IS THE
 * APPLICATION'S RESPONSIBILITY.  Even with an encrypted environment
 * (DB_ENCRYPT), the pages and log records are decrypted in the buffer pool
 * before they reach the transport, so they cross the application's wire in the
 * clear unless the application encrypts the link.
 *
 * This file shows the shape of a TLS-wrapped transport: each peer link is an
 * OpenSSL SSL* layered over the application's own socket.  The send function
 * writes through SSL_write; a reader thread (not shown in full) reads through
 * SSL_read and calls DB_ENV->rep_process_message().  Mutual authentication
 * (client + server certificates verified against a CA) gives peer identity;
 * TLS 1.3 gives confidentiality, integrity and forward secrecy.
 *
 * This is illustrative, not a drop-in library: error handling and the
 * connection/reader machinery are an application concern.  Compile it only as
 * a reference (it needs -lssl -lcrypto); it is not built by default.
 */

#include <sys/types.h>
#include <string.h>

#include <openssl/ssl.h>
#include <openssl/err.h>

#include "db.h"

/*
 * Per-peer link state the application keeps in its machine table.  The SSL*
 * wraps the already-connected socket for this peer.
 */
typedef struct {
	int	eid;		/* the peer's environment id */
	SSL	*ssl;		/* TLS session over this peer's socket */
} tls_link_t;

/*
 * Build the process-wide SSL_CTX once, at startup, from this site's cert/key
 * and the CA bundle used to verify peers.  TLS 1.3 only; require and verify a
 * peer certificate (mutual auth).
 */
SSL_CTX *
ex_rep_tls_ctx(const char *cert, const char *key, const char *ca)
{
	SSL_CTX *ctx;

	if ((ctx = SSL_CTX_new(TLS_method())) == NULL)
		return (NULL);
	(void)SSL_CTX_set_min_proto_version(ctx, TLS1_3_VERSION);
	(void)SSL_CTX_set_max_proto_version(ctx, TLS1_3_VERSION);
	if (SSL_CTX_use_certificate_chain_file(ctx, cert) != 1 ||
	    SSL_CTX_use_PrivateKey_file(ctx, key, SSL_FILETYPE_PEM) != 1 ||
	    SSL_CTX_check_private_key(ctx) != 1 ||
	    SSL_CTX_load_verify_locations(ctx, ca, NULL) != 1) {
		SSL_CTX_free(ctx);
		return (NULL);
	}
	/* Mutual authentication: refuse a peer that sends no certificate. */
	SSL_CTX_set_verify(ctx,
	    SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT, NULL);
	return (ctx);
}

/*
 * Attach TLS to a freshly connected (client) or accepted (server) socket and
 * complete the handshake.  'fd' is a connected socket the application already
 * owns.  Returns an SSL* ready for application I/O, or NULL on failure.
 */
SSL *
ex_rep_tls_handshake(SSL_CTX *ctx, int fd, int is_client)
{
	SSL *ssl;
	int rc;

	if ((ssl = SSL_new(ctx)) == NULL)
		return (NULL);
	if (SSL_set_fd(ssl, fd) != 1) {
		SSL_free(ssl);
		return (NULL);
	}
	if (is_client)
		SSL_set_connect_state(ssl);
	else
		SSL_set_accept_state(ssl);
	/*
	 * On a blocking socket SSL_do_handshake completes in one call.  On a
	 * non-blocking socket, loop on SSL_ERROR_WANT_READ/WANT_WRITE and wait
	 * for the socket in the application's event loop before retrying --
	 * exactly what repmgr's built-in TLS does internally.
	 */
	if ((rc = SSL_do_handshake(ssl)) != 1) {
		ERR_print_errors_fp(stderr);
		SSL_free(ssl);
		return (NULL);
	}
	return (ssl);
}

/*
 * The DB_ENV->rep_set_transport send function, TLS edition.  libdb calls this
 * to deliver the marshalled control + rec DBTs to a peer (or to broadcast when
 * eid == DB_EID_BROADCAST).  We write both DBTs (length-prefixed, as the
 * application's own framing requires) through the peer's SSL*, so the bytes on
 * the wire are TLS-encrypted.
 *
 * Install with:
 *   dbenv->rep_set_transport(dbenv, my_eid, ex_rep_tls_send);
 * after storing each peer's tls_link_t where find_link() can reach it.
 */
static int ex_rep_tls_write_dbt(SSL *, const DBT *);
static tls_link_t *find_link(int eid);		/* app-provided lookup */

int
ex_rep_tls_send(DB_ENV *dbenv, const DBT *control, const DBT *rec,
    const DB_LSN *lsnp, int eid, u_int32_t flags)
{
	tls_link_t *link;

	(void)lsnp;
	(void)flags;

	if (eid == DB_EID_BROADCAST) {
		/*
		 * Application loops over every known peer; omitted here.  Each
		 * iteration is the single-peer case below.  A base-API app must
		 * return 0 only when the message reached enough peers for its
		 * durability policy, and non-zero (e.g. DB_REP_UNAVAIL) when a
		 * PERMANENT message could not be delivered -- unchanged by TLS.
		 */
		return (0);
	}

	if ((link = find_link(eid)) == NULL || link->ssl == NULL)
		return (DB_REP_UNAVAIL);

	if (ex_rep_tls_write_dbt(link->ssl, control) != 0 ||
	    ex_rep_tls_write_dbt(link->ssl, rec) != 0) {
		dbenv->err(dbenv, DB_REP_UNAVAIL, "ex_rep_tls_send: TLS write");
		return (DB_REP_UNAVAIL);
	}
	return (0);
}

/*
 * Write one DBT as a 4-byte big-endian length followed by its bytes, through
 * TLS.  On a non-blocking socket, SSL_write can return WANT_READ/WANT_WRITE;
 * retry with the identical pointer+length (as repmgr does) once the socket is
 * ready.  Shown here against a blocking socket for brevity.
 */
static int
ex_rep_tls_write_dbt(SSL *ssl, const DBT *dbt)
{
	unsigned char hdr[4];
	u_int32_t len;

	len = dbt->size;
	hdr[0] = (unsigned char)(len >> 24);
	hdr[1] = (unsigned char)(len >> 16);
	hdr[2] = (unsigned char)(len >> 8);
	hdr[3] = (unsigned char)(len);
	if (SSL_write(ssl, hdr, sizeof(hdr)) != (int)sizeof(hdr))
		return (-1);
	if (len > 0 && SSL_write(ssl, dbt->data, (int)len) != (int)len)
		return (-1);
	return (0);
}

/*
 * The receiving side (sketch): a per-peer reader thread reads the two
 * length-prefixed DBTs back through SSL_read, then calls
 *   dbenv->rep_process_message(dbenv, &control, &rec, &peer_eid, &ret_lsn);
 * The engine applies the log/page just as with any transport; TLS is invisible
 * to it.  DB_REP_ISPERM / DB_REP_NOTPERM handling is unchanged.
 */
