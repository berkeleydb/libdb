/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * Some parts of this code originally written by Adam Stubblefield
 * -- astubble@rice.edu
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/crypto.h"
#include "dbinc/hmac.h"

/*
 * __crypto_provider_kek --
 *	Obtain the key-encryption key (KEK) and the verifier secret from the
 *	installed provider.  For the built-in password provider the KEK is a
 *	fast SHA-256 of the password and the verifier secret is the password
 *	itself; a custom provider supplies the KEK and we verify the KEK.
 *
 *	vsecret/vslen point at the bytes fed to the slow PBKDF2 verifier.
 */
static int
__crypto_provider_kek(env, kek, vsecret, vslen)
	ENV *env;
	u_int8_t *kek;
	const u_int8_t **vsecret;
	size_t *vslen;
{
	DB_ENV *dbenv;
	DB_CRYPTO_PROVIDER *prov;
	int ret;

	dbenv = env->dbenv;
	prov = dbenv->crypto_provider;

	if (prov != NULL && prov->provider_id != DB_CRYPTO_PROV_PASSWD) {
		/*
		 * A key-file / TPM / custom provider supplies the KEK
		 * directly.  The verifier is computed over the KEK so a
		 * joiner with the wrong key is rejected without the region
		 * holding the secret.
		 */
		if (prov->get_key == NULL)
			return (EINVAL);
		if ((ret = prov->get_key(prov, NULL, kek,
		    DB_CRYPTO_KEK_LEN)) != 0)
			return (ret);
		*vsecret = kek;
		*vslen = DB_CRYPTO_KEK_LEN;
		return (0);
	}

	/* Password provider: KEK = SHA-256(passwd), verifier over passwd. */
	if (dbenv->passwd == NULL)
		return (EINVAL);
	{
		SHA256_CTX ctx;
		__db_SHA256Init(&ctx);
		__db_SHA256Update(&ctx,
		    (u_int8_t *)dbenv->passwd, dbenv->passwd_len);
		__db_SHA256Update(&ctx,
		    (u_int8_t *)DB_ENC_MAGIC, strlen(DB_ENC_MAGIC));
		__db_SHA256Final(kek, &ctx);
	}
	*vsecret = (const u_int8_t *)dbenv->passwd;
	*vslen = dbenv->passwd_len;
	return (0);
}

/*
 * __crypto_region_init --
 *	Initialize crypto.  RFC 0019: the region holds a salted PBKDF2 verifier
 *	and (in wrapping mode) a KEK-wrapped data key -- never a cleartext
 *	secret.  A joiner recomputes the verifier and compares it in constant
 *	time.
 */
int
__crypto_region_init(env)
	ENV *env;
{
	CIPHER *cipher;
	DB_CIPHER *db_cipher;
	DB_ENV *dbenv;
	REGENV *renv;
	REGINFO *infop;
	const u_int8_t *vsecret;
	u_int8_t kek[DB_CRYPTO_KEK_LEN], verifier[DB_CRYPTO_VRFY_LEN];
	u_int8_t dek[DB_CRYPTO_DEK_LEN];
	size_t vslen;
	u_int32_t iters, wrap;
	int ret;

	dbenv = env->dbenv;
	infop = env->reginfo;
	renv = infop->primary;
	db_cipher = env->crypto_handle;
	ret = 0;
	vsecret = NULL;
	vslen = 0;

	/* Wrapping mode: required whenever the provider is not a password. */
	wrap = (dbenv->crypto_provider != NULL &&
	    dbenv->crypto_provider->provider_id != DB_CRYPTO_PROV_PASSWD) ?
	    1 : 0;

	if (renv->cipher_off == INVALID_ROFF) {
		if (!CRYPTO_ON(env))
			return (0);
		if (!F_ISSET(infop, REGION_CREATE)) {
			__db_errx(env, DB_STR("0172",
    "Joining non-encrypted environment with encryption key"));
			return (EINVAL);
		}
		if (F_ISSET(db_cipher, CIPHER_ANY)) {
			__db_errx(env, DB_STR("0173",
			    "Encryption algorithm not supplied"));
			return (EINVAL);
		}
		/*
		 * Creator.  Build a salted verifier (no secret at rest).  In
		 * wrapping mode, also generate a random DEK and store it
		 * wrapped under the provider KEK.
		 */
		if ((ret = __crypto_provider_kek(env,
		    kek, &vsecret, &vslen)) != 0)
			return (ret);
		iters = DB_CRYPTO_KDF_ITERS;
		MUTEX_LOCK(env, renv->mtx_regenv);
		if ((ret = __env_alloc(infop, sizeof(CIPHER), &cipher)) != 0) {
			MUTEX_UNLOCK(env, renv->mtx_regenv);
			goto done;
		}
		MUTEX_UNLOCK(env, renv->mtx_regenv);
		memset(cipher, 0, sizeof(*cipher));
		cipher->flags = db_cipher->alg;
		cipher->fmt_version = DB_CRYPTO_FMT_VERIFIER;
		cipher->kdf_iters = iters;
		cipher->provider = (dbenv->crypto_provider != NULL) ?
		    dbenv->crypto_provider->provider_id :
		    DB_CRYPTO_PROV_PASSWD;
		if ((ret = __os_csprng(env,
		    cipher->salt, DB_CRYPTO_SALT_LEN)) != 0)
			goto done;
		if ((ret = __db_crypto_make_verifier(env, vsecret, vslen,
		    cipher->salt, iters, cipher->verifier)) != 0)
			goto done;
#ifdef CRYPTO_REGION_SECRET_SABOTAGE
		/*
		 * Sabotage arm (test teeth): put the secret back into the
		 * region (as the pre-RFC code did) WITHOUT breaking auth -- the
		 * verifier above is still correct.  The leak scan must then find
		 * the password in the region file, so the leak test FAILS.
		 */
		memcpy(cipher->wrapped_dek, vsecret,
		    vslen < DB_CRYPTO_WRAP_LEN ? vslen : DB_CRYPTO_WRAP_LEN);
#endif
		if (wrap) {
			if ((ret = __os_csprng(env, dek, DB_CRYPTO_DEK_LEN))
			    != 0)
				goto done;
			if ((ret = __db_aes_keywrap(env, kek, dek,
			    DB_CRYPTO_DEK_LEN, cipher->wrapped_dek)) != 0)
				goto done;
			cipher->dek_wrapped = 1;
			memcpy(db_cipher->dek, dek, DB_CRYPTO_DEK_LEN);
			db_cipher->dek_len = DB_CRYPTO_DEK_LEN;
		}
		renv->cipher_off = R_OFFSET(infop, cipher);
	} else {
		if (!CRYPTO_ON(env)) {
			__db_errx(env, DB_STR("0174",
		    "Encrypted environment: no encryption key supplied"));
			return (EINVAL);
		}
		cipher = R_ADDR(infop, renv->cipher_off);
		/*
		 * Joiner.  Recompute the verifier from our secret and compare
		 * in constant time against the region copy.
		 */
		if ((ret = __crypto_provider_kek(env,
		    kek, &vsecret, &vslen)) != 0)
			return (ret);
		if ((ret = __db_crypto_make_verifier(env, vsecret, vslen,
		    cipher->salt, cipher->kdf_iters, verifier)) != 0)
			goto done;
		if (!__db_ct_memeq(verifier, cipher->verifier,
		    DB_CRYPTO_VRFY_LEN)) {
			__db_errx(env, DB_STR("0175", "Invalid password"));
			ret = EPERM;
			goto done;
		}
		if (!F_ISSET(db_cipher, CIPHER_ANY) &&
		    db_cipher->alg != cipher->flags) {
			__db_errx(env, DB_STR("0176",
    "Environment encrypted using a different algorithm"));
			ret = EINVAL;
			goto done;
		}
		if (F_ISSET(db_cipher, CIPHER_ANY))
			/*
			 * We have CIPHER_ANY and we are joining the existing
			 * env.  Setup our cipher structure for whatever
			 * algorithm this env has.
			 */
			if ((ret = __crypto_algsetup(env, db_cipher,
			    cipher->flags, 0)) != 0)
				goto done;
		if (cipher->dek_wrapped) {
			if ((ret = __db_aes_keyunwrap(env, kek,
			    cipher->wrapped_dek, DB_CRYPTO_WRAP_LEN, dek)) != 0) {
				if (ret == EPERM)
					__db_errx(env, DB_STR("5460",
					    "Key unwrap failed: wrong key"));
				goto done;
			}
			memcpy(db_cipher->dek, dek, DB_CRYPTO_DEK_LEN);
			db_cipher->dek_len = DB_CRYPTO_DEK_LEN;
		}
	}
	ret = db_cipher->init(env, db_cipher);

done:
	/*
	 * Scrub all transient secrets from the stack, and smash/free the
	 * env password now that the region (verifier) is set up.
	 */
	__db_explicit_bzero(kek, sizeof(kek));
	__db_explicit_bzero(dek, sizeof(dek));
	__db_explicit_bzero(verifier, sizeof(verifier));
	if (dbenv->passwd != NULL) {
		__db_explicit_bzero(dbenv->passwd, dbenv->passwd_len);
		__os_free(env, dbenv->passwd);
		dbenv->passwd = NULL;
		dbenv->passwd_len = 0;
	}

	return (ret);
}

/*
 * __crypto_env_close --
 *	Crypto-specific destruction of ENV structure.
 *
 * PUBLIC: int __crypto_env_close __P((ENV *));
 */
int
__crypto_env_close(env)
	ENV *env;
{
	DB_CIPHER *db_cipher;
	DB_ENV *dbenv;
	int ret;

	dbenv = env->dbenv;

	if (dbenv->passwd != NULL) {
		memset(dbenv->passwd, 0xff, dbenv->passwd_len-1);
		__os_free(env, dbenv->passwd);
		dbenv->passwd = NULL;
	}

	/* Free an env-owned built-in provider (keyfile/TPM). */
	if (dbenv->crypto_provider != NULL &&
	    (dbenv->crypto_provider->flags & 0x80000000) != 0) {
		if (dbenv->crypto_provider->ctx != NULL)
			__os_free(env, dbenv->crypto_provider->ctx);
		__os_free(env, dbenv->crypto_provider);
		dbenv->crypto_provider = NULL;
	}

	if (!CRYPTO_ON(env))
		return (0);

	ret = 0;
	db_cipher = env->crypto_handle;
	if (!F_ISSET(db_cipher, CIPHER_ANY))
		ret = db_cipher->close(env, db_cipher->data);
	/* Scrub process-local key material before freeing the handle. */
	__db_explicit_bzero(db_cipher->dek, sizeof(db_cipher->dek));
	__db_explicit_bzero(db_cipher->mac_key, sizeof(db_cipher->mac_key));
	__os_free(env, db_cipher);

	env->crypto_handle = NULL;
	return (ret);
}

/*
 * __crypto_env_refresh --
 *	Clean up after the crpto system on a close or failed open.
 *
 * PUBLIC: int __crypto_env_refresh __P((ENV *));
 */
int
__crypto_env_refresh(env)
	ENV *env;
{
	CIPHER *cipher;
	REGENV *renv;
	REGINFO *infop;

	/*
	 * If a private region, return the memory to the heap.  Not needed for
	 * filesystem-backed or system shared memory regions, that memory isn't
	 * owned by any particular process.
	 */
	if (F_ISSET(env, ENV_PRIVATE)) {
		infop = env->reginfo;
		renv = infop->primary;
		if (renv->cipher_off != INVALID_ROFF) {
			cipher = R_ADDR(infop, renv->cipher_off);
			MUTEX_LOCK(env, renv->mtx_regenv);
			__env_alloc_free(infop, cipher);
			MUTEX_UNLOCK(env, renv->mtx_regenv);
		}
	}
	return (0);
}

/*
 * __crypto_algsetup --
 *	Given a db_cipher structure and a valid algorithm flag, call
 * the specific algorithm setup function.
 *
 * PUBLIC: int __crypto_algsetup __P((ENV *, DB_CIPHER *, u_int32_t, int));
 */
int
__crypto_algsetup(env, db_cipher, alg, do_init)
	ENV *env;
	DB_CIPHER *db_cipher;
	u_int32_t alg;
	int do_init;
{
	int ret;

	ret = 0;
	if (!CRYPTO_ON(env)) {
		__db_errx(env, DB_STR("0177",
		    "No cipher structure given"));
		return (EINVAL);
	}
	F_CLR(db_cipher, CIPHER_ANY);
	switch (alg) {
	case CIPHER_AES:
		db_cipher->alg = CIPHER_AES;
		ret = __aes_setup(env, db_cipher);
		break;
	default:
		ret = __env_panic(env, EINVAL);
		break;
	}
	if (ret == 0 && do_init)
		ret = db_cipher->init(env, db_cipher);
	return (ret);
}

/*
 * __crypto_decrypt_meta --
 *	Perform decryption on a metapage if needed.
 *
 * PUBLIC:  int __crypto_decrypt_meta __P((ENV *, DB *, u_int8_t *, int));
 */
int
__crypto_decrypt_meta(env, dbp, mbuf, do_metachk)
	ENV *env;
	DB *dbp;
	u_int8_t *mbuf;
	int do_metachk;
{
	DB dummydb;
	DBMETA *meta;
	DB_CIPHER *db_cipher;
	size_t pg_off;
	int ret;
	u_int8_t *iv;

	/*
	 * If we weren't given a dbp, we just want to decrypt the page on
	 * behalf of some internal subsystem, not on behalf of a user with
	 * a dbp.  Therefore, set up a dummy dbp so that the call to
	 * P_OVERHEAD below works.
	 */
	if (dbp == NULL) {
		memset(&dummydb, 0, sizeof(DB));
		dbp = &dummydb;
	}

	ret = 0;
	meta = (DBMETA *)mbuf;

	/*
	 * !!!
	 * We used an "unused" field in the meta-data page to flag whether or
	 * not the database is encrypted.  Unfortunately, that unused field
	 * was used in Berkeley DB releases before 3.0 (for example, 2.7.7).
	 * It would have been OK, except encryption doesn't follow the usual
	 * rules of "upgrade before doing anything else", we check encryption
	 * before checking for old versions of the database.
	 *
	 * We don't have to check Btree databases -- before 3.0, the field of
	 * interest was the bt_maxkey field (which was never supported and has
	 * since been removed).
	 *
	 * Ugly check to jump out if this format is older than what we support.
	 * This works because we do not encrypt the page header.
	 */
	if (meta->magic == DB_HASHMAGIC && meta->version <= 5)
		return (0);

	/*
	 * Meta-pages may be encrypted for DBMETASIZE bytes.  If we have a
	 * non-zero IV (that is written after encryption) then we decrypt (or
	 * error if the user isn't set up for security).  We guarantee that
	 * the IV space on non-encrypted pages will be zero and a zero-IV is
	 * illegal for encryption.  Therefore any non-zero IV means an
	 * encrypted database.  This basically checks the passwd on the file
	 * if we cannot find a good magic number.  We walk through all the
	 * algorithms we know about attempting to decrypt (and possibly
	 * byteswap).
	 *
	 * !!!
	 * All method meta pages have the IV and checksum at the exact same
	 * location, but not in DBMETA, use BTMETA.
	 */
	if (meta->encrypt_alg != 0) {
		db_cipher = env->crypto_handle;
		if (!F_ISSET(dbp, DB_AM_ENCRYPT)) {
			if (!CRYPTO_ON(env)) {
				__db_errx(env, DB_STR("0178",
    "Encrypted database: no encryption flag specified"));
				return (EINVAL);
			}
			/*
			 * User has a correct, secure env, but has encountered
			 * a database in that env that is secure, but user
			 * didn't dbp->set_flags.  Since it is existing, use
			 * encryption if it is that way already.
			 */
			F_SET(dbp, DB_AM_ENCRYPT|DB_AM_CHKSUM);
		}
		/*
		 * This was checked in set_flags when DB_AM_ENCRYPT was set.
		 * So it better still be true here.
		 */
		DB_ASSERT(env, CRYPTO_ON(env));
		if (!F_ISSET(db_cipher, CIPHER_ANY) &&
		    meta->encrypt_alg != db_cipher->alg) {
			__db_errx(env, DB_STR("0179",
			    "Database encrypted using a different algorithm"));
			return (EINVAL);
		}
		DB_ASSERT(env, F_ISSET(dbp, DB_AM_CHKSUM));
		iv = ((BTMETA *)mbuf)->iv;
		/*
		 * For ALL pages, we do not encrypt the beginning of the page
		 * that contains overhead information.  This is true of meta
		 * and all other pages.
		 */
		pg_off = P_OVERHEAD(dbp);
alg_retry:
		/*
		 * If they asked for a specific algorithm, then
		 * use it.  Otherwise walk through those we know.
		 */
		if (!F_ISSET(db_cipher, CIPHER_ANY)) {
			if (do_metachk && (ret = db_cipher->decrypt(env,
			    db_cipher->data, iv, mbuf + pg_off,
			    DBMETASIZE - pg_off)))
				return (ret);
			if (((BTMETA *)meta)->crypto_magic !=
			    meta->magic) {
				__db_errx(env, DB_STR("0180",
				    "Invalid password"));
				return (EINVAL);
			}
			/*
			 * Success here.  The algorithm asked for and the one
			 * on the file match.  We've just decrypted the meta
			 * page and checked the magic numbers.  They match,
			 * indicating the password is right.  All is right
			 * with the world.
			 */
			return (0);
		}
		/*
		 * If we get here, CIPHER_ANY must be set.
		 */
		ret = __crypto_algsetup(env, db_cipher, meta->encrypt_alg, 1);
		goto alg_retry;
	} else if (F_ISSET(dbp, DB_AM_ENCRYPT)) {
		/*
		 * They gave us a passwd, but the database is not encrypted.
		 * This is an error.  We do NOT want to silently allow them
		 * to write data in the clear when the user set up and expects
		 * encrypted data.
		 *
		 * This covers at least the following scenario.
		 * 1.  User creates and sets up an encrypted database.
		 * 2.  Attacker cannot read the actual data in the database
		 * because it is encrypted, but can remove/replace the file
		 * with an empty, unencrypted database file.
		 * 3.  User sets encryption and we get to this code now.
		 * If we allowed the file to be used in the clear since
		 * it is that way on disk, the user would unsuspectingly
		 * write sensitive data in the clear.
		 * 4.  Attacker reads data that user thought was encrypted.
		 *
		 * Therefore, asking for encryption with a database that
		 * was not encrypted is an error.
		 */
		__db_errx(env, DB_STR("0181",
		    "Unencrypted database with a supplied encryption key"));
		return (EINVAL);
	}
	return (ret);
}

/*
 * __crypto_set_passwd --
 *	Formerly reconstructed the env password from the shared region to
 *	duplicate an environment handle.  RFC 0019 no longer stores the secret
 *	in the region (only a salted verifier), so a handle cannot be
 *	duplicated this way; the application must call set_encrypt /
 *	set_encrypt_provider on the new handle itself.  No caller remains in
 *	the tree; kept for ABI and to fail loudly if resurrected.
 *
 * PUBLIC: int __crypto_set_passwd __P((ENV *, ENV *));
 */
int
__crypto_set_passwd(env_src, env_dest)
	ENV *env_src, *env_dest;
{
	COMPQUIET(env_dest, NULL);
	DB_ASSERT(env_src, CRYPTO_ON(env_src));
	__db_errx(env_src, DB_STR("5461",
	    "Cannot copy an encryption key from the region: supply the key to the new handle"));
	return (EINVAL);
}
