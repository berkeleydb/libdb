/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * TPM 2.0 key provider (RFC 0019), built only under --enable-tpm / -Dtpm=enabled.
 * The key-encryption key (KEK) is a 256-bit secret sealed to the TPM storage
 * hierarchy as a keyedhash (sealed-data) object, optionally bound to a PCR
 * policy.  The sealed blob (public + private parts, marshalled) lives in a
 * file in the env home; get_key loads the blob under a primary key in the
 * storage hierarchy and unseals it.  A wrong PCR state (or a wrong TPM) makes
 * the unseal fail, which surfaces as EPERM at env open.
 *
 * Uses the libtss2 ESAPI (BSD-2-Clause, license-compatible).  db_keytool
 * creates the sealed blob; this file only consumes it.
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/crypto.h"

#ifdef HAVE_TPM

#include <tss2/tss2_esys.h>
#include <tss2/tss2_mu.h>

/* On-disk layout of the sealed blob file written by db_keytool. */
#define	DB_TPM_BLOB_MAGIC	0x5450424cu	/* "TPBL" */
typedef struct __db_tpm_blob_hdr {
	u_int32_t	magic;
	u_int32_t	pub_len;
	u_int32_t	priv_len;
	u_int32_t	pcr_mask;	/* 0 => no PCR policy. */
} DB_TPM_BLOB_HDR;

/*
 * The SRK template: a fixed primary key in the storage hierarchy, so the same
 * parent is recreated deterministically to load the sealed object.  ECC P-256
 * restricted decrypt key (the standard SRK shape).
 */
static const TPM2B_PUBLIC srk_template = {
	.size = 0,
	.publicArea = {
		.type = TPM2_ALG_ECC,
		.nameAlg = TPM2_ALG_SHA256,
		.objectAttributes = (TPMA_OBJECT_USERWITHAUTH |
		    TPMA_OBJECT_RESTRICTED | TPMA_OBJECT_DECRYPT |
		    TPMA_OBJECT_FIXEDTPM | TPMA_OBJECT_FIXEDPARENT |
		    TPMA_OBJECT_SENSITIVEDATAORIGIN | TPMA_OBJECT_NODA),
		.authPolicy = { .size = 0 },
		.parameters.eccDetail = {
			.symmetric = { .algorithm = TPM2_ALG_AES,
			    .keyBits.aes = 128, .mode.aes = TPM2_ALG_CFB },
			.scheme = { .scheme = TPM2_ALG_NULL },
			.curveID = TPM2_ECC_NIST_P256,
			.kdf = { .scheme = TPM2_ALG_NULL } },
		.unique.ecc = { .x = { .size = 0 }, .y = { .size = 0 } }
	}
};

/*
 * __crypto_tpm_unseal --
 *	Load and unseal the KEK from the blob file into buf (len bytes).
 */
static int
__crypto_tpm_unseal(path, buf, len)
	const char *path;
	u_int8_t *buf;
	size_t len;
{
	ESYS_CONTEXT *ectx;
	ESYS_TR primary, sealed, session;
	TPM2B_PUBLIC *pub_in, in_public;
	TPM2B_PRIVATE in_private;
	TPM2B_SENSITIVE_DATA *unsealed;
	TPM2B_SENSITIVE_CREATE sens;
	TPM2B_DATA outside;
	TPML_PCR_SELECTION creation_pcr;
	FILE *fp;
	DB_TPM_BLOB_HDR hdr;
	u_int8_t *pubbuf, *privbuf;
	size_t off;
	TSS2_RC rc;
	int ret;

	ectx = NULL; primary = sealed = session = ESYS_TR_NONE;
	pub_in = NULL; unsealed = NULL; pubbuf = privbuf = NULL; ret = EPERM;

	if ((fp = fopen(path, "rb")) == NULL)
		return (ENOENT);
	if (fread(&hdr, 1, sizeof(hdr), fp) != sizeof(hdr) ||
	    hdr.magic != DB_TPM_BLOB_MAGIC) {
		(void)fclose(fp); return (EINVAL);
	}
	if ((pubbuf = malloc(hdr.pub_len)) == NULL ||
	    (privbuf = malloc(hdr.priv_len)) == NULL ||
	    fread(pubbuf, 1, hdr.pub_len, fp) != hdr.pub_len ||
	    fread(privbuf, 1, hdr.priv_len, fp) != hdr.priv_len) {
		ret = EINVAL; goto out;
	}
	(void)fclose(fp); fp = NULL;

	/*
	 * Tss2_MU_TPM2B_*_Unmarshal rejects ("Size not zero") an output whose
	 * size field is non-zero on entry, so the stack structs must start
	 * zeroed; otherwise the unseal fails on whatever the stack held.
	 */
	memset(&in_public, 0, sizeof(in_public));
	memset(&in_private, 0, sizeof(in_private));
	off = 0;
	if (Tss2_MU_TPM2B_PUBLIC_Unmarshal(pubbuf, hdr.pub_len, &off,
	    &in_public) != TSS2_RC_SUCCESS) { ret = EINVAL; goto out; }
	off = 0;
	if (Tss2_MU_TPM2B_PRIVATE_Unmarshal(privbuf, hdr.priv_len, &off,
	    &in_private) != TSS2_RC_SUCCESS) { ret = EINVAL; goto out; }

	if (Esys_Initialize(&ectx, NULL, NULL) != TSS2_RC_SUCCESS)
		{ ret = EAGAIN; goto out; }

	memset(&sens, 0, sizeof(sens));
	memset(&outside, 0, sizeof(outside));
	memset(&creation_pcr, 0, sizeof(creation_pcr));
	if (Esys_CreatePrimary(ectx, ESYS_TR_RH_OWNER,
	    ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE,
	    &sens, &srk_template, &outside, &creation_pcr,
	    &primary, &pub_in, NULL, NULL, NULL) != TSS2_RC_SUCCESS)
		{ ret = EAGAIN; goto out; }

	if (Esys_Load(ectx, primary, ESYS_TR_PASSWORD, ESYS_TR_NONE,
	    ESYS_TR_NONE, &in_private, &in_public, &sealed) != TSS2_RC_SUCCESS)
		{ ret = EPERM; goto out; }

	/*
	 * If the object has a PCR policy, start a policy session and satisfy
	 * PolicyPCR; otherwise use a password (empty) session.  A wrong PCR
	 * state makes Unseal fail -> EPERM.
	 */
	if (hdr.pcr_mask != 0) {
		TPMT_SYM_DEF sym = { .algorithm = TPM2_ALG_AES,
		    .keyBits.aes = 128, .mode.aes = TPM2_ALG_CFB };
		TPML_PCR_SELECTION pcrsel;
		memset(&pcrsel, 0, sizeof(pcrsel));
		pcrsel.count = 1;
		pcrsel.pcrSelections[0].hash = TPM2_ALG_SHA256;
		pcrsel.pcrSelections[0].sizeofSelect = 3;
		pcrsel.pcrSelections[0].pcrSelect[0] =
		    (u_int8_t)(hdr.pcr_mask & 0xff);
		pcrsel.pcrSelections[0].pcrSelect[1] =
		    (u_int8_t)((hdr.pcr_mask >> 8) & 0xff);
		pcrsel.pcrSelections[0].pcrSelect[2] =
		    (u_int8_t)((hdr.pcr_mask >> 16) & 0xff);
		if (Esys_StartAuthSession(ectx, ESYS_TR_NONE, ESYS_TR_NONE,
		    ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE, NULL,
		    TPM2_SE_POLICY, &sym, TPM2_ALG_SHA256, &session)
		    != TSS2_RC_SUCCESS) { ret = EAGAIN; goto out; }
		if (Esys_PolicyPCR(ectx, session, ESYS_TR_NONE, ESYS_TR_NONE,
		    ESYS_TR_NONE, NULL, &pcrsel) != TSS2_RC_SUCCESS)
			{ ret = EPERM; goto out; }
		rc = Esys_Unseal(ectx, sealed, session, ESYS_TR_NONE,
		    ESYS_TR_NONE, &unsealed);
	} else
		rc = Esys_Unseal(ectx, sealed, ESYS_TR_PASSWORD, ESYS_TR_NONE,
		    ESYS_TR_NONE, &unsealed);

	if (rc != TSS2_RC_SUCCESS) { ret = EPERM; goto out; }
	if (unsealed->size != len) { ret = EINVAL; goto out; }
	memcpy(buf, unsealed->buffer, len);
	ret = 0;

out:
	if (unsealed != NULL) {
		__db_explicit_bzero(unsealed->buffer, unsealed->size);
		Esys_Free(unsealed);
	}
	if (pub_in != NULL) Esys_Free(pub_in);
	if (session != ESYS_TR_NONE) (void)Esys_FlushContext(ectx, session);
	if (sealed != ESYS_TR_NONE) (void)Esys_FlushContext(ectx, sealed);
	if (primary != ESYS_TR_NONE) (void)Esys_FlushContext(ectx, primary);
	if (ectx != NULL) Esys_Finalize(&ectx);
	if (fp != NULL) (void)fclose(fp);
	if (pubbuf != NULL) free(pubbuf);
	if (privbuf != NULL) free(privbuf);
	return (ret);
}

static int
__crypto_tpm_get_key(prov, key_id, buf, len)
	DB_CRYPTO_PROVIDER *prov;
	const char *key_id;
	u_int8_t *buf;
	size_t len;
{
	COMPQUIET(key_id, NULL);
	if (prov->ctx == NULL || len != DB_CRYPTO_KEK_LEN)
		return (EINVAL);
	return (__crypto_tpm_unseal((const char *)prov->ctx, buf, len));
}

/*
 * __crypto_tpm_provider --
 *	Fill in a provider struct for a TPM-sealed KEK at the given blob path.
 *
 * PUBLIC: #ifdef HAVE_TPM
 * PUBLIC: int __crypto_tpm_provider __P((DB_CRYPTO_PROVIDER *, const char *));
 * PUBLIC: #endif
 */
int
__crypto_tpm_provider(prov, blobpath)
	DB_CRYPTO_PROVIDER *prov;
	const char *blobpath;
{
	memset(prov, 0, sizeof(*prov));
	prov->provider_id = DB_CRYPTO_PROV_TPM;
	prov->ctx = (void *)blobpath;
	prov->get_key = __crypto_tpm_get_key;
	prov->release = NULL;
	prov->rotate = NULL;
	return (0);
}

/*
 * __crypto_tpm_inspect --
 *	Print the public fields of a sealed blob file (no secret).  Used by
 *	db_keytool -i.
 *
 * PUBLIC: #ifdef HAVE_TPM
 * PUBLIC: int __crypto_tpm_inspect __P((const char *));
 * PUBLIC: #endif
 */
int
__crypto_tpm_inspect(path)
	const char *path;
{
	FILE *fp;
	DB_TPM_BLOB_HDR hdr;

	if ((fp = fopen(path, "rb")) == NULL)
		return (ENOENT);
	if (fread(&hdr, 1, sizeof(hdr), fp) != sizeof(hdr) ||
	    hdr.magic != DB_TPM_BLOB_MAGIC) {
		(void)fclose(fp);
		return (EINVAL);
	}
	(void)fclose(fp);
	printf("TPM sealed blob %s:\n", path);
	printf("  public area:  %u bytes\n", hdr.pub_len);
	printf("  private area: %u bytes\n", hdr.priv_len);
	if (hdr.pcr_mask != 0)
		printf("  PCR policy:   SHA-256 over PCR mask 0x%06x\n",
		    hdr.pcr_mask & 0xffffff);
	else
		printf("  PCR policy:   none (storage-hierarchy auth only)\n");
	return (0);
}

/*
 * __crypto_tpm_seal --
 *	Seal a KEK to the TPM storage hierarchy and write the blob file.  Used
 *	by db_keytool.  If pcr_mask != 0, bind to a SHA-256 PCR policy over the
 *	selected PCRs in the low 24 bits.
 *
 * PUBLIC: #ifdef HAVE_TPM
 * PUBLIC: int __crypto_tpm_seal __P((ENV *, const char *,
 * PUBLIC:     const u_int8_t *, size_t, u_int32_t));
 * PUBLIC: #endif
 */
int
__crypto_tpm_seal(env, path, kek, keklen, pcr_mask)
	ENV *env;
	const char *path;
	const u_int8_t *kek;
	size_t keklen;
	u_int32_t pcr_mask;
{
	ESYS_CONTEXT *ectx;
	ESYS_TR primary, session;
	TPM2B_PUBLIC *pub_out, in_public, *srk_pub;
	TPM2B_PRIVATE *priv_out;
	TPM2B_SENSITIVE_CREATE sens;
	TPM2B_DATA outside;
	TPML_PCR_SELECTION creation_pcr;
	TPM2B_DIGEST *policy_digest;
	FILE *fp;
	DB_TPM_BLOB_HDR hdr;
	u_int8_t pubbuf[2048], privbuf[2048];
	size_t pub_off, priv_off;
	int ret;

	ectx = NULL; primary = session = ESYS_TR_NONE;
	pub_out = priv_out = srk_pub = NULL; policy_digest = NULL; ret = EAGAIN;
	COMPQUIET(env, NULL);

	if (keklen > sizeof(((TPM2B_SENSITIVE_DATA *)0)->buffer))
		return (EINVAL);
	if (Esys_Initialize(&ectx, NULL, NULL) != TSS2_RC_SUCCESS)
		return (EAGAIN);

	memset(&sens, 0, sizeof(sens));
	memset(&outside, 0, sizeof(outside));
	memset(&creation_pcr, 0, sizeof(creation_pcr));
	if (Esys_CreatePrimary(ectx, ESYS_TR_RH_OWNER, ESYS_TR_PASSWORD,
	    ESYS_TR_NONE, ESYS_TR_NONE, &sens, &srk_template, &outside,
	    &creation_pcr, &primary, &srk_pub, NULL, NULL, NULL)
	    != TSS2_RC_SUCCESS)
		goto out;

	/* Build the sealed (keyedhash) object holding the KEK. */
	memset(&in_public, 0, sizeof(in_public));
	in_public.publicArea.type = TPM2_ALG_KEYEDHASH;
	in_public.publicArea.nameAlg = TPM2_ALG_SHA256;
	in_public.publicArea.objectAttributes =
	    TPMA_OBJECT_FIXEDTPM | TPMA_OBJECT_FIXEDPARENT;
	in_public.publicArea.parameters.keyedHashDetail.scheme.scheme =
	    TPM2_ALG_NULL;

	if (pcr_mask != 0) {
		TPMT_SYM_DEF sym = { .algorithm = TPM2_ALG_AES,
		    .keyBits.aes = 128, .mode.aes = TPM2_ALG_CFB };
		TPML_PCR_SELECTION pcrsel;
		memset(&pcrsel, 0, sizeof(pcrsel));
		pcrsel.count = 1;
		pcrsel.pcrSelections[0].hash = TPM2_ALG_SHA256;
		pcrsel.pcrSelections[0].sizeofSelect = 3;
		pcrsel.pcrSelections[0].pcrSelect[0] = (u_int8_t)(pcr_mask);
		pcrsel.pcrSelections[0].pcrSelect[1] =
		    (u_int8_t)(pcr_mask >> 8);
		pcrsel.pcrSelections[0].pcrSelect[2] =
		    (u_int8_t)(pcr_mask >> 16);
		if (Esys_StartAuthSession(ectx, ESYS_TR_NONE, ESYS_TR_NONE,
		    ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE, NULL,
		    TPM2_SE_TRIAL, &sym, TPM2_ALG_SHA256, &session)
		    != TSS2_RC_SUCCESS)
			goto out;
		if (Esys_PolicyPCR(ectx, session, ESYS_TR_NONE, ESYS_TR_NONE,
		    ESYS_TR_NONE, NULL, &pcrsel) != TSS2_RC_SUCCESS)
			goto out;
		if (Esys_PolicyGetDigest(ectx, session, ESYS_TR_NONE,
		    ESYS_TR_NONE, ESYS_TR_NONE, &policy_digest)
		    != TSS2_RC_SUCCESS)
			goto out;
		in_public.publicArea.authPolicy = *policy_digest;
		in_public.publicArea.objectAttributes |= TPMA_OBJECT_ADMINWITHPOLICY;
	} else
		in_public.publicArea.objectAttributes |= TPMA_OBJECT_USERWITHAUTH;

	memset(&sens, 0, sizeof(sens));
	sens.sensitive.data.size = (UINT16)keklen;
	memcpy(sens.sensitive.data.buffer, kek, keklen);

	if (Esys_Create(ectx, primary, ESYS_TR_PASSWORD, ESYS_TR_NONE,
	    ESYS_TR_NONE, &sens, &in_public, &outside, &creation_pcr,
	    &priv_out, &pub_out, NULL, NULL, NULL) != TSS2_RC_SUCCESS) {
		__db_explicit_bzero(sens.sensitive.data.buffer, keklen);
		goto out;
	}
	__db_explicit_bzero(sens.sensitive.data.buffer, keklen);

	pub_off = priv_off = 0;
	if (Tss2_MU_TPM2B_PUBLIC_Marshal(pub_out, pubbuf, sizeof(pubbuf),
	    &pub_off) != TSS2_RC_SUCCESS ||
	    Tss2_MU_TPM2B_PRIVATE_Marshal(priv_out, privbuf, sizeof(privbuf),
	    &priv_off) != TSS2_RC_SUCCESS) {
		ret = EINVAL; goto out;
	}

	hdr.magic = DB_TPM_BLOB_MAGIC;
	hdr.pub_len = (u_int32_t)pub_off;
	hdr.priv_len = (u_int32_t)priv_off;
	hdr.pcr_mask = pcr_mask;
	if ((fp = fopen(path, "wb")) == NULL) { ret = __os_get_errno(); goto out; }
	if (fwrite(&hdr, 1, sizeof(hdr), fp) != sizeof(hdr) ||
	    fwrite(pubbuf, 1, pub_off, fp) != pub_off ||
	    fwrite(privbuf, 1, priv_off, fp) != priv_off)
		ret = EIO;
	else
		ret = 0;
	(void)fclose(fp);

out:
	if (policy_digest != NULL) Esys_Free(policy_digest);
	if (pub_out != NULL) Esys_Free(pub_out);
	if (priv_out != NULL) Esys_Free(priv_out);
	if (srk_pub != NULL) Esys_Free(srk_pub);
	if (session != ESYS_TR_NONE) (void)Esys_FlushContext(ectx, session);
	if (primary != ESYS_TR_NONE) (void)Esys_FlushContext(ectx, primary);
	if (ectx != NULL) Esys_Finalize(&ectx);
	return (ret);
}
#endif /* HAVE_TPM */
