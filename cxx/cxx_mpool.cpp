/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2001
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: cxx_mpool.cpp,v 11.14 2001/07/24 22:37:06 dda Exp $";
#endif /* not lint */

#include <errno.h>

#include "db_cxx.h"
#include "cxx_int.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            DbMpoolFile                             //
//                                                                    //
////////////////////////////////////////////////////////////////////////

DbMpoolFile::DbMpoolFile()
:	imp_(0)
{
}

DbMpoolFile::~DbMpoolFile()
{
}

int DbMpoolFile::close(u_int32_t flags)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->close(mpf, flags)) != 0) {
		DB_ERROR("DbMpoolFile::close", err, ON_ERROR_UNKNOWN);
	}

	if (err == 0) {
		imp_ = 0;                   // extra safety

		// This may seem weird, but is legal as long as we don't access
		// any data before returning.
		//
		delete this;
	}
	return (err);
}

int DbMpoolFile::get(db_pgno_t *pgnoaddr, u_int32_t flags, void *pagep)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->get(mpf, pgnoaddr, flags, pagep)) != 0) {
		DB_ERROR("DbMpoolFile::get", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}

void DbMpoolFile::last_pgno(db_pgno_t *pgnoaddr)
{
	DB_MPOOLFILE *mpf;

	mpf = unwrap(this);
	mpf->last_pgno(mpf, pgnoaddr);
}

int DbMpoolFile::open(const char *file, u_int32_t flags, int mode, size_t pagesize)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->open(mpf, file, flags, mode, pagesize)) != 0) {
		DB_ERROR("DbMpoolFile::open", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}

int DbMpoolFile::put(void *pgaddr, u_int32_t flags)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->put(mpf, pgaddr, flags)) != 0) {
		DB_ERROR("DbMpoolFile::put", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}

void DbMpoolFile::refcnt(db_pgno_t *pgnoaddr)
{
	DB_MPOOLFILE *mpf;

	mpf = unwrap(this);
	mpf->refcnt(mpf, pgnoaddr);
}

int DbMpoolFile::set(void *pgaddr, u_int32_t flags)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->set(mpf, pgaddr, flags)) != 0) {
		DB_ERROR("DbMpoolFile::set", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}

int DbMpoolFile::set_clear_len(u_int32_t len)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->set_clear_len(mpf, len)) != 0) {
		DB_ERROR("DbMpoolFile::set_clear_len", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}

int DbMpoolFile::set_fileid(u_int8_t *fileid)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->set_fileid(mpf, fileid)) != 0) {
		DB_ERROR("DbMpoolFile::set_fileid", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}

int DbMpoolFile::set_ftype(int ftype)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->set_ftype(mpf, ftype)) != 0) {
		DB_ERROR("DbMpoolFile::set_ftype", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}

int DbMpoolFile::set_lsn_offset(int32_t offset)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->set_lsn_offset(mpf, offset)) != 0) {
		DB_ERROR("DbMpoolFile::set_lsn_offset", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}

int DbMpoolFile::set_pgcookie(DBT *dbt)
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->set_pgcookie(mpf, dbt)) != 0) {
		DB_ERROR("DbMpoolFile::set_pgcookie", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}

void DbMpoolFile::set_unlink(int ul)
{
	DB_MPOOLFILE *mpf;

	mpf = unwrap(this);
	mpf->set_unlink(mpf, ul);
}

int DbMpoolFile::sync()
{
	DB_MPOOLFILE *mpf;
	int err;

	mpf = unwrap(this);
	if (mpf == NULL) {
		err = EINVAL;
	}
	else if ((err = mpf->sync(mpf)) != 0) {
		DB_ERROR("DbMpoolFile::sync", err, ON_ERROR_UNKNOWN);
	}
	return (err);
}
