// SPDX-License-Identifier: Sleepycat
//! 64-bit sequences. A [`Sequence`] is a persistent 64-bit counter stored
//! under a key in a database; [`Sequence::next`] atomically returns and
//! advances it, caching a block of values for speed.

use libdb_sys as ffi;

use crate::{check, dbt_in, Db, Result};

/// A persistent 64-bit sequence stored in a [`Db`].
pub struct Sequence<'db> {
    ptr: *mut ffi::DB_SEQUENCE,
    _db: core::marker::PhantomData<&'db ()>,
}

pub(crate) fn open<'db>(db: &'db Db<'_>, key: &[u8]) -> Result<Sequence<'db>> {
    let mut seqp: *mut ffi::DB_SEQUENCE = core::ptr::null_mut();
    check(unsafe { ffi::db_sequence_create(&mut seqp, db.as_ptr(), 0) })?;
    // Default to an initial value of 1 and create-if-absent.
    let rc_init = unsafe { ffi::libdb_seq_initial_value(seqp, 1) };
    if let Err(e) = check(rc_init) {
        unsafe { ffi::libdb_seq_close(seqp, 0) };
        return Err(e);
    }
    let mut k = dbt_in(key);
    let rc = unsafe { ffi::libdb_seq_open(seqp, core::ptr::null_mut(), &mut k, ffi::DB_CREATE) };
    match check(rc) {
        Ok(()) => Ok(Sequence {
            ptr: seqp,
            _db: core::marker::PhantomData,
        }),
        Err(e) => {
            unsafe { ffi::libdb_seq_close(seqp, 0) };
            Err(e)
        }
    }
}

impl<'db> Sequence<'db> {
    /// Atomically return the current value and advance by 1.
    pub fn next(&self) -> Result<i64> {
        self.advance(1)
    }

    /// Atomically return the current value and advance by `delta`.
    pub fn advance(&self, delta: i32) -> Result<i64> {
        let mut out: ffi::db_seq_t = 0;
        check(unsafe { ffi::libdb_seq_get(self.ptr, core::ptr::null_mut(), delta, &mut out, 0) })?;
        Ok(out)
    }

    /// Set how many values are cached per allocation (fewer DB writes).
    pub fn set_cache(&self, size: i32) -> Result<()> {
        check(unsafe { ffi::libdb_seq_set_cachesize(self.ptr, size) })
    }

    /// Remove the sequence from its database. Consumes `self`.
    pub fn remove(self) -> Result<()> {
        let rc = unsafe { ffi::libdb_seq_remove(self.ptr, core::ptr::null_mut(), 0) };
        // remove also closes the handle; forget to avoid a double close.
        core::mem::forget(self);
        check(rc)
    }
}

impl<'db> Drop for Sequence<'db> {
    fn drop(&mut self) {
        unsafe { ffi::libdb_seq_close(self.ptr, 0) };
    }
}
// A sequence is !Sync (uses the DB handle serially), Send ok.
unsafe impl<'db> Send for Sequence<'db> {}
