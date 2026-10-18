// SPDX-License-Identifier: Sleepycat
//! Transactions. A [`Txn`] borrows its [`Env`] (so it cannot outlive it), is
//! resolved by [`Txn::commit`] or [`Txn::abort`] (both consume `self`, so the
//! handle is dead after — the C rule), and is `!Sync` (the C library says a
//! transaction may span threads only serially).

use core::ffi::c_int;
use core::marker::PhantomData;

use libdb_sys as ffi;

use crate::{check, Env, Error, NotSync, Result, TxnOptions};

/// A transaction. Operations take `&Txn`; resolve it with [`Txn::commit`] or
/// [`Txn::abort`]. If a `Txn` is dropped without being resolved, it is aborted.
pub struct Txn<'env> {
    ptr: *mut ffi::DB_TXN,
    env: &'env Env,
    resolved: bool,
    _not_sync: NotSync,
    _life: PhantomData<&'env Env>,
}

pub(crate) fn begin<'env>(
    env: &'env Env,
    parent: Option<&Txn<'env>>,
    opts: TxnOptions,
) -> Result<Txn<'env>> {
    let mut ptr: *mut ffi::DB_TXN = core::ptr::null_mut();
    let parentptr = parent.map_or(core::ptr::null_mut(), |p| p.ptr);
    let rc = unsafe {
        ffi::libdb_env_txn_begin(env.as_ptr(), parentptr, &mut ptr, opts.to_begin_flags())
    };
    check(rc).map_err(|e| env.enrich(e))?;
    Ok(Txn {
        ptr,
        env,
        resolved: false,
        _not_sync: PhantomData,
        _life: PhantomData,
    })
}

impl<'env> Txn<'env> {
    pub(crate) fn as_ptr(&self) -> *mut ffi::DB_TXN {
        self.ptr
    }

    /// Begin a nested child transaction under this one.
    pub fn begin_child(&self) -> Result<Txn<'env>> {
        begin(self.env, Some(self), TxnOptions::default())
    }

    /// Begin a nested child transaction with options.
    pub fn begin_child_opts(&self, opts: TxnOptions) -> Result<Txn<'env>> {
        begin(self.env, Some(self), opts)
    }

    /// Commit the transaction, making its changes durable. Consumes `self`.
    pub fn commit(mut self) -> Result<()> {
        self.resolved = true;
        let rc = unsafe { ffi::libdb_txn_commit(self.ptr, 0) };
        let env = self.env;
        core::mem::forget(self);
        check(rc).map_err(|e| env.enrich(e))
    }

    /// Commit without a synchronous log flush (faster, less durable).
    pub fn commit_nosync(mut self) -> Result<()> {
        self.resolved = true;
        let rc = unsafe { ffi::libdb_txn_commit(self.ptr, ffi::DB_TXN_NOSYNC) };
        let env = self.env;
        core::mem::forget(self);
        check(rc).map_err(|e| env.enrich(e))
    }

    /// Abort the transaction, discarding its changes. Consumes `self`.
    pub fn abort(mut self) -> Result<()> {
        self.resolved = true;
        let rc = unsafe { ffi::libdb_txn_abort(self.ptr) };
        core::mem::forget(self);
        check(rc)
    }

    /// Discard a prepared-but-unresolved (recovered) transaction. Consumes
    /// `self`.
    pub fn discard(mut self) -> Result<()> {
        self.resolved = true;
        let rc = unsafe { ffi::libdb_txn_discard(self.ptr, 0) };
        core::mem::forget(self);
        check(rc)
    }

    /// Prepare the transaction for a two-phase commit with the 128-bit global
    /// transaction id `gid` (XA).
    pub fn prepare(&self, gid: &[u8; 128]) -> Result<()> {
        let rc = unsafe { ffi::libdb_txn_prepare(self.ptr, gid.as_ptr() as *mut u8) };
        check(rc)
    }

    /// The transaction's numeric id.
    pub fn id(&self) -> u32 {
        unsafe { ffi::libdb_txn_id(self.ptr) }
    }

    /// Set a human-readable name for the transaction (shown in diagnostics).
    pub fn set_name(&self, name: &str) -> Result<()> {
        let c =
            std::ffi::CString::new(name).map_err(|_| Error::Usage("name has an interior NUL"))?;
        check(unsafe { ffi::libdb_txn_set_name(self.ptr, c.as_ptr()) })
    }

    /// Set the lock timeout for this transaction, in microseconds.
    pub fn set_lock_timeout(&self, micros: u32) -> Result<()> {
        check(unsafe { ffi::libdb_txn_set_timeout(self.ptr, micros, ffi::DB_SET_LOCK_TIMEOUT) })
    }

    /// Set the overall timeout for this transaction, in microseconds.
    pub fn set_txn_timeout(&self, micros: u32) -> Result<()> {
        check(unsafe { ffi::libdb_txn_set_timeout(self.ptr, micros, ffi::DB_SET_TXN_TIMEOUT) })
    }
}

impl<'env> Drop for Txn<'env> {
    fn drop(&mut self) {
        if !self.resolved {
            // An unresolved txn at drop is aborted (the safe default).
            let _: c_int = unsafe { ffi::libdb_txn_abort(self.ptr) };
        }
    }
}
// A Txn may move between threads but not be shared: !Sync, Send ok.
unsafe impl<'env> Send for Txn<'env> {}
