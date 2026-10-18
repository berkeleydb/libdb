// SPDX-License-Identifier: Sleepycat
//! Idiomatic Rust bindings for **libdb** (the community Berkeley DB fork).
//!
//! This is the Phase-0 proof: it demonstrates the whole binding shape works
//! end to end — an owning [`Env`] and [`Db`], the error model, and a
//! borrowed-in / copy-out `put`/`get` round trip. Transactions, cursors,
//! callbacks and bulk ops land in later phases (see `rfc/0016`).
//!
//! ```no_run
//! let env = libdb::Env::create()?.cachesize(0, 64 << 20)?.open("db-home")?;
//! let db  = env.database().open_btree("data.db")?;
//! db.put(b"key", b"value")?;
//! assert_eq!(db.get(b"key")?.as_deref(), Some(&b"value"[..]));
//! # Ok::<(), libdb::Error>(())
//! ```
use core::ffi::{c_int, CStr};
use std::ffi::CString;
use std::path::Path;
use libdb_sys as ffi;

/// A libdb error: a typed DB-* code, an OS errno, or a usage error from the
/// binding itself. The control-flow negatives (`DB_NOTFOUND`, `DB_KEYEXIST`,
/// `DB_BUFFER_SMALL`) are NOT errors — they are modelled in the return types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// A deadlock; the transaction should abort and retry.
    Deadlock,
    /// The environment panicked; it must be recovered.
    RunRecovery,
    /// Any other DB-range code (<0), with its number and message.
    Db(c_int, String),
    /// An OS errno (>0), with its number and message.
    Os(c_int, String),
    /// A usage error detected in the binding (e.g. an interior NUL in a path).
    Usage(&'static str),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Deadlock => write!(f, "deadlock (retry the transaction)"),
            Error::RunRecovery => write!(f, "panic: run recovery"),
            Error::Db(c, m) | Error::Os(c, m) => write!(f, "{m} ({c})"),
            Error::Usage(s) => write!(f, "usage error: {s}"),
        }
    }
}
impl std::error::Error for Error {}

/// Map a C return code to `Result`, demoting the expected control-flow
/// negatives per the caller (which passes them through as `Ok`).
fn strerror(code: c_int) -> String {
    unsafe {
        let p = ffi::db_strerror(code);
        if p.is_null() { String::new() } else { CStr::from_ptr(p).to_string_lossy().into_owned() }
    }
}
fn check(code: c_int) -> core::result::Result<(), Error> {
    match code {
        0 => Ok(()),
        ffi::DB_LOCK_DEADLOCK => Err(Error::Deadlock),
        ffi::DB_RUNRECOVERY => Err(Error::RunRecovery),
        c if c < 0 => Err(Error::Db(c, strerror(c))),
        c => Err(Error::Os(c, strerror(c))),
    }
}
pub type Result<T> = core::result::Result<T, Error>;

fn cpath(p: &Path) -> Result<CString> {
    CString::new(p.as_os_str().to_string_lossy().into_owned())
        .map_err(|_| Error::Usage("path contains an interior NUL"))
}

/// The library version at run time, as `(major, minor, patch)`.
pub fn version() -> (i32, i32, i32) {
    let (mut a, mut b, mut c) = (0, 0, 0);
    unsafe { ffi::db_version(&mut a, &mut b, &mut c); }
    (a, b, c)
}

// --- Env --------------------------------------------------------------------

/// A database environment: the unit of caching, locking, logging and
/// transactions. Create one, configure it, open it, then open databases in it.
pub struct Env {
    ptr: *mut ffi::DB_ENV,
    open: bool,
}

/// Builder for an [`Env`] (config must happen before `open`).
pub struct EnvBuilder {
    ptr: *mut ffi::DB_ENV,
}

impl Env {
    /// Allocate a new environment handle.
    pub fn create() -> Result<EnvBuilder> {
        let mut ptr: *mut ffi::DB_ENV = core::ptr::null_mut();
        check(unsafe { ffi::db_env_create(&mut ptr, 0) })?;
        Ok(EnvBuilder { ptr })
    }
    /// Open a database in this environment.
    pub fn database(&self) -> DbBuilder<'_> {
        DbBuilder { env: self, dbptr: core::ptr::null_mut(), ty: ffi::DB_BTREE }
    }
}

impl EnvBuilder {
    /// Set the cache size (gigabytes + bytes). Must precede `open`.
    pub fn cachesize(self, gbytes: u32, bytes: u32) -> Result<Self> {
        check(unsafe { ffi::libdb_env_set_cachesize(self.ptr, gbytes, bytes, 1) })?;
        Ok(self)
    }
    /// Open the environment at `home`, creating a transactional environment.
    pub fn open<P: AsRef<Path>>(self, home: P) -> Result<Env> {
        let c = cpath(home.as_ref())?;
        let flags = ffi::DB_CREATE | ffi::DB_INIT_MPOOL | ffi::DB_INIT_LOCK
            | ffi::DB_INIT_LOG | ffi::DB_INIT_TXN | ffi::DB_THREAD;
        let r = check(unsafe { ffi::libdb_env_open(self.ptr, c.as_ptr(), flags, 0o644) });
        match r {
            Ok(()) => {
                let env = Env { ptr: self.ptr, open: true };
                core::mem::forget(self); // ownership moved into Env
                Ok(env)
            }
            Err(e) => {
                // open failed: the handle is dead; close to release it.
                unsafe { ffi::libdb_env_close(self.ptr, 0); }
                core::mem::forget(self);
                Err(e)
            }
        }
    }
}

impl Drop for EnvBuilder {
    fn drop(&mut self) {
        // Only runs if open() was never called (both paths mem::forget self).
        unsafe { ffi::libdb_env_close(self.ptr, 0); }
    }
}
impl Drop for Env {
    fn drop(&mut self) {
        if self.open { unsafe { ffi::libdb_env_close(self.ptr, 0); } }
    }
}
// Opened with DB_THREAD, so the environment is free-threaded.
unsafe impl Send for Env {}
unsafe impl Sync for Env {}

// --- Db ---------------------------------------------------------------------

/// A database (a key/value store) inside an [`Env`]. Borrows the env so it
/// cannot outlive it (the C library requires the env outlive every db).
pub struct Db<'env> {
    ptr: *mut ffi::DB,
    _env: &'env Env,
}

/// Builder for a [`Db`].
pub struct DbBuilder<'env> {
    env: &'env Env,
    dbptr: *mut ffi::DB,
    ty: c_int,
}

impl<'env> DbBuilder<'env> {
    /// Open a Btree database backed by `file`.
    pub fn open_btree<P: AsRef<Path>>(mut self, file: P) -> Result<Db<'env>> {
        self.ty = ffi::DB_BTREE;
        self.open(file)
    }
    fn open<P: AsRef<Path>>(self, file: P) -> Result<Db<'env>> {
        let mut dbp: *mut ffi::DB = core::ptr::null_mut();
        check(unsafe { ffi::db_create(&mut dbp, self.env.ptr, 0) })?;
        let c = cpath(file.as_ref())?;
        let flags = ffi::DB_CREATE | ffi::DB_AUTO_COMMIT | ffi::DB_THREAD;
        let r = check(unsafe {
            ffi::libdb_db_open(dbp, core::ptr::null_mut(), c.as_ptr(),
                core::ptr::null(), self.ty, flags, 0o644)
        });
        let _ = self.dbptr;
        match r {
            Ok(()) => Ok(Db { ptr: dbp, _env: self.env }),
            Err(e) => { unsafe { ffi::libdb_db_close(dbp, 0); } Err(e) }
        }
    }
}

impl<'env> Db<'env> {
    /// Store `data` under `key`. Overwrites an existing value (Btree default).
    pub fn put(&self, key: &[u8], data: &[u8]) -> Result<()> {
        let mut k = dbt_in(key);
        let mut d = dbt_in(data);
        check(unsafe { ffi::libdb_db_put(self.ptr, core::ptr::null_mut(), &mut k, &mut d, 0) })
    }
    /// Fetch the value under `key`. Returns `Ok(None)` if the key is absent
    /// (`DB_NOTFOUND` is not an error). The value is copied out of DB memory.
    pub fn get(&self, key: &[u8]) -> Result<Option<Vec<u8>>> {
        let mut k = dbt_in(key);
        let mut d = ffi::DBT::default();
        d.flags = ffi::DB_DBT_MALLOC; // DB allocates; we copy then free.
        let rc = unsafe { ffi::libdb_db_get(self.ptr, core::ptr::null_mut(), &mut k, &mut d, 0) };
        if rc == ffi::DB_NOTFOUND {
            return Ok(None);
        }
        check(rc)?;
        // Copy out, then free the DB-allocated buffer with libc free (the
        // default allocator, since we did not override set_alloc).
        let out = unsafe {
            if d.data.is_null() || d.size == 0 {
                Vec::new()
            } else {
                let s = core::slice::from_raw_parts(d.data as *const u8, d.size as usize);
                let v = s.to_vec();
                extern "C" { fn free(p: *mut core::ffi::c_void); }
                free(d.data);
                v
            }
        };
        Ok(Some(out))
    }
}

impl<'env> Drop for Db<'env> {
    fn drop(&mut self) { unsafe { ffi::libdb_db_close(self.ptr, 0); } }
}

/// Build a borrowed-input DBT pointing at `b` for the duration of a call.
/// Guards the `size==0`/`data==NULL` shape (a real UB source in the C library).
fn dbt_in(b: &[u8]) -> ffi::DBT {
    let mut d = ffi::DBT::default();
    if !b.is_empty() {
        d.data = b.as_ptr() as *mut core::ffi::c_void;
        d.size = b.len() as u32;
    }
    d
}
