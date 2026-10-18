// SPDX-License-Identifier: Sleepycat
//! Idiomatic Rust bindings for **libdb** (the community Berkeley DB fork).
//!
//! `libdb` wraps the C library's object model — an environment, databases,
//! transactions and cursors — in owning Rust types whose destructors release
//! the underlying handle exactly once, whose lifetimes encode the C ownership
//! rules (an [`Env`] outlives its [`Db`]s, a [`Cursor`] cannot outlive its
//! [`Txn`]), and whose errors demote the expected control-flow negatives
//! (`DB_NOTFOUND` is `Ok(None)`, not an error).
//!
//! Through Phase 2: the core data path, transactions with the
//! [`Env::transact`] deadlock-retry helper, and cursors as native iterators.
//! Callbacks, secondary indices, sequences and bulk ops land in Phase 3.
//!
//! # A first round trip
//!
//! ```no_run
//! let env = libdb::Env::builder()
//!     .cachesize(0, 64 << 20)
//!     .open("db-home")?;
//! let db = env.database().btree().open("data.db")?;
//! db.put(b"key", b"value")?;
//! assert_eq!(db.get(b"key")?.as_deref(), Some(&b"value"[..]));
//! # Ok::<(), libdb::Error>(())
//! ```
#![warn(missing_docs)]

use core::ffi::{c_int, c_void, CStr};
use std::cell::RefCell;
use std::ffi::CString;
use std::marker::PhantomData;
use std::path::Path;

use libdb_sys as ffi;

mod cursor;
mod txn;

pub use cursor::{Cursor, CursorIter, Direction};
pub use txn::Txn;

// ===========================================================================
// Errors
// ===========================================================================

/// A libdb error: a typed DB-range code, an OS errno, or a usage error from
/// the binding itself.
///
/// The control-flow negatives (`DB_NOTFOUND`, `DB_KEYEXIST`,
/// `DB_BUFFER_SMALL`) are **not** errors — they are modelled in the return
/// types ([`Option`], [`Put`], [`GetInto`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// A deadlock: the transaction should abort and retry. [`Env::transact`]
    /// does this for you.
    Deadlock,
    /// A lock was unavailable (`DB_NOWAIT` / a timeout).
    LockNotGranted,
    /// The environment panicked; it must be recovered.
    RunRecovery,
    /// A secondary index is corrupt.
    SecondaryBad,
    /// Any other DB-range code (`< 0`), with its number and message. If an
    /// error callback captured a diagnostic string, it is appended.
    Db(c_int, String),
    /// An OS errno (`> 0`), with its number and message.
    Os(c_int, String),
    /// A usage error detected by the binding (e.g. an interior NUL in a path,
    /// or a callback that panicked).
    Usage(&'static str),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Deadlock => write!(f, "deadlock (retry the transaction)"),
            Error::LockNotGranted => write!(f, "lock not granted"),
            Error::RunRecovery => write!(f, "panic: run recovery"),
            Error::SecondaryBad => write!(f, "secondary index corrupt"),
            Error::Db(c, m) | Error::Os(c, m) => write!(f, "{m} ({c})"),
            Error::Usage(s) => write!(f, "usage error: {s}"),
        }
    }
}
impl std::error::Error for Error {}

/// The crate result type.
pub type Result<T> = core::result::Result<T, Error>;

fn strerror(code: c_int) -> String {
    unsafe {
        let p = ffi::db_strerror(code);
        if p.is_null() {
            String::new()
        } else {
            CStr::from_ptr(p).to_string_lossy().into_owned()
        }
    }
}

/// Map a C return code to a `Result<()>`, demoting the typed negatives the
/// caller handles separately.
fn check(code: c_int) -> Result<()> {
    match code {
        0 => Ok(()),
        ffi::DB_LOCK_DEADLOCK => Err(Error::Deadlock),
        ffi::DB_LOCK_NOTGRANTED => Err(Error::LockNotGranted),
        ffi::DB_RUNRECOVERY => Err(Error::RunRecovery),
        ffi::DB_SECONDARY_BAD => Err(Error::SecondaryBad),
        c if c < 0 => Err(Error::Db(c, strerror(c))),
        c => Err(Error::Os(c, strerror(c))),
    }
}

fn cpath(p: &Path) -> Result<CString> {
    CString::new(p.as_os_str().to_string_lossy().into_owned())
        .map_err(|_| Error::Usage("path contains an interior NUL"))
}

// ===========================================================================
// Version
// ===========================================================================

/// The library version at run time, as `(major, minor, patch)`.
///
/// ```
/// let (major, _minor, _patch) = libdb::version();
/// assert_eq!(major, 2026);
/// ```
pub fn version() -> (i32, i32, i32) {
    let (mut a, mut b, mut c) = (0, 0, 0);
    unsafe {
        ffi::db_version(&mut a, &mut b, &mut c);
    }
    (a, b, c)
}

/// Check that the library linked at run time matches the `db.h` compiled
/// against on the format/ABI level (major + minor). Errors on skew.
///
/// ```
/// libdb::check_version().expect("header/library version skew");
/// ```
pub fn check_version() -> Result<()> {
    let (rt_major, rt_minor, _) = version();
    let (ct_major, ct_minor) = (ffi::DB_VERSION_MAJOR, ffi::DB_VERSION_MINOR);
    if rt_major != ct_major || rt_minor != ct_minor {
        return Err(Error::Db(
            ffi::DB_RUNRECOVERY,
            format!(
                "libdb version skew: compiled against {ct_major}.{ct_minor}, \
                 linked {rt_major}.{rt_minor}"
            ),
        ));
    }
    Ok(())
}

// ===========================================================================
// DBT helpers
// ===========================================================================

/// Build a borrowed-input DBT pointing at `b` for the duration of a call.
/// Guards the `size==0`/`data==NULL` shape (a real UB source in the library).
pub(crate) fn dbt_in(b: &[u8]) -> ffi::DBT {
    let mut d = ffi::DBT::default();
    if !b.is_empty() {
        d.data = b.as_ptr() as *mut c_void;
        d.size = b.len() as u32;
    }
    d
}

/// A fresh output DBT carrying only the given flags.
pub(crate) fn dbt_out(flags: ffi::u_int32_t) -> ffi::DBT {
    ffi::DBT {
        flags,
        ..Default::default()
    }
}

/// A USERMEM output DBT pointing into the caller's buffer.
pub(crate) fn dbt_usermem(buf: &mut [u8]) -> ffi::DBT {
    ffi::DBT {
        data: buf.as_mut_ptr() as *mut c_void,
        ulen: buf.len() as u32,
        flags: ffi::DB_DBT_USERMEM,
        ..Default::default()
    }
}

/// Copy a DB-owned (MALLOC) output DBT into an owned `Vec`, freeing the DB
/// buffer. Safe for the empty shape.
pub(crate) unsafe fn take_malloc(d: &ffi::DBT) -> Vec<u8> {
    if d.data.is_null() || d.size == 0 {
        return Vec::new();
    }
    let s = core::slice::from_raw_parts(d.data as *const u8, d.size as usize);
    let v = s.to_vec();
    extern "C" {
        fn free(p: *mut c_void);
    }
    free(d.data);
    v
}

/// The result of a `put` with `NOOVERWRITE`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Put {
    /// The pair was stored.
    Stored,
    /// A value already existed under the key; nothing was changed.
    Exists,
}

/// A `get_into` outcome.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GetInto {
    /// The key was not found.
    NotFound,
    /// The value fit; this many bytes were written into the caller's buffer.
    Fit(usize),
    /// The buffer was too small; the value needs this many bytes.
    TooSmall(usize),
}

// ===========================================================================
// Transaction options
// ===========================================================================

/// Options for [`Env::begin_opts`].
#[derive(Debug, Clone, Copy, Default)]
pub struct TxnOptions {
    /// Use snapshot (MVCC) isolation for this transaction.
    pub snapshot: bool,
    /// Do not block on a lock; fail instead of waiting.
    pub nowait: bool,
    /// Do not flush the log on commit (faster, less durable).
    pub nosync: bool,
}

impl TxnOptions {
    pub(crate) fn to_begin_flags(self) -> ffi::u_int32_t {
        let mut f = 0;
        if self.snapshot {
            f |= ffi::DB_TXN_SNAPSHOT;
        }
        if self.nowait {
            f |= ffi::DB_TXN_NOWAIT;
        }
        if self.nosync {
            f |= ffi::DB_TXN_NOSYNC;
        }
        f
    }
}

/// A marker type making a struct `!Sync` (shared by `Txn` and `Cursor`).
pub(crate) type NotSync = PhantomData<*const ()>;

// ===========================================================================
// Env
// ===========================================================================

/// A database environment. Create one via [`Env::builder`], configure it, open
/// it, then open databases and begin transactions in it.
///
/// Opened with `DB_THREAD`, so [`Send`] + [`Sync`].
pub struct Env {
    ptr: *mut ffi::DB_ENV,
    open: bool,
    diag: Box<RefCell<Option<String>>>,
}

/// Builder for an [`Env`].
pub struct EnvBuilder {
    ptr: *mut ffi::DB_ENV,
    extra_flags: ffi::u_int32_t,
    diag: Box<RefCell<Option<String>>>,
    capture_diag: bool,
    err: Option<Error>,
}

impl Env {
    /// Begin building a new environment.
    pub fn builder() -> EnvBuilder {
        let mut ptr: *mut ffi::DB_ENV = core::ptr::null_mut();
        let rc = unsafe { ffi::db_env_create(&mut ptr, 0) };
        EnvBuilder {
            ptr,
            extra_flags: 0,
            diag: Box::new(RefCell::new(None)),
            capture_diag: false,
            err: check(rc).err(),
        }
    }

    /// Open a database in this environment.
    pub fn database(&self) -> DbBuilder<'_> {
        DbBuilder {
            env: self,
            ty: ffi::DB_BTREE,
            pagesize: 0,
            set_flags: 0,
            err: None,
        }
    }

    /// Begin a top-level transaction.
    pub fn begin(&self) -> Result<Txn<'_>> {
        self.begin_opts(None, TxnOptions::default())
    }

    /// Begin a transaction with explicit options (parent, snapshot, nowait).
    pub fn begin_opts<'env>(
        &'env self,
        parent: Option<&Txn<'env>>,
        opts: TxnOptions,
    ) -> Result<Txn<'env>> {
        txn::begin(self, parent, opts)
    }

    /// Run `body` inside a transaction, committing on `Ok` and aborting on
    /// `Err`. On [`Error::Deadlock`] the transaction is aborted and the body
    /// retried, up to `retries` times (then the deadlock is returned).
    ///
    /// The deadlock-retry loop every transactional application needs, once.
    ///
    /// ```no_run
    /// # let env = libdb::Env::builder().open("h")?;
    /// # let db = env.database().btree().open("d.db")?;
    /// env.transact(10, |txn| {
    ///     db.tput(txn, b"n", b"1")?;
    ///     Ok(())
    /// })?;
    /// # Ok::<(), libdb::Error>(())
    /// ```
    pub fn transact<F, T>(&self, retries: u32, mut body: F) -> Result<T>
    where
        F: FnMut(&Txn<'_>) -> Result<T>,
    {
        let mut attempt = 0u32;
        loop {
            let txn = self.begin()?;
            match body(&txn) {
                Ok(val) => {
                    txn.commit()?;
                    return Ok(val);
                }
                Err(Error::Deadlock) => {
                    txn.abort()?;
                    if attempt >= retries {
                        return Err(Error::Deadlock);
                    }
                    attempt += 1;
                }
                Err(e) => {
                    txn.abort()?;
                    return Err(e);
                }
            }
        }
    }

    /// Flush the transaction log and checkpoint the environment.
    pub fn checkpoint(&self) -> Result<()> {
        check(unsafe { ffi::libdb_env_txn_checkpoint(self.ptr, 0, 0, 0) })
    }

    /// The last diagnostic string the error callback captured, if any.
    pub fn last_diagnostic(&self) -> Option<String> {
        self.diag.borrow_mut().take()
    }

    pub(crate) fn as_ptr(&self) -> *mut ffi::DB_ENV {
        self.ptr
    }

    fn take_diag(&self) -> Option<String> {
        self.diag.borrow_mut().take()
    }

    pub(crate) fn enrich(&self, e: Error) -> Error {
        if let (Error::Db(c, m), Some(d)) = (&e, self.take_diag()) {
            Error::Db(*c, format!("{m}: {d}"))
        } else {
            e
        }
    }
}

impl EnvBuilder {
    /// Set the cache size (gigabytes + bytes).
    pub fn cachesize(mut self, gbytes: u32, bytes: u32) -> Self {
        if self.err.is_none() {
            if let Err(e) =
                check(unsafe { ffi::libdb_env_set_cachesize(self.ptr, gbytes, bytes, 1) })
            {
                self.err = Some(e);
            }
        }
        self
    }

    /// Turn an environment behavior flag on or off.
    pub fn set_flag(mut self, flag: u32, onoff: bool) -> Self {
        if self.err.is_none() {
            if let Err(e) =
                check(unsafe { ffi::libdb_env_set_flags(self.ptr, flag, onoff as c_int) })
            {
                self.err = Some(e);
            }
        }
        self
    }

    /// Set the error-message prefix libdb prepends to diagnostics.
    pub fn errpfx(self, pfx: &str) -> Self {
        if let Ok(c) = CString::new(pfx) {
            unsafe { ffi::libdb_env_set_errpfx(self.ptr, c.as_ptr()) };
        }
        self
    }

    /// Capture libdb's diagnostic strings into [`Env::last_diagnostic`] and the
    /// error returned by a failing call.
    pub fn capture_diagnostics(mut self) -> Self {
        self.capture_diag = true;
        self
    }

    /// Route libdb's informational messages to stderr via the message callback.
    pub fn log_messages(self) -> Self {
        unsafe { ffi::libdb_env_set_msgcall(self.ptr, Some(msgcall_thunk)) };
        self
    }

    /// OR extra flags into the environment `open` call (advanced).
    pub fn extra_open_flags(mut self, flags: u32) -> Self {
        self.extra_flags |= flags;
        self
    }

    /// Open the environment at `home` as a transactional, free-threaded
    /// environment, creating it if necessary.
    pub fn open<P: AsRef<Path>>(mut self, home: P) -> Result<Env> {
        if let Some(e) = self.err.take() {
            unsafe { ffi::libdb_env_close(self.ptr, 0) };
            core::mem::forget(self);
            return Err(e);
        }
        let c = match cpath(home.as_ref()) {
            Ok(c) => c,
            Err(e) => {
                unsafe { ffi::libdb_env_close(self.ptr, 0) };
                core::mem::forget(self);
                return Err(e);
            }
        };
        if self.capture_diag {
            let p = (&*self.diag) as *const RefCell<Option<String>> as *mut c_void;
            unsafe {
                ffi::libdb_env_set_app_private(self.ptr, p);
                ffi::libdb_env_set_errcall(self.ptr, Some(errcall_thunk));
            }
        }
        let flags = ffi::DB_CREATE
            | ffi::DB_INIT_MPOOL
            | ffi::DB_INIT_LOCK
            | ffi::DB_INIT_LOG
            | ffi::DB_INIT_TXN
            | ffi::DB_THREAD
            | self.extra_flags;
        let r = check(unsafe { ffi::libdb_env_open(self.ptr, c.as_ptr(), flags, 0o644) });
        match r {
            Ok(()) => {
                let diag = std::mem::replace(&mut self.diag, Box::new(RefCell::new(None)));
                let env = Env {
                    ptr: self.ptr,
                    open: true,
                    diag,
                };
                core::mem::forget(self);
                Ok(env)
            }
            Err(e) => {
                let e = if let (Error::Db(c, m), Some(d)) = (&e, self.diag.borrow_mut().take()) {
                    Error::Db(*c, format!("{m}: {d}"))
                } else {
                    e
                };
                unsafe { ffi::libdb_env_close(self.ptr, 0) };
                core::mem::forget(self);
                Err(e)
            }
        }
    }
}

impl Drop for EnvBuilder {
    fn drop(&mut self) {
        unsafe { ffi::libdb_env_close(self.ptr, 0) };
    }
}
impl Drop for Env {
    fn drop(&mut self) {
        if self.open {
            unsafe { ffi::libdb_env_close(self.ptr, 0) };
        }
    }
}
unsafe impl Send for Env {}
unsafe impl Sync for Env {}

extern "C" fn errcall_thunk(
    env: *const ffi::DB_ENV,
    _pfx: *const core::ffi::c_char,
    msg: *const core::ffi::c_char,
) {
    let _ = std::panic::catch_unwind(|| unsafe {
        let p = ffi::libdb_env_get_app_private(env as *mut ffi::DB_ENV);
        if p.is_null() || msg.is_null() {
            return;
        }
        let cell = &*(p as *const RefCell<Option<String>>);
        let s = CStr::from_ptr(msg).to_string_lossy().into_owned();
        *cell.borrow_mut() = Some(s);
    });
}

extern "C" fn msgcall_thunk(_env: *const ffi::DB_ENV, msg: *const core::ffi::c_char) {
    let _ = std::panic::catch_unwind(|| unsafe {
        if !msg.is_null() {
            eprintln!("libdb: {}", CStr::from_ptr(msg).to_string_lossy());
        }
    });
}

/// Environment and database flag constants.
pub mod flag {
    use super::ffi;
    /// Keep multiple versions of data (MVCC); enables snapshot isolation.
    pub const MULTIVERSION: u32 = ffi::DB_MULTIVERSION;
    /// Do not flush the log synchronously on commit.
    pub const TXN_NOSYNC: u32 = ffi::DB_TXN_NOSYNC;
    /// Default new transactions to snapshot (MVCC) isolation.
    pub const TXN_SNAPSHOT: u32 = ffi::DB_TXN_SNAPSHOT;
}

// ===========================================================================
// Db
// ===========================================================================

/// A database inside an [`Env`]. Borrows the env so it cannot outlive it.
///
/// Free-threaded (opened with `DB_THREAD`): [`Send`] + [`Sync`].
pub struct Db<'env> {
    ptr: *mut ffi::DB,
    env: &'env Env,
}

/// The database access-method type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DbType {
    /// Sorted balanced tree (the default).
    Btree,
    /// Extended linear hash.
    Hash,
    /// Record-number indexed, variable length.
    Recno,
    /// Fixed-length records, queue semantics.
    Queue,
}
impl DbType {
    fn to_ffi(self) -> c_int {
        match self {
            DbType::Btree => ffi::DB_BTREE,
            DbType::Hash => ffi::DB_HASH,
            DbType::Recno => ffi::DB_RECNO,
            DbType::Queue => ffi::DB_QUEUE,
        }
    }
}

/// Builder for a [`Db`].
pub struct DbBuilder<'env> {
    env: &'env Env,
    ty: c_int,
    pagesize: ffi::u_int32_t,
    set_flags: ffi::u_int32_t,
    err: Option<Error>,
}

impl<'env> DbBuilder<'env> {
    /// Make this a Btree database (the default).
    pub fn btree(mut self) -> Self {
        self.ty = ffi::DB_BTREE;
        self
    }
    /// Make this a Hash database.
    pub fn hash(mut self) -> Self {
        self.ty = ffi::DB_HASH;
        self
    }
    /// Make this a Recno (record-number) database.
    pub fn recno(mut self) -> Self {
        self.ty = ffi::DB_RECNO;
        self
    }
    /// Make this a Queue database.
    pub fn queue(mut self) -> Self {
        self.ty = ffi::DB_QUEUE;
        self
    }
    /// Set the access-method type explicitly.
    pub fn db_type(mut self, ty: DbType) -> Self {
        self.ty = ty.to_ffi();
        self
    }
    /// Set the page size in bytes (power of two, 512..64K).
    pub fn pagesize(mut self, bytes: u32) -> Self {
        self.pagesize = bytes;
        self
    }
    /// Allow unsorted duplicate data items for a key.
    pub fn duplicates(mut self) -> Self {
        self.set_flags |= ffi::DB_DUP;
        self
    }
    /// Allow sorted duplicate data items for a key.
    pub fn sorted_duplicates(mut self) -> Self {
        self.set_flags |= ffi::DB_DUP | ffi::DB_DUPSORT;
        self
    }

    /// Open the database backed by `file`.
    pub fn open<P: AsRef<Path>>(self, file: P) -> Result<Db<'env>> {
        self.open_inner(file.as_ref(), None)
    }

    /// Open the database inside a transaction.
    pub fn open_txn<P: AsRef<Path>>(self, txn: &Txn<'env>, file: P) -> Result<Db<'env>> {
        let f = file.as_ref().to_path_buf();
        self.open_inner(&f, Some(txn))
    }

    fn open_inner(mut self, file: &Path, txn: Option<&Txn<'env>>) -> Result<Db<'env>> {
        if let Some(e) = self.err.take() {
            return Err(e);
        }
        let mut dbp: *mut ffi::DB = core::ptr::null_mut();
        check(unsafe { ffi::db_create(&mut dbp, self.env.ptr, 0) })?;

        if self.pagesize != 0 {
            if let Err(e) = check(unsafe { ffi::libdb_db_set_pagesize(dbp, self.pagesize) }) {
                unsafe { ffi::libdb_db_close(dbp, 0) };
                return Err(e);
            }
        }
        if self.set_flags != 0 {
            if let Err(e) = check(unsafe { ffi::libdb_db_set_flags(dbp, self.set_flags) }) {
                unsafe { ffi::libdb_db_close(dbp, 0) };
                return Err(e);
            }
        }

        let c = cpath(file)?;
        // In a txn, DB_AUTO_COMMIT would conflict with the explicit txn.
        let flags = if txn.is_some() {
            ffi::DB_CREATE | ffi::DB_THREAD
        } else {
            ffi::DB_CREATE | ffi::DB_AUTO_COMMIT | ffi::DB_THREAD
        };
        let txnptr = txn.map_or(core::ptr::null_mut(), |t| t.as_ptr());
        let r = check(unsafe {
            ffi::libdb_db_open(
                dbp,
                txnptr,
                c.as_ptr(),
                core::ptr::null(),
                self.ty,
                flags,
                0o644,
            )
        });
        match r {
            Ok(()) => Ok(Db {
                ptr: dbp,
                env: self.env,
            }),
            Err(e) => {
                let e = self.env.enrich(e);
                unsafe { ffi::libdb_db_close(dbp, 0) };
                Err(e)
            }
        }
    }
}

impl<'env> Db<'env> {
    pub(crate) fn as_ptr(&self) -> *mut ffi::DB {
        self.ptr
    }
    pub(crate) fn env(&self) -> &'env Env {
        self.env
    }

    // --- put ---------------------------------------------------------------

    /// Store `data` under `key`, overwriting any existing value.
    pub fn put(&self, key: &[u8], data: &[u8]) -> Result<()> {
        self.put_flags(core::ptr::null_mut(), key, data, 0)
            .map(|_| ())
    }

    /// Store `data` under `key` in transaction `txn`, overwriting.
    pub fn tput(&self, txn: &Txn<'_>, key: &[u8], data: &[u8]) -> Result<()> {
        self.put_flags(txn.as_ptr(), key, data, 0).map(|_| ())
    }

    /// Store only if `key` is absent (`DB_NOOVERWRITE`).
    pub fn put_no_overwrite(&self, key: &[u8], data: &[u8]) -> Result<Put> {
        self.put_flags(core::ptr::null_mut(), key, data, ffi::DB_NOOVERWRITE)
    }

    /// Transactional [`Db::put_no_overwrite`].
    pub fn tput_no_overwrite(&self, txn: &Txn<'_>, key: &[u8], data: &[u8]) -> Result<Put> {
        self.put_flags(txn.as_ptr(), key, data, ffi::DB_NOOVERWRITE)
    }

    fn put_flags(
        &self,
        txn: *mut ffi::DB_TXN,
        key: &[u8],
        data: &[u8],
        flags: ffi::u_int32_t,
    ) -> Result<Put> {
        let mut k = dbt_in(key);
        let mut d = dbt_in(data);
        let rc = unsafe { ffi::libdb_db_put(self.ptr, txn, &mut k, &mut d, flags) };
        if rc == ffi::DB_KEYEXIST {
            return Ok(Put::Exists);
        }
        check(rc).map_err(|e| self.env.enrich(e))?;
        Ok(Put::Stored)
    }

    // --- get (copy-out) ----------------------------------------------------

    /// Fetch the value under `key`, copied out. `Ok(None)` if absent.
    pub fn get(&self, key: &[u8]) -> Result<Option<Vec<u8>>> {
        self.get_tx(core::ptr::null_mut(), key)
    }

    /// Transactional [`Db::get`].
    pub fn tget(&self, txn: &Txn<'_>, key: &[u8]) -> Result<Option<Vec<u8>>> {
        self.get_tx(txn.as_ptr(), key)
    }

    fn get_tx(&self, txn: *mut ffi::DB_TXN, key: &[u8]) -> Result<Option<Vec<u8>>> {
        let mut k = dbt_in(key);
        let mut d = dbt_out(ffi::DB_DBT_MALLOC);
        let rc = unsafe { ffi::libdb_db_get(self.ptr, txn, &mut k, &mut d, 0) };
        if rc == ffi::DB_NOTFOUND {
            return Ok(None);
        }
        check(rc).map_err(|e| self.env.enrich(e))?;
        Ok(Some(unsafe { take_malloc(&d) }))
    }

    // --- get_into (zero-alloc USERMEM) -------------------------------------

    /// Fetch the value under `key` into the caller's buffer with no allocation.
    pub fn get_into(&self, key: &[u8], buf: &mut [u8]) -> Result<GetInto> {
        let mut k = dbt_in(key);
        let mut d = dbt_usermem(buf);
        let rc = unsafe { ffi::libdb_db_get(self.ptr, core::ptr::null_mut(), &mut k, &mut d, 0) };
        if rc == ffi::DB_NOTFOUND {
            return Ok(GetInto::NotFound);
        }
        if rc == ffi::DB_BUFFER_SMALL {
            return Ok(GetInto::TooSmall(d.size as usize));
        }
        check(rc).map_err(|e| self.env.enrich(e))?;
        Ok(GetInto::Fit(d.size as usize))
    }

    /// Like [`Db::get_into`] but grows `buf` and retries on a too-small buffer.
    pub fn get_into_grow(&self, key: &[u8], buf: &mut Vec<u8>) -> Result<Option<usize>> {
        if buf.is_empty() {
            buf.resize(32, 0);
        }
        match self.get_into(key, buf.as_mut_slice())? {
            GetInto::NotFound => Ok(None),
            GetInto::Fit(n) => Ok(Some(n)),
            GetInto::TooSmall(need) => {
                buf.resize(need, 0);
                match self.get_into(key, buf.as_mut_slice())? {
                    GetInto::Fit(n) => Ok(Some(n)),
                    GetInto::NotFound => Ok(None),
                    GetInto::TooSmall(_) => Err(Error::Usage("value grew between retries")),
                }
            }
        }
    }

    // --- with_value (borrowed closure) -------------------------------------

    /// Call `f` with a borrowed view of the value under `key`, valid only for
    /// the duration of the call. Zero heap allocation up to 256 bytes.
    pub fn with_value<R, F>(&self, key: &[u8], f: F) -> Result<Option<R>>
    where
        F: FnOnce(&[u8]) -> R,
    {
        let mut stack = [0u8; 256];
        match self.get_into(key, &mut stack)? {
            GetInto::NotFound => Ok(None),
            GetInto::Fit(n) => Ok(Some(f(&stack[..n]))),
            GetInto::TooSmall(need) => {
                let mut heap = vec![0u8; need];
                match self.get_into(key, &mut heap)? {
                    GetInto::Fit(n) => Ok(Some(f(&heap[..n]))),
                    GetInto::NotFound => Ok(None),
                    GetInto::TooSmall(_) => Err(Error::Usage("value grew between retries")),
                }
            }
        }
    }

    // --- del / exists ------------------------------------------------------

    /// Delete the pair under `key`. `Ok(false)` if absent.
    pub fn del(&self, key: &[u8]) -> Result<bool> {
        self.del_tx(core::ptr::null_mut(), key)
    }
    /// Transactional [`Db::del`].
    pub fn tdel(&self, txn: &Txn<'_>, key: &[u8]) -> Result<bool> {
        self.del_tx(txn.as_ptr(), key)
    }
    fn del_tx(&self, txn: *mut ffi::DB_TXN, key: &[u8]) -> Result<bool> {
        let mut k = dbt_in(key);
        let rc = unsafe { ffi::libdb_db_del(self.ptr, txn, &mut k, 0) };
        if rc == ffi::DB_NOTFOUND {
            return Ok(false);
        }
        check(rc).map_err(|e| self.env.enrich(e))?;
        Ok(true)
    }

    /// Test whether `key` exists.
    pub fn exists(&self, key: &[u8]) -> Result<bool> {
        self.exists_tx(core::ptr::null_mut(), key)
    }
    /// Transactional [`Db::exists`].
    pub fn texists(&self, txn: &Txn<'_>, key: &[u8]) -> Result<bool> {
        self.exists_tx(txn.as_ptr(), key)
    }
    fn exists_tx(&self, txn: *mut ffi::DB_TXN, key: &[u8]) -> Result<bool> {
        let mut k = dbt_in(key);
        let rc = unsafe { ffi::libdb_db_exists(self.ptr, txn, &mut k, 0) };
        if rc == ffi::DB_NOTFOUND {
            return Ok(false);
        }
        check(rc).map_err(|e| self.env.enrich(e))?;
        Ok(true)
    }

    // --- cursors -----------------------------------------------------------

    /// Open a cursor over this database, not in any transaction.
    pub fn cursor(&self) -> Result<Cursor<'_, '_>> {
        cursor::open(self, None)
    }

    /// Open a cursor over this database in transaction `txn`. The cursor
    /// borrows both the txn and the db, so the compiler refuses to let it
    /// outlive the transaction — the exact C rule, enforced for free.
    pub fn cursor_txn<'txn>(&self, txn: &'txn Txn<'env>) -> Result<Cursor<'txn, '_>>
    where
        'env: 'txn,
    {
        cursor::open(self, Some(txn))
    }

    /// Flush any cached database pages to the backing file.
    pub fn sync(&self) -> Result<()> {
        check(unsafe { ffi::libdb_db_sync(self.ptr, 0) })
    }
}

impl<'env> Drop for Db<'env> {
    fn drop(&mut self) {
        unsafe { ffi::libdb_db_close(self.ptr, 0) };
    }
}
unsafe impl<'env> Send for Db<'env> {}
unsafe impl<'env> Sync for Db<'env> {}
