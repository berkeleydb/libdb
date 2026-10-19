// SPDX-License-Identifier: Sleepycat
//! Idiomatic Rust bindings for **libdb** (the community Berkeley DB fork).
//!
//! `libdb` wraps the C library's object model — an environment, databases,
//! transactions, cursors and sequences — in owning Rust types whose
//! destructors release the underlying handle exactly once, whose lifetimes
//! encode the C ownership rules (an [`Env`] outlives its [`Db`]s, a [`Cursor`]
//! cannot outlive its [`Txn`]), and whose errors demote the expected
//! control-flow negatives (`DB_NOTFOUND` is `Ok(None)`, not an error).
//!
//! The value-facing API offers three output forms ([`Db::get`] copy-out,
//! [`Db::get_into`] zero-alloc, [`Db::with_value`] borrowed), transactions with
//! the [`Env::transact`] deadlock-retry helper, cursors as native iterators,
//! comparators and a secondary index, 64-bit [`Sequence`]s, and zero-copy bulk
//! retrieval. See the `guide/` for a chapter tour and `MAPPING.md` for the
//! C-method cross-walk.
//!
//! # A first round trip
//!
//! ```
//! # let dir = std::env::temp_dir().join(format!("libdb_doc_{}", std::process::id()));
//! # std::fs::create_dir_all(&dir).unwrap();
//! let env = libdb::Env::builder()
//!     .cachesize(0, 16 << 20)
//!     .open(&dir)?;
//! let db = env.database().btree().open("data.db")?;
//! db.put(b"key", b"value")?;
//! assert_eq!(db.get(b"key")?.as_deref(), Some(&b"value"[..]));
//! # Ok::<(), libdb::Error>(())
//! ```
//!
//! # Allocator invariant
//!
//! This crate hands libdb memory (an `associate` secondary-key buffer) and
//! receives libdb-allocated memory (copy-out getters), and it allocates/frees
//! both through libdb's **own** allocator (`libdb_umalloc`/`libdb_ufree` in the
//! shim, i.e. `__os_umalloc`/`__os_ufree`). This matches the process-global
//! allocator hooks (`db_env_set_func_malloc`/`free`) and otherwise libc
//! `malloc`/`free`. The one allocator configuration that would break this is a
//! **per-env** allocator (`DB_ENV->set_alloc`), which libdb's own
//! `FREE_IF_NEEDED` threads an env through: this crate must never configure one
//! (the safe API does not expose it, and the shim does not wrap it).
//!
//! # Thread safety
//!
//! [`Env`] and [`Db`] are opened with `DB_THREAD` and are [`Send`] + [`Sync`]:
//! the handles may be shared across threads. **Transactions and cursors are
//! not** — the C library requires a transaction (and every cursor opened under
//! it) to be used by a single thread. [`Txn`] is [`Send`] but `!Sync` (it may
//! be *moved* to another thread and used serially, never *shared*), and a
//! [`Cursor`] is `!Send` (it borrows the `Txn`, and `&Txn` is `!Send` because
//! `Txn` is `!Sync`). The compiler enforces this: you cannot send a cursor,
//! nor two cursors sharing one transaction, to another thread.
#![warn(missing_docs)]

use core::ffi::{c_int, c_void, CStr};
use std::ffi::CString;
use std::marker::PhantomData;
use std::path::Path;
use std::sync::Mutex;

use libdb_sys as ffi;

mod bulk;
mod cursor;
mod seq;
mod txn;

pub use bulk::{BulkPairs, MultiKey};
pub use cursor::{Cursor, CursorIter, Direction};
pub use seq::Sequence;
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

/// Jittered backoff between transaction retries. Sleeps a small, randomised
/// time that grows with the attempt count (capped), so two symmetric
/// contenders desynchronise and make progress instead of livelocking. The
/// jitter is a cheap per-thread PRNG seeded from the clock and attempt — no
/// dependency, good enough to break lockstep.
fn backoff(attempt: u32) {
    use std::hash::{Hash, Hasher};
    use std::time::{Duration, SystemTime, UNIX_EPOCH};
    // Grow the window with the attempt (1..=~1ms) so persistent contenders
    // spread out further each round and cannot livelock in lockstep.
    let base = (1u64 << attempt.min(10)).min(1024); // microseconds, capped ~1ms
    let mut h = std::collections::hash_map::DefaultHasher::new();
    std::thread::current().id().hash(&mut h);
    attempt.hash(&mut h);
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0);
    now.hash(&mut h);
    let jitter = h.finish() % base.max(1);
    std::thread::sleep(Duration::from_micros(base + jitter));
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
/// buffer through libdb's own allocator (`__os_ufree`, via the shim) rather
/// than libc `free`, so the free matches whatever allocator libdb used to
/// allocate the buffer. Safe for the empty shape.
///
/// # Allocator invariant
/// This is only guaranteed to free with the right allocator while the crate
/// never configures a **per-env** libdb allocator (`dbenv->set_alloc`). The
/// safe API never does, and that path is not wrapped. See the crate-level
/// allocator note.
pub(crate) unsafe fn take_malloc(d: &ffi::DBT) -> Vec<u8> {
    if d.data.is_null() || d.size == 0 {
        return Vec::new();
    }
    let s = core::slice::from_raw_parts(d.data as *const u8, d.size as usize);
    let v = s.to_vec();
    ffi::libdb_ufree(d.data);
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
/// Opened with `DB_THREAD`, so [`Send`] + [`Sync`]: the handle itself may be
/// shared across threads. The diagnostic slot captured by the error callback
/// is held behind a [`Mutex`], so even the cold `&self` diagnostic paths are
/// race-free. (See the thread-safety section of `README.md` for the
/// txn/cursor single-thread rules, which `Send`/`Sync` on `Env`/`Db` do *not*
/// relax.)
pub struct Env {
    ptr: *mut ffi::DB_ENV,
    open: bool,
    diag: Box<Mutex<Option<String>>>,
}

/// Builder for an [`Env`].
pub struct EnvBuilder {
    ptr: *mut ffi::DB_ENV,
    extra_flags: ffi::u_int32_t,
    diag: Box<Mutex<Option<String>>>,
    capture_diag: bool,
    err: Option<Error>,
}

/// Deadlock-detector victim-selection policy for [`EnvBuilder::lock_detect`].
/// When a deadlock is found, the chosen transaction is the one aborted (so a
/// `lock_get` returns [`Error::Deadlock`] and [`Env::transact`] retries it).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LockDetect {
    /// The library's default policy.
    Default,
    /// Abort the locker holding the most locks.
    MaxLocks,
    /// Abort the locker with the fewest write locks.
    MinWrite,
    /// Abort the oldest locker.
    Oldest,
    /// Abort a randomly chosen locker.
    Random,
    /// Abort the youngest locker.
    Youngest,
}
impl LockDetect {
    fn to_ffi(self) -> ffi::u_int32_t {
        match self {
            LockDetect::Default => ffi::DB_LOCK_DEFAULT,
            LockDetect::MaxLocks => ffi::DB_LOCK_MAXLOCKS,
            LockDetect::MinWrite => ffi::DB_LOCK_MINWRITE,
            LockDetect::Oldest => ffi::DB_LOCK_OLDEST,
            LockDetect::Random => ffi::DB_LOCK_RANDOM,
            LockDetect::Youngest => ffi::DB_LOCK_YOUNGEST,
        }
    }
}

impl Env {
    /// Begin building a new environment.
    pub fn builder() -> EnvBuilder {
        let mut ptr: *mut ffi::DB_ENV = core::ptr::null_mut();
        let rc = unsafe { ffi::db_env_create(&mut ptr, 0) };
        EnvBuilder {
            ptr,
            extra_flags: 0,
            diag: Box::new(Mutex::new(None)),
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
            bt_compare: None,
            dup_compare: None,
            h_hash: None,
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
    /// ```
    /// # let dir = std::env::temp_dir().join(format!("libdb_doc_tx_{}", std::process::id()));
    /// # std::fs::create_dir_all(&dir).unwrap();
    /// # let env = libdb::Env::builder().open(&dir)?;
    /// # let db = env.database().btree().open("d.db")?;
    /// env.transact(10, |txn| {
    ///     db.tput(txn, b"n", b"1")?;
    ///     Ok(())
    /// })?;
    /// assert_eq!(db.get(b"n")?.as_deref(), Some(&b"1"[..]));
    /// # Ok::<(), libdb::Error>(())
    /// ```
    pub fn transact<F, T>(&self, retries: u32, body: F) -> Result<T>
    where
        F: FnMut(&Txn<'_>) -> Result<T>,
    {
        self.transact_opts(retries, TxnOptions::default(), body)
    }

    /// [`Env::transact`] with explicit transaction options (e.g. `nowait` so a
    /// blocked lock fails fast as a deadlock to retry, instead of waiting for
    /// the deadlock detector). Both [`Error::Deadlock`] and
    /// [`Error::LockNotGranted`] are treated as retryable.
    pub fn transact_opts<F, T>(&self, retries: u32, opts: TxnOptions, mut body: F) -> Result<T>
    where
        F: FnMut(&Txn<'_>) -> Result<T>,
    {
        let mut attempt = 0u32;
        loop {
            let txn = self.begin_opts(None, opts)?;
            match body(&txn) {
                Ok(val) => {
                    txn.commit()?;
                    return Ok(val);
                }
                Err(Error::Deadlock) | Err(Error::LockNotGranted) => {
                    txn.abort()?;
                    if attempt >= retries {
                        return Err(Error::Deadlock);
                    }
                    attempt += 1;
                    // Jittered backoff so two symmetric contenders desynchronise
                    // and make progress rather than livelocking (important with
                    // NOWAIT, where a conflict fails instantly). Capped so a
                    // real workload stays responsive.
                    backoff(attempt);
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
        self.diag.lock().unwrap_or_else(|e| e.into_inner()).take()
    }

    pub(crate) fn as_ptr(&self) -> *mut ffi::DB_ENV {
        self.ptr
    }

    fn take_diag(&self) -> Option<String> {
        self.diag.lock().unwrap_or_else(|e| e.into_inner()).take()
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

    /// Enable the deadlock detector with the given victim-selection policy.
    ///
    /// **Essential for concurrent transactions.** libdb does *not* detect
    /// deadlocks on its own: without a detector (or a lock timeout), two
    /// transactions that deadlock wait forever and [`Env::transact`] never
    /// sees [`Error::Deadlock`] to retry on. Set this on any environment used
    /// by more than one concurrent writer. [`LockDetect::Default`] is a sound
    /// choice.
    pub fn lock_detect(mut self, mode: LockDetect) -> Self {
        if self.err.is_none() {
            if let Err(e) = check(unsafe { ffi::libdb_env_set_lk_detect(self.ptr, mode.to_ffi()) })
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
            let p = (&*self.diag) as *const Mutex<Option<String>> as *mut c_void;
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
                let diag = std::mem::replace(&mut self.diag, Box::new(Mutex::new(None)));
                let env = Env {
                    ptr: self.ptr,
                    open: true,
                    diag,
                };
                core::mem::forget(self);
                Ok(env)
            }
            Err(e) => {
                let d = self.diag.lock().unwrap_or_else(|e| e.into_inner()).take();
                let e = if let (Error::Db(c, m), Some(d)) = (&e, d) {
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
        let cell = &*(p as *const Mutex<Option<String>>);
        // Never block the C library: if a Rust reader holds the lock, drop this
        // diagnostic rather than wait inside libdb's error path.
        if let Ok(mut g) = cell.try_lock() {
            let s = CStr::from_ptr(msg).to_string_lossy().into_owned();
            *g = Some(s);
        }
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

/// Cursor positioning opcodes, for [`Cursor::pget`].
pub mod opcode {
    use super::ffi;
    /// First record.
    pub const FIRST: u32 = ffi::DB_FIRST;
    /// Last record.
    pub const LAST: u32 = ffi::DB_LAST;
    /// Next record.
    pub const NEXT: u32 = ffi::DB_NEXT;
    /// Previous record.
    pub const PREV: u32 = ffi::DB_PREV;
    /// Next duplicate of the current key.
    pub const NEXT_DUP: u32 = ffi::DB_NEXT_DUP;
    /// Current record.
    pub const CURRENT: u32 = ffi::DB_CURRENT;
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
    // The installed comparator/hash closures, kept alive for the handle's
    // lifetime; their raw pointer is stashed on the DB handle for the thunks.
    _callbacks: Option<Box<DbCallbacks>>,
}

/// A key/value comparator closure.
pub(crate) type CompareClosure =
    Box<dyn Fn(&[u8], &[u8]) -> core::cmp::Ordering + Send + Sync + 'static>;
/// A hash-function closure.
pub(crate) type HashClosure = Box<dyn Fn(&[u8]) -> u32 + Send + Sync + 'static>;
/// A secondary-index extractor closure.
pub(crate) type ExtractClosure =
    Box<dyn Fn(&[u8], &[u8]) -> Option<Vec<u8>> + Send + Sync + 'static>;

/// The host closures stashed behind the DB handle's `app_private` slot so the
/// extern-C comparator/hash/associate thunks can recover them.
///
/// `bt_compare`/`dup_compare`/`h_hash` are fixed at build time. The `associate`
/// extractor is installed later (by [`Db::associate`]) on the *secondary*
/// handle, so it lives behind a [`Mutex`] and merges into whatever comparator
/// closures the secondary already owns rather than replacing them. The box is
/// owned by the `Db` (field `_callbacks`) and dropped with the handle — no
/// leak — and never moves, so the raw pointer stashed in `app_private` stays
/// valid for the handle's life.
struct DbCallbacks {
    bt_compare: Option<CompareClosure>,
    dup_compare: Option<CompareClosure>,
    h_hash: Option<HashClosure>,
    associate: Mutex<Option<ExtractClosure>>,
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
    bt_compare: Option<CompareClosure>,
    dup_compare: Option<CompareClosure>,
    h_hash: Option<HashClosure>,
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
    /// Install a Btree key comparator defining the key order.
    pub fn bt_compare<F>(mut self, f: F) -> Self
    where
        F: Fn(&[u8], &[u8]) -> core::cmp::Ordering + Send + Sync + 'static,
    {
        self.bt_compare = Some(Box::new(f));
        self
    }
    /// Install a duplicate-data comparator (requires `sorted_duplicates`).
    pub fn dup_compare<F>(mut self, f: F) -> Self
    where
        F: Fn(&[u8], &[u8]) -> core::cmp::Ordering + Send + Sync + 'static,
    {
        self.dup_compare = Some(Box::new(f));
        self
    }
    /// Install a Hash hash function.
    pub fn h_hash<F>(mut self, f: F) -> Self
    where
        F: Fn(&[u8]) -> u32 + Send + Sync + 'static,
    {
        self.h_hash = Some(Box::new(f));
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

        // Always allocate and stash a callbacks box so that app_private is a
        // stable pointer to a Db-owned struct: comparators (if any) are set now,
        // and a later associate() merges its extractor into the SAME box rather
        // than allocating/leaking a second one that would clobber comparators.
        let cb = Box::new(DbCallbacks {
            bt_compare: self.bt_compare.take(),
            dup_compare: self.dup_compare.take(),
            h_hash: self.h_hash.take(),
            associate: Mutex::new(None),
        });
        let raw = (&*cb) as *const DbCallbacks as *mut c_void;
        unsafe {
            ffi::libdb_db_set_app_private(dbp, raw);
            if cb.bt_compare.is_some() {
                ffi::libdb_db_set_bt_compare(dbp, Some(bt_compare_thunk));
            }
            if cb.dup_compare.is_some() {
                ffi::libdb_db_set_dup_compare(dbp, Some(dup_compare_thunk));
            }
            if cb.h_hash.is_some() {
                ffi::libdb_db_set_h_hash(dbp, Some(h_hash_thunk));
            }
        }
        let callbacks = Some(cb);

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
                _callbacks: callbacks,
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
    ///
    /// ```
    /// # let dir = std::env::temp_dir().join(format!("libdb_doc_wv_{}", std::process::id()));
    /// # std::fs::create_dir_all(&dir).unwrap();
    /// # let env = libdb::Env::builder().open(&dir)?;
    /// # let db = env.database().btree().open("d.db")?;
    /// db.put(b"k", b"borrowed")?;
    /// // The slice is valid only inside the closure; here we read its length.
    /// let len = db.with_value(b"k", |v| v.len())?;
    /// assert_eq!(len, Some(8));
    /// # Ok::<(), libdb::Error>(())
    /// ```
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

    // --- secondary index ---------------------------------------------------

    /// Associate `secondary` with this (primary) database. The `extractor`
    /// derives a secondary key from each primary (key, value); return `None`
    /// to skip indexing a record (`DB_DONOTINDEX`). After this, a `pget` on the
    /// secondary returns the primary's value.
    ///
    /// ```
    /// # let dir = std::env::temp_dir().join(format!("libdb_doc_sec_{}", std::process::id()));
    /// # std::fs::create_dir_all(&dir).unwrap();
    /// # let env = libdb::Env::builder().open(&dir)?;
    /// let people = env.database().btree().open("people.db")?;
    /// let by_city = env.database().btree().sorted_duplicates().open("by_city.db")?;
    /// // index each "name|city" value by its city field
    /// people.associate(&by_city, |_k, v| {
    ///     let bar = v.iter().position(|&c| c == b'|')?;
    ///     Some(v[bar + 1..].to_vec())
    /// })?;
    /// people.put(b"1", b"ada|london")?;
    /// // look the primary up through the secondary key
    /// let (pk, val) = by_city.pget(b"london")?.unwrap();
    /// assert_eq!(pk, b"1");
    /// assert_eq!(val, b"ada|london");
    /// # Ok::<(), libdb::Error>(())
    /// ```
    pub fn associate<F>(&self, secondary: &Db<'env>, extractor: F) -> Result<()>
    where
        F: Fn(&[u8], &[u8]) -> Option<Vec<u8>> + Send + Sync + 'static,
    {
        // The associate extractor must live on the SECONDARY handle (its first
        // callback arg is the secondary). The secondary's app_private already
        // points at its Db-owned DbCallbacks box (installed at open); merge the
        // extractor into that box's `associate` slot so it (a) drops with the
        // secondary handle — no leak — and (b) preserves any dup_compare/h_hash
        // the secondary was built with, rather than clobbering them.
        let cb = secondary
            ._callbacks
            .as_ref()
            .expect("every Db has an owned callbacks box");
        *cb.associate.lock().unwrap_or_else(|e| e.into_inner()) = Some(Box::new(extractor));
        let rc = unsafe {
            ffi::libdb_db_associate(
                self.ptr,
                core::ptr::null_mut(),
                secondary.ptr,
                Some(associate_thunk),
                0,
            )
        };
        check(rc).map_err(|e| self.env.enrich(e))?;
        Ok(())
    }

    /// Secondary `pget`: look a secondary key up and return the primary's
    /// `(primary_key, value)`. `Ok(None)` if absent.
    pub fn pget(&self, skey: &[u8]) -> Result<Option<(Vec<u8>, Vec<u8>)>> {
        let mut k = dbt_in(skey);
        let mut pk = dbt_out(ffi::DB_DBT_MALLOC);
        let mut d = dbt_out(ffi::DB_DBT_MALLOC);
        let rc = unsafe {
            ffi::libdb_db_pget(self.ptr, core::ptr::null_mut(), &mut k, &mut pk, &mut d, 0)
        };
        if rc == ffi::DB_NOTFOUND {
            return Ok(None);
        }
        check(rc).map_err(|e| self.env.enrich(e))?;
        Ok(Some(unsafe { (take_malloc(&pk), take_malloc(&d)) }))
    }

    // --- sequence ----------------------------------------------------------

    /// A 64-bit sequence stored under `key` in this database.
    ///
    /// ```
    /// # let dir = std::env::temp_dir().join(format!("libdb_doc_seq_{}", std::process::id()));
    /// # std::fs::create_dir_all(&dir).unwrap();
    /// # let env = libdb::Env::builder().open(&dir)?;
    /// # let db = env.database().btree().open("d.db")?;
    /// let ids = db.sequence(b"order_id")?;
    /// assert_eq!(ids.next()?, 1);
    /// assert_eq!(ids.next()?, 2);
    /// # Ok::<(), libdb::Error>(())
    /// ```
    pub fn sequence(&self, key: &[u8]) -> Result<Sequence<'_>> {
        seq::open(self, key)
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

// --- comparator / hash / associate thunks ----------------------------------

fn recover_callbacks(db: *mut ffi::DB) -> Option<&'static DbCallbacks> {
    unsafe {
        let p = ffi::libdb_db_get_app_private(db);
        if p.is_null() {
            None
        } else {
            Some(&*(p as *const DbCallbacks))
        }
    }
}

unsafe fn dbt_slice<'a>(d: *const ffi::DBT) -> &'a [u8] {
    let d = &*d;
    if d.data.is_null() || d.size == 0 {
        &[]
    } else {
        core::slice::from_raw_parts(d.data as *const u8, d.size as usize)
    }
}

extern "C" fn bt_compare_thunk(db: *mut ffi::DB, a: *const ffi::DBT, b: *const ffi::DBT) -> c_int {
    std::panic::catch_unwind(|| {
        let cbs = match recover_callbacks(db) {
            Some(c) => c,
            None => return 0,
        };
        let f = match &cbs.bt_compare {
            Some(f) => f,
            None => return 0,
        };
        let (sa, sb) = unsafe { (dbt_slice(a), dbt_slice(b)) };
        ordering_to_int(f(sa, sb))
    })
    .unwrap_or(0)
}

extern "C" fn dup_compare_thunk(db: *mut ffi::DB, a: *const ffi::DBT, b: *const ffi::DBT) -> c_int {
    std::panic::catch_unwind(|| {
        let cbs = match recover_callbacks(db) {
            Some(c) => c,
            None => return 0,
        };
        let f = match &cbs.dup_compare {
            Some(f) => f,
            None => return 0,
        };
        let (sa, sb) = unsafe { (dbt_slice(a), dbt_slice(b)) };
        ordering_to_int(f(sa, sb))
    })
    .unwrap_or(0)
}

extern "C" fn h_hash_thunk(
    db: *mut ffi::DB,
    data: *const c_void,
    len: ffi::u_int32_t,
) -> ffi::u_int32_t {
    std::panic::catch_unwind(|| {
        let cbs = match recover_callbacks(db) {
            Some(c) => c,
            None => return 0,
        };
        let f = match &cbs.h_hash {
            Some(f) => f,
            None => return 0,
        };
        let s = unsafe {
            if data.is_null() || len == 0 {
                &[][..]
            } else {
                core::slice::from_raw_parts(data as *const u8, len as usize)
            }
        };
        f(s)
    })
    .unwrap_or(0)
}

extern "C" fn associate_thunk(
    secondary: *mut ffi::DB,
    key: *const ffi::DBT,
    data: *const ffi::DBT,
    result: *mut ffi::DBT,
) -> c_int {
    std::panic::catch_unwind(|| {
        let cbs = match recover_callbacks(secondary) {
            Some(c) => c,
            None => return ffi::DB_DONOTINDEX,
        };
        let guard = cbs.associate.lock().unwrap_or_else(|e| e.into_inner());
        let f = match guard.as_ref() {
            Some(f) => f,
            None => return ffi::DB_DONOTINDEX,
        };
        let (sk, sd) = unsafe { (dbt_slice(key), dbt_slice(data)) };
        match f(sk, sd) {
            None => ffi::DB_DONOTINDEX,
            Some(v) => unsafe {
                // Hand libdb a buffer it will free via FREE_IF_NEEDED ->
                // __os_ufree (the APPMALLOC path). Allocate it through libdb's
                // OWN allocator (the shim's libdb_umalloc) so the alloc/free
                // pair matches; raw libc malloc would mismatch if a global
                // allocator hook were installed.
                let out = &mut *result;
                if v.is_empty() {
                    out.data = core::ptr::null_mut();
                    out.size = 0;
                } else {
                    let p = ffi::libdb_umalloc(v.len());
                    if p.is_null() {
                        return -1;
                    }
                    core::ptr::copy_nonoverlapping(v.as_ptr(), p as *mut u8, v.len());
                    out.data = p;
                    out.size = v.len() as u32;
                    out.flags = 0x001; // DB_DBT_APPMALLOC: libdb frees it
                }
                0
            },
        }
    })
    .unwrap_or(ffi::DB_DONOTINDEX)
}

fn ordering_to_int(o: core::cmp::Ordering) -> c_int {
    match o {
        core::cmp::Ordering::Less => -1,
        core::cmp::Ordering::Equal => 0,
        core::cmp::Ordering::Greater => 1,
    }
}
