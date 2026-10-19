// SPDX-License-Identifier: Sleepycat
//! Cursors. A [`Cursor`] borrows both its [`Db`] and (optionally) its [`Txn`],
//! so the compiler refuses to let it outlive either — the C rule that a cursor
//! must be closed before its transaction resolves, enforced for free. Cursors
//! are `!Sync` (the C library says a cursor is not free-threaded).

use core::marker::PhantomData;

use libdb_sys as ffi;

use crate::{check, dbt_in, dbt_out, take_malloc, Db, Error, NotSync, Put, Result, Txn};

/// Iteration direction for [`Cursor::iter`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    /// First → last (`DB_NEXT`).
    Forward,
    /// Last → first (`DB_PREV`).
    Backward,
}

/// A database cursor.
///
/// Lifetimes: `'txn` is the transaction it operates in (or the db's lifetime
/// when not in a txn), `'db` is the database. A `Cursor` cannot outlive either.
///
/// Thread affinity: a cursor is **not** `Send`. The C library requires that a
/// cursor — and every cursor sharing one transaction — be used by a single
/// thread. We get this structurally: the `_txn` marker borrows the `Txn`
/// (`&'txn Txn`), and because `Txn` is `!Sync`, `&Txn` is `!Send`, so the
/// cursor is `!Send` with no `unsafe impl`. A non-txn cursor borrows the `Db`
/// (via the same `'db`-tied marker) and is likewise `!Send`.
pub struct Cursor<'txn, 'db> {
    ptr: *mut ffi::DBC,
    env_err: *const crate::Env,
    _not_sync: NotSync,
    // Borrow the real `Txn`, not `()`: `Txn: !Sync` makes `&Txn: !Send`, which
    // makes the cursor `!Send` — exactly the C single-thread-affinity rule.
    _txn: PhantomData<&'txn Txn<'db>>,
    _db: PhantomData<&'db ()>,
}

pub(crate) fn open<'txn, 'db>(
    db: &'db Db<'_>,
    txn: Option<&'txn Txn<'_>>,
) -> Result<Cursor<'txn, 'db>> {
    let mut ptr: *mut ffi::DBC = core::ptr::null_mut();
    let txnptr = txn.map_or(core::ptr::null_mut(), |t| t.as_ptr());
    let rc = unsafe { ffi::libdb_db_cursor(db.as_ptr(), txnptr, &mut ptr, 0) };
    check(rc).map_err(|e| db.env().enrich(e))?;
    Ok(Cursor {
        ptr,
        env_err: db.env() as *const crate::Env,
        _not_sync: PhantomData,
        _txn: PhantomData,
        _db: PhantomData,
    })
}

/// The `(key, value)` pair a cursor get yields, copied out.
type Pair = (Vec<u8>, Vec<u8>);

/// A `(secondary_key, primary_key, value)` triple a secondary `pget` yields.
type Triple = (Vec<u8>, Vec<u8>, Vec<u8>);

impl<'txn, 'db> Cursor<'txn, 'db> {
    fn enrich(&self, e: Error) -> Error {
        unsafe { (*self.env_err).enrich(e) }
    }

    /// Positioned get with the given opcode, copying out `(key, value)`.
    /// `Ok(None)` on `DB_NOTFOUND`. For SET/SET_RANGE `key` seeds the lookup;
    /// for positional ops it is ignored.
    fn get_op(&self, key: &[u8], op: ffi::u_int32_t, seed_key: bool) -> Result<Option<Pair>> {
        // For positional ops the key is pure output under MALLOC. For a seeded
        // SET/SET_RANGE the key DBT is BOTH input (the seed) and output (the
        // matched key); a DB_THREAD handle refuses to return its internal
        // pointer, and MALLOC on a DBT whose data already points at borrowed
        // memory is undefined. So seed the key via a USERMEM buffer that holds
        // the seed and is large enough for libdb to overwrite with the match.
        //
        // The matched key (SET_RANGE in particular) can exceed the seed, so on
        // DB_BUFFER_SMALL we grow the USERMEM key buffer to the size libdb
        // reports and re-issue the same op (the value DBT stays MALLOC).
        if !seed_key {
            let mut k = dbt_out(ffi::DB_DBT_MALLOC);
            let mut d = dbt_out(ffi::DB_DBT_MALLOC);
            let rc = unsafe { ffi::libdb_dbc_get(self.ptr, &mut k, &mut d, op) };
            if rc == ffi::DB_NOTFOUND {
                return Ok(None);
            }
            check(rc).map_err(|e| self.enrich(e))?;
            return Ok(Some(unsafe { (take_malloc(&k), take_malloc(&d)) }));
        }

        let mut cap = key.len().max(256);
        loop {
            let mut keybuf: Vec<u8> = Vec::with_capacity(cap);
            keybuf.extend_from_slice(key);
            keybuf.resize(cap, 0);
            let mut k = ffi::DBT {
                data: keybuf.as_mut_ptr() as *mut core::ffi::c_void,
                size: key.len() as u32,
                ulen: keybuf.len() as u32,
                flags: ffi::DB_DBT_USERMEM,
                ..Default::default()
            };
            let mut d = dbt_out(ffi::DB_DBT_MALLOC);
            let rc = unsafe { ffi::libdb_dbc_get(self.ptr, &mut k, &mut d, op) };
            if rc == ffi::DB_NOTFOUND {
                return Ok(None);
            }
            if rc == ffi::DB_BUFFER_SMALL && (k.size as usize) > cap {
                // The matched key is larger than the seed buffer: grow to the
                // size libdb reported and retry. (No value was copied out on a
                // BUFFER_SMALL return, so there is nothing to free.)
                cap = k.size as usize;
                continue;
            }
            check(rc).map_err(|e| self.enrich(e))?;
            // libdb wrote the matched key length into k.size, bytes into keybuf.
            keybuf.truncate(k.size as usize);
            let val = unsafe { take_malloc(&d) };
            return Ok(Some((keybuf, val)));
        }
    }

    /// Position on the first record and return it.
    pub fn first(&self) -> Result<Option<Pair>> {
        self.get_op(&[], ffi::DB_FIRST, false)
    }
    /// Position on the last record and return it.
    pub fn last(&self) -> Result<Option<Pair>> {
        self.get_op(&[], ffi::DB_LAST, false)
    }
    /// Advance to the next record.
    pub fn next(&self) -> Result<Option<Pair>> {
        self.get_op(&[], ffi::DB_NEXT, false)
    }
    /// Step back to the previous record.
    pub fn prev(&self) -> Result<Option<Pair>> {
        self.get_op(&[], ffi::DB_PREV, false)
    }
    /// Advance to the next duplicate of the current key.
    pub fn next_dup(&self) -> Result<Option<Pair>> {
        self.get_op(&[], ffi::DB_NEXT_DUP, false)
    }
    /// Advance to the first record of the next distinct key.
    pub fn next_nodup(&self) -> Result<Option<Pair>> {
        self.get_op(&[], ffi::DB_NEXT_NODUP, false)
    }
    /// Return the record at the current position.
    pub fn current(&self) -> Result<Option<Pair>> {
        self.get_op(&[], ffi::DB_CURRENT, false)
    }
    /// Position exactly on `key`.
    pub fn set(&self, key: &[u8]) -> Result<Option<Pair>> {
        self.get_op(key, ffi::DB_SET, true)
    }
    /// Position on the smallest key ≥ `key`.
    pub fn set_range(&self, key: &[u8]) -> Result<Option<Pair>> {
        self.get_op(key, ffi::DB_SET_RANGE, true)
    }

    /// Store `(key, data)` through the cursor (`DB_KEYLAST`).
    pub fn put(&self, key: &[u8], data: &[u8]) -> Result<()> {
        let mut k = dbt_in(key);
        let mut d = dbt_in(data);
        check(unsafe { ffi::libdb_dbc_put(self.ptr, &mut k, &mut d, ffi::DB_KEYLAST) })
            .map_err(|e| self.enrich(e))
    }

    /// Add a duplicate data item for `key` (`DB_NODUPDATA`); reports if the
    /// exact pair already exists.
    pub fn put_dup(&self, key: &[u8], data: &[u8]) -> Result<Put> {
        let mut k = dbt_in(key);
        let mut d = dbt_in(data);
        let rc = unsafe { ffi::libdb_dbc_put(self.ptr, &mut k, &mut d, ffi::DB_NODUPDATA) };
        if rc == ffi::DB_KEYEXIST {
            return Ok(Put::Exists);
        }
        check(rc).map_err(|e| self.enrich(e))?;
        Ok(Put::Stored)
    }

    /// Delete the record at the current cursor position.
    pub fn del(&self) -> Result<()> {
        check(unsafe { ffi::libdb_dbc_del(self.ptr, 0) }).map_err(|e| self.enrich(e))
    }

    /// Count the duplicate data items for the current key.
    pub fn count(&self) -> Result<u32> {
        let mut n: ffi::db_recno_t = 0;
        check(unsafe { ffi::libdb_dbc_count(self.ptr, &mut n, 0) }).map_err(|e| self.enrich(e))?;
        Ok(n)
    }

    /// Duplicate this cursor (uninitialised position; re-position after).
    pub fn dup(&self) -> Result<Cursor<'txn, 'db>> {
        let mut ptr: *mut ffi::DBC = core::ptr::null_mut();
        check(unsafe { ffi::libdb_dbc_dup(self.ptr, &mut ptr, 0) }).map_err(|e| self.enrich(e))?;
        Ok(Cursor {
            ptr,
            env_err: self.env_err,
            _not_sync: PhantomData,
            _txn: PhantomData,
            _db: PhantomData,
        })
    }

    /// Iterate forward or backward over every record as a native
    /// [`Iterator`]. Each item is `Result<(key, value)>`; iteration ends at
    /// `DB_NOTFOUND`.
    ///
    /// ```
    /// # let dir = std::env::temp_dir().join(format!("libdb_doc_it_{}", std::process::id()));
    /// # std::fs::create_dir_all(&dir).unwrap();
    /// # let env = libdb::Env::builder().open(&dir)?;
    /// # let db = env.database().btree().open("d.db")?;
    /// db.put(b"a", b"1")?;
    /// db.put(b"b", b"2")?;
    /// let keys: Vec<Vec<u8>> = db
    ///     .cursor()?
    ///     .iter(libdb::Direction::Forward)
    ///     .map(|r| r.unwrap().0)
    ///     .collect();
    /// assert_eq!(keys, vec![b"a".to_vec(), b"b".to_vec()]);
    /// # Ok::<(), libdb::Error>(())
    /// ```
    pub fn iter(self, dir: Direction) -> CursorIter<'txn, 'db> {
        CursorIter {
            cursor: self,
            dir,
            started: false,
            done: false,
        }
    }

    /// Secondary cursor `pget`: position by secondary opcode and return
    /// `(secondary_key, primary_key, value)`. Only valid on a cursor over a
    /// secondary database.
    pub fn pget(&self, op: ffi::u_int32_t) -> Result<Option<Triple>> {
        let mut sk = dbt_out(ffi::DB_DBT_MALLOC);
        let mut pk = dbt_out(ffi::DB_DBT_MALLOC);
        let mut d = dbt_out(ffi::DB_DBT_MALLOC);
        let rc = unsafe { ffi::libdb_dbc_pget(self.ptr, &mut sk, &mut pk, &mut d, op) };
        if rc == ffi::DB_NOTFOUND {
            return Ok(None);
        }
        check(rc).map_err(|e| self.enrich(e))?;
        Ok(Some(unsafe {
            (take_malloc(&sk), take_malloc(&pk), take_malloc(&d))
        }))
    }

    /// Bulk forward scan: fetch many records in one `DB_MULTIPLE_KEY` call and
    /// walk the returned buffer with pure pointer arithmetic — zero per-record
    /// allocation. The returned [`crate::BulkPairs`] borrows `buf`, so each
    /// `(key, value)` is a slice into it.
    ///
    /// ```
    /// # let dir = std::env::temp_dir().join(format!("libdb_doc_bulk_{}", std::process::id()));
    /// # std::fs::create_dir_all(&dir).unwrap();
    /// # let env = libdb::Env::builder().open(&dir)?;
    /// # let db = env.database().btree().open("d.db")?;
    /// for i in 0..3 {
    ///     db.put(format!("k{i}").as_bytes(), format!("v{i}").as_bytes())?;
    /// }
    /// let c = db.cursor()?;
    /// let mut buf = Vec::new();
    /// let pairs: Vec<_> = c.bulk(&mut buf)?.map(|(k, v)| (k.to_vec(), v.to_vec())).collect();
    /// assert_eq!(pairs.len(), 3);
    /// assert_eq!(pairs[0], (b"k0".to_vec(), b"v0".to_vec()));
    /// # Ok::<(), libdb::Error>(())
    /// ```
    pub fn bulk<'a>(&self, buf: &'a mut Vec<u8>) -> Result<crate::BulkPairs<'a>> {
        crate::bulk::fetch(self.ptr, buf, |e| self.enrich(e))
    }
}

impl<'txn, 'db> Drop for Cursor<'txn, 'db> {
    fn drop(&mut self) {
        unsafe { ffi::libdb_dbc_close(self.ptr) };
    }
}

/// A forward/backward [`Iterator`] over a cursor's records.
pub struct CursorIter<'txn, 'db> {
    cursor: Cursor<'txn, 'db>,
    dir: Direction,
    started: bool,
    done: bool,
}

impl<'txn, 'db> Iterator for CursorIter<'txn, 'db> {
    type Item = Result<Pair>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let step = if !self.started {
            self.started = true;
            match self.dir {
                Direction::Forward => self.cursor.first(),
                Direction::Backward => self.cursor.last(),
            }
        } else {
            match self.dir {
                Direction::Forward => self.cursor.next(),
                Direction::Backward => self.cursor.prev(),
            }
        };
        match step {
            Ok(Some(pair)) => Some(Ok(pair)),
            Ok(None) => {
                self.done = true;
                None
            }
            Err(e) => {
                self.done = true;
                Some(Err(e))
            }
        }
    }
}
