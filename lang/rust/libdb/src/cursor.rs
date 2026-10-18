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
pub struct Cursor<'txn, 'db> {
    ptr: *mut ffi::DBC,
    env_err: *const crate::Env,
    _not_sync: NotSync,
    _txn: PhantomData<&'txn ()>,
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
        let mut keybuf: Vec<u8> = Vec::new();
        let mut k = if seed_key {
            keybuf = Vec::with_capacity(key.len().max(256));
            keybuf.extend_from_slice(key);
            keybuf.resize(keybuf.capacity(), 0);
            ffi::DBT {
                data: keybuf.as_mut_ptr() as *mut core::ffi::c_void,
                size: key.len() as u32,
                ulen: keybuf.len() as u32,
                flags: ffi::DB_DBT_USERMEM,
                ..Default::default()
            }
        } else {
            dbt_out(ffi::DB_DBT_MALLOC)
        };
        let mut d = dbt_out(ffi::DB_DBT_MALLOC);
        let rc = unsafe { ffi::libdb_dbc_get(self.ptr, &mut k, &mut d, op) };
        if rc == ffi::DB_NOTFOUND {
            return Ok(None);
        }
        check(rc).map_err(|e| self.enrich(e))?;
        let key_out = if seed_key {
            // libdb wrote the matched key length into k.size, bytes into keybuf.
            keybuf.truncate(k.size as usize);
            keybuf
        } else {
            unsafe { take_malloc(&k) }
        };
        let val = unsafe { take_malloc(&d) };
        Ok(Some((key_out, val)))
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
    pub fn iter(self, dir: Direction) -> CursorIter<'txn, 'db> {
        CursorIter {
            cursor: self,
            dir,
            started: false,
            done: false,
        }
    }
}

impl<'txn, 'db> Drop for Cursor<'txn, 'db> {
    fn drop(&mut self) {
        unsafe { ffi::libdb_dbc_close(self.ptr) };
    }
}
unsafe impl<'txn, 'db> Send for Cursor<'txn, 'db> {}

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
