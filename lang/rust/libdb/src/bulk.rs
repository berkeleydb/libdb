// SPDX-License-Identifier: Sleepycat
//! Bulk retrieval. A single `DB_MULTIPLE_KEY` cursor get returns many
//! `(key, value)` records packed into one buffer; [`BulkPairs`] walks that
//! buffer with pure pointer arithmetic — zero per-record allocation. This is
//! where the binding beats a naive one-get-per-record loop.
//!
//! The on-wire layout (from `db.h`'s `DB_MULTIPLE_KEY_NEXT`): starting at
//! `data + ulen - 4`, the buffer holds, growing toward lower addresses,
//! groups of four `u32`s — `keyoff, keylen, dataoff, datalen` — until a
//! sentinel word of `(u32)-1`. The key/value bytes themselves live at the low
//! end of the same buffer at the recorded offsets.

use core::ffi::c_void;

use libdb_sys as ffi;

use crate::{check, Error, Result};

/// Perform one `DB_MULTIPLE_KEY` get on a cursor into `buf` and return an
/// iterator over the records it holds. The buffer must be reasonably sized and
/// `u32`-aligned; a `Vec<u8>`'s allocation is suitably aligned.
pub(crate) fn fetch<'a, EF>(
    dbc: *mut ffi::DBC,
    buf: &'a mut Vec<u8>,
    enrich: EF,
) -> Result<BulkPairs<'a>>
where
    EF: Fn(Error) -> Error,
{
    if buf.len() < 1024 {
        buf.resize(64 * 1024, 0);
    }
    let mut k = ffi::DBT::default();
    let mut d = ffi::DBT {
        data: buf.as_mut_ptr() as *mut c_void,
        ulen: buf.len() as u32,
        flags: ffi::DB_DBT_USERMEM,
        ..Default::default()
    };
    let rc =
        unsafe { ffi::libdb_dbc_get(dbc, &mut k, &mut d, ffi::DB_FIRST | ffi::DB_MULTIPLE_KEY) };
    if rc == ffi::DB_NOTFOUND {
        return Ok(BulkPairs::empty());
    }
    if rc == ffi::DB_BUFFER_SMALL {
        return Err(enrich(Error::Usage("bulk buffer too small; grow it")));
    }
    check(rc).map_err(enrich)?;
    Ok(BulkPairs::new(buf.as_ptr(), buf.len()))
}

/// A zero-copy iterator over the `(key, value)` pairs in a bulk-get buffer.
pub struct BulkPairs<'a> {
    base: *const u8,
    cursor: *const u32, // walks downward through the index words
    done: bool,
    _buf: core::marker::PhantomData<&'a [u8]>,
}

impl<'a> BulkPairs<'a> {
    fn empty() -> Self {
        BulkPairs {
            base: core::ptr::null(),
            cursor: core::ptr::null(),
            done: true,
            _buf: core::marker::PhantomData,
        }
    }
    fn new(base: *const u8, ulen: usize) -> Self {
        // DB_MULTIPLE_INIT: pointer = data + ulen - sizeof(u32).
        let cursor = unsafe { base.add(ulen - core::mem::size_of::<u32>()) as *const u32 };
        BulkPairs {
            base,
            cursor,
            done: false,
            _buf: core::marker::PhantomData,
        }
    }
}

/// A single bulk record: a key slice and a value slice, both borrowing the
/// bulk buffer.
pub type MultiKey<'a> = (&'a [u8], &'a [u8]);

impl<'a> Iterator for BulkPairs<'a> {
    type Item = MultiKey<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        unsafe {
            // DB_MULTIPLE_KEY_NEXT: read keyoff, keylen, dataoff, datalen,
            // decrementing the pointer; sentinel (u32)-1 ends iteration.
            let mut p = self.cursor;
            let first = *p;
            if first == u32::MAX {
                self.done = true;
                return None;
            }
            let keyoff = *p;
            p = p.sub(1);
            let keylen = *p;
            p = p.sub(1);
            let dataoff = *p;
            p = p.sub(1);
            let datalen = *p;
            p = p.sub(1);
            self.cursor = p;

            let key = core::slice::from_raw_parts(self.base.add(keyoff as usize), keylen as usize);
            let val =
                core::slice::from_raw_parts(self.base.add(dataoff as usize), datalen as usize);
            Some((key, val))
        }
    }
}
