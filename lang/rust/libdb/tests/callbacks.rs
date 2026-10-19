// SPDX-License-Identifier: Sleepycat
//! Phase-3 integration tests: comparators (bt_compare ordering), a secondary
//! index (associate + pget, with DB_DONOTINDEX skipping a record), 64-bit
//! sequences, and zero-copy bulk get (== N single gets).
use libdb::{Direction, Env};
use std::path::PathBuf;

fn tmp_home(name: &str) -> PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("libdb_rs_p3_{}_{}", name, std::process::id()));
    let _ = std::fs::remove_dir_all(&p);
    std::fs::create_dir_all(&p).unwrap();
    p
}

fn open(name: &str) -> (Env, PathBuf) {
    let home = tmp_home(name);
    let env = Env::builder().cachesize(0, 16 << 20).open(&home).unwrap();
    (env, home)
}

#[test]
fn bt_compare_reverses_order() {
    let (env, home) = open("btcmp");
    // Reverse-lexicographic comparator: b before a.
    let db = env
        .database()
        .btree()
        .bt_compare(|a, b| b.cmp(a))
        .open("d.db")
        .unwrap();
    for k in [b"a".as_slice(), b"b", b"c"] {
        db.put(k, b"x").unwrap();
    }
    let order: Vec<Vec<u8>> = db
        .cursor()
        .unwrap()
        .iter(Direction::Forward)
        .map(|r| r.unwrap().0)
        .collect();
    // forward iteration under the reverse comparator yields c, b, a
    assert_eq!(
        order,
        vec![b"c".to_vec(), b"b".to_vec(), b"a".to_vec()],
        "custom comparator must drive the stored order"
    );

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn secondary_index_lookup() {
    let (env, home) = open("secondary");
    // Primary: id -> "name:age" ; secondary indexes by age field.
    let primary = env.database().btree().open("primary.db").unwrap();
    let secondary = env
        .database()
        .btree()
        .sorted_duplicates()
        .open("secondary.db")
        .unwrap();

    // extractor: skip records whose value starts with '_' (DB_DONOTINDEX)
    primary
        .associate(&secondary, |_pk, data| {
            if data.first() == Some(&b'_') {
                return None; // DB_DONOTINDEX
            }
            // index by the substring after the ':'
            let s = data.iter().position(|&c| c == b':')?;
            Some(data[s + 1..].to_vec())
        })
        .unwrap();

    primary.put(b"1", b"alice:30").unwrap();
    primary.put(b"2", b"bob:25").unwrap();
    primary.put(b"3", b"carol:30").unwrap();
    primary.put(b"4", b"_hidden:30").unwrap(); // extractor returns None

    // pget by the secondary key "30" returns a primary (key, value).
    let got = secondary.pget(b"30").unwrap();
    assert!(got.is_some());
    let (pk, val) = got.unwrap();
    // the first "30" record by primary order is id 1 (alice)
    assert_eq!(pk, b"1".to_vec());
    assert_eq!(val, b"alice:30".to_vec());

    // "25" -> bob
    assert_eq!(
        secondary.pget(b"25").unwrap().map(|(_, v)| v),
        Some(b"bob:25".to_vec())
    );
    // the hidden record was skipped: no secondary entry maps to its key
    // (its age was 30 but it was DONOTINDEX'd, so only alice+carol are under 30)
    // Count the "30" duplicates via a secondary cursor pget scan.
    let c = secondary.cursor().unwrap();
    let mut thirties = 0;
    let mut cur = c.pget(libdb::opcode::FIRST).unwrap();
    while let Some((sk, _pk, _v)) = cur {
        if sk == b"30" {
            thirties += 1;
        }
        cur = c.pget(libdb::opcode::NEXT).unwrap();
    }
    assert_eq!(thirties, 2, "alice + carol, hidden was DONOTINDEX'd");

    drop(c);
    drop(secondary);
    drop(primary);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn sequence_increments() {
    let (env, home) = open("seq");
    let db = env.database().btree().open("d.db").unwrap();
    let seq = db.sequence(b"counter").unwrap();
    // initial value is 1; next() returns and advances.
    assert_eq!(seq.next().unwrap(), 1);
    assert_eq!(seq.next().unwrap(), 2);
    assert_eq!(seq.next().unwrap(), 3);
    // advance by a delta
    assert_eq!(seq.advance(10).unwrap(), 4); // returns current (4), advances to 14
    assert_eq!(seq.next().unwrap(), 14);

    drop(seq);
    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn bulk_get_equals_n_single_gets() {
    let (env, home) = open("bulk");
    let db = env.database().btree().open("d.db").unwrap();
    // insert N records
    let n = 500usize;
    for i in 0..n {
        let k = format!("key{i:04}");
        let v = format!("value-{i}");
        db.put(k.as_bytes(), v.as_bytes()).unwrap();
    }

    // single-get reference set, in sorted key order
    let single: Vec<(Vec<u8>, Vec<u8>)> = db
        .cursor()
        .unwrap()
        .iter(Direction::Forward)
        .map(|r| r.unwrap())
        .collect();
    assert_eq!(single.len(), n);

    // bulk get: one call, zero-copy iterator over the returned buffer
    let c = db.cursor().unwrap();
    let mut buf = Vec::new();
    let bulk: Vec<(Vec<u8>, Vec<u8>)> = c
        .bulk(&mut buf)
        .unwrap()
        .map(|(k, v)| (k.to_vec(), v.to_vec()))
        .collect();

    assert_eq!(bulk.len(), single.len(), "bulk count must equal N");
    assert_eq!(bulk, single, "bulk pairs must equal the single-get set");

    drop(c);
    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn comparator_panic_is_contained() {
    // A panic in a comparator must NOT cross the FFI boundary (catch_unwind in
    // the thunk converts it to a return of 0 rather than aborting the
    // process). We prove containment: the process survives the comparator
    // firing on the second insert, and this test function returns normally.
    // (The btree order is undefined under a degraded comparator; we only
    // assert survival, not ordering.)
    let (env, home) = open("panic");
    let db = env
        .database()
        .btree()
        .bt_compare(|_a, _b| panic!("boom in comparator"))
        .open("d.db")
        .unwrap();
    db.put(b"a", b"1").unwrap();
    // This insert fires the comparator, which panics; catch_unwind swallows it
    // and returns 0. The call may error (degraded compare), but must not abort.
    let _ = db.put(b"b", b"2");
    // If we reach here, the panic did not cross FFI / abort the process.

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn dup_compare_drives_duplicate_order() {
    // Regression + coverage for the dup_compare thunk, which no test fired.
    // A reverse-numeric duplicate comparator must drive the stored order of
    // duplicate data items under one key: inserting "1","2","3" (as bytes)
    // must iterate back as "3","2","1".
    let (env, home) = open("dupcmp");
    let db = env
        .database()
        .btree()
        .sorted_duplicates()
        // compare the duplicate DATA items in REVERSE byte order
        .dup_compare(|a, b| b.cmp(a))
        .open("d.db")
        .unwrap();

    for d in [b"1".as_slice(), b"2", b"3"] {
        db.put(b"k", d).unwrap();
    }

    // walk the duplicates of "k" in stored order
    let c = db.cursor().unwrap();
    let mut dups = Vec::new();
    let mut cur = c.set(b"k").unwrap();
    while let Some((_, v)) = cur {
        dups.push(v);
        cur = c.next_dup().unwrap();
    }
    assert_eq!(
        dups,
        vec![b"3".to_vec(), b"2".to_vec(), b"1".to_vec()],
        "reverse dup_compare must drive the duplicate order"
    );

    drop(c);
    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn h_hash_is_used_by_the_hash_db() {
    // Regression + coverage for the h_hash thunk, which no test fired. We
    // install a custom hash function that counts how many times it is called;
    // storing and fetching records must invoke it, proving the thunk routes to
    // the host closure (a hash DB that ignored our function would never bump
    // the counter). The exact hash value is irrelevant to correctness — any
    // function that distributes keys works — so we only assert it ran and the
    // data round-trips.
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    let (env, home) = open("hhash");
    let calls = Arc::new(AtomicUsize::new(0));
    let calls_cb = Arc::clone(&calls);
    let db = env
        .database()
        .hash()
        .h_hash(move |data| {
            calls_cb.fetch_add(1, Ordering::SeqCst);
            // a trivial additive hash; sum the bytes
            data.iter().fold(0u32, |a, &b| a.wrapping_add(b as u32))
        })
        .open("h.db")
        .unwrap();

    for i in 0..50u32 {
        let k = format!("key{i}");
        db.put(k.as_bytes(), b"v").unwrap();
    }
    // round-trip a few
    assert_eq!(db.get(b"key0").unwrap().as_deref(), Some(&b"v"[..]));
    assert_eq!(db.get(b"key49").unwrap().as_deref(), Some(&b"v"[..]));

    assert!(
        calls.load(Ordering::SeqCst) > 0,
        "the custom h_hash must be invoked by the hash DB"
    );

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn secondary_with_dup_compare_keeps_both_callbacks() {
    // Regression for S3/S4: associate() used to overwrite+leak the secondary's
    // callbacks box, clobbering a user dup_compare. Here the secondary has a
    // reverse dup_compare AND is associated; both must survive — the extractor
    // indexes records, and the (duplicate) secondary keys must be ordered by
    // the reverse comparator.
    let (env, home) = open("sec_dupcmp");
    let primary = env.database().btree().open("primary.db").unwrap();
    let secondary = env
        .database()
        .btree()
        .sorted_duplicates()
        .dup_compare(|a, b| b.cmp(a)) // reverse: larger primary key first
        .open("secondary.db")
        .unwrap();

    // index everything under a single constant secondary key so the PRIMARY
    // keys become the duplicate data items whose order the dup_compare drives.
    primary
        .associate(&secondary, |_pk, _v| Some(b"group".to_vec()))
        .unwrap();

    primary.put(b"1", b"a").unwrap();
    primary.put(b"2", b"b").unwrap();
    primary.put(b"3", b"c").unwrap();

    // pget by the single secondary key; a cursor scan of its duplicates yields
    // the primary keys. If associate had clobbered dup_compare, the order would
    // be the default (ascending 1,2,3); with it preserved it is reverse 3,2,1.
    let c = secondary.cursor().unwrap();
    let mut pks = Vec::new();
    let mut cur = c.pget(libdb::opcode::FIRST).unwrap();
    while let Some((_sk, pk, _v)) = cur {
        pks.push(pk);
        cur = c.pget(libdb::opcode::NEXT).unwrap();
    }
    assert_eq!(
        pks,
        vec![b"3".to_vec(), b"2".to_vec(), b"1".to_vec()],
        "the secondary's dup_compare must survive associate() (reverse order)"
    );

    drop(c);
    drop(secondary);
    drop(primary);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}
