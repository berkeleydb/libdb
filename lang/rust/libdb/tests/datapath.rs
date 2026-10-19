// SPDX-License-Identifier: Sleepycat
//! Phase-1 integration tests: the core data path against the real library —
//! the three get forms, USERMEM grow-retry, exists/del, empty values, and the
//! compile-vs-runtime version check.
use libdb::{Env, GetInto, Put};
use std::path::PathBuf;

fn tmp_home(name: &str) -> PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("libdb_rs_p1_{}_{}", name, std::process::id()));
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
fn version_check_passes() {
    libdb::check_version().expect("header/library skew");
    let (maj, min, _patch) = libdb::version();
    assert_eq!((maj, min), (2026, 0));
}

#[test]
fn three_get_forms_agree() {
    let (env, home) = open("getforms");
    let db = env.database().btree().open("d.db").unwrap();
    db.put(b"k", b"hello world").unwrap();

    // copy-out
    assert_eq!(db.get(b"k").unwrap().as_deref(), Some(&b"hello world"[..]));

    // into caller buffer (fits)
    let mut buf = [0u8; 32];
    match db.get_into(b"k", &mut buf).unwrap() {
        GetInto::Fit(n) => assert_eq!(&buf[..n], b"hello world"),
        other => panic!("expected Fit, got {other:?}"),
    }

    // borrowed closure — lifetime cannot escape
    let len = db.with_value(b"k", |v| v.len()).unwrap();
    assert_eq!(len, Some(11));
    let copied = db.with_value(b"k", <[u8]>::to_vec).unwrap();
    assert_eq!(copied.as_deref(), Some(&b"hello world"[..]));

    // absent key in each form
    assert_eq!(db.get(b"absent").unwrap(), None);
    assert!(matches!(
        db.get_into(b"absent", &mut buf).unwrap(),
        GetInto::NotFound
    ));
    assert_eq!(db.with_value(b"absent", |_| ()).unwrap(), None);

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn usermem_grow_retry() {
    let (env, home) = open("usermem");
    let db = env.database().btree().open("d.db").unwrap();
    let big = vec![0xabu8; 500];
    db.put(b"big", &big).unwrap();

    // A buffer too small reports the needed size (TooSmall), not an error.
    let mut small = [0u8; 8];
    match db.get_into(b"big", &mut small).unwrap() {
        GetInto::TooSmall(need) => assert_eq!(need, 500),
        other => panic!("expected TooSmall(500), got {other:?}"),
    }

    // get_into_grow does the retry: starts small, grows to fit.
    let mut buf = Vec::new();
    let n = db.get_into_grow(b"big", &mut buf).unwrap().unwrap();
    assert_eq!(n, 500);
    assert_eq!(&buf[..n], &big[..]);

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn exists_del_and_no_overwrite() {
    let (env, home) = open("existsdel");
    let db = env.database().btree().open("d.db").unwrap();

    assert!(!db.exists(b"x").unwrap());
    assert_eq!(db.put_no_overwrite(b"x", b"1").unwrap(), Put::Stored);
    assert!(db.exists(b"x").unwrap());
    // second no-overwrite reports Exists, does not change the value
    assert_eq!(db.put_no_overwrite(b"x", b"2").unwrap(), Put::Exists);
    assert_eq!(db.get(b"x").unwrap().as_deref(), Some(&b"1"[..]));

    assert!(db.del(b"x").unwrap());
    assert!(!db.del(b"x").unwrap()); // deleting an absent key: Ok(false)
    assert!(!db.exists(b"x").unwrap());

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn empty_and_binary_values() {
    let (env, home) = open("empty");
    let db = env.database().btree().open("d.db").unwrap();
    db.put(b"empty", b"").unwrap();
    assert_eq!(db.get(b"empty").unwrap().as_deref(), Some(&b""[..]));
    let bin = [0u8, 1, 2, 255, 0, 128];
    db.put(b"bin", &bin).unwrap();
    assert_eq!(db.get(b"bin").unwrap().as_deref(), Some(&bin[..]));
    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn hash_and_recno_types_open() {
    let (env, home) = open("types");
    let h = env.database().hash().open("h.db").unwrap();
    h.put(b"a", b"1").unwrap();
    assert_eq!(h.get(b"a").unwrap().as_deref(), Some(&b"1"[..]));
    drop(h);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}
