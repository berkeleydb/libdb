// SPDX-License-Identifier: Sleepycat
//! Phase-0 integration test: a real put/get round trip against the installed
//! library, plus absent-key and empty-value edge cases.
use std::path::PathBuf;

fn tmp_home(name: &str) -> PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("libdb_rs_{}_{}", name, std::process::id()));
    let _ = std::fs::remove_dir_all(&p);
    std::fs::create_dir_all(&p).unwrap();
    p
}

#[test]
fn version_is_sane() {
    let (a, b, _c) = libdb::version();
    assert_eq!(a, 2026, "major should be the frozen format level 2026");
    assert_eq!(b, 0);
}

#[test]
fn round_trip() {
    let home = tmp_home("rt");
    let env = libdb::Env::create().unwrap()
        .cachesize(0, 16 << 20).unwrap()
        .open(&home).unwrap();
    let db = env.database().open_btree("data.db").unwrap();

    db.put(b"alpha", b"one").unwrap();
    db.put(b"beta", b"two").unwrap();

    assert_eq!(db.get(b"alpha").unwrap().as_deref(), Some(&b"one"[..]));
    assert_eq!(db.get(b"beta").unwrap().as_deref(), Some(&b"two"[..]));
    // absent key: Ok(None), NOT an error.
    assert_eq!(db.get(b"gamma").unwrap(), None);

    // overwrite
    db.put(b"alpha", b"ONE!").unwrap();
    assert_eq!(db.get(b"alpha").unwrap().as_deref(), Some(&b"ONE!"[..]));

    // empty value round-trips (the data==NULL/size==0 guard)
    db.put(b"empty", b"").unwrap();
    assert_eq!(db.get(b"empty").unwrap().as_deref(), Some(&b""[..]));

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}
