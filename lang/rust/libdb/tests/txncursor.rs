// SPDX-License-Identifier: Sleepycat
//! Phase-2 integration tests: transactions (commit vs abort visibility, the
//! deadlock-retry helper, nested txns, snapshot/nowait options) and cursors
//! (forward + backward scans equal the inserted set, SET/SET_RANGE, put/del/
//! count/dup).
use libdb::{Direction, Env, TxnOptions};
use std::path::PathBuf;

fn tmp_home(name: &str) -> PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("libdb_rs_p2_{}_{}", name, std::process::id()));
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
fn commit_makes_changes_visible() {
    let (env, home) = open("commit");
    let db = env.database().btree().open("d.db").unwrap();

    let txn = env.begin().unwrap();
    db.tput(&txn, b"k", b"v").unwrap();
    // visible inside its own txn
    assert_eq!(db.tget(&txn, b"k").unwrap().as_deref(), Some(&b"v"[..]));
    txn.commit().unwrap();

    // and after commit, in a fresh read
    assert_eq!(db.get(b"k").unwrap().as_deref(), Some(&b"v"[..]));

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn abort_discards_changes() {
    let (env, home) = open("abort");
    let db = env.database().btree().open("d.db").unwrap();
    db.put(b"k", b"orig").unwrap();

    let txn = env.begin().unwrap();
    db.tput(&txn, b"k", b"changed").unwrap();
    assert_eq!(
        db.tget(&txn, b"k").unwrap().as_deref(),
        Some(&b"changed"[..])
    );
    txn.abort().unwrap();

    // the original survives the abort
    assert_eq!(db.get(b"k").unwrap().as_deref(), Some(&b"orig"[..]));

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn transact_commits_on_ok() {
    let (env, home) = open("transact");
    let db = env.database().btree().open("d.db").unwrap();
    env.transact(10, |txn| {
        db.tput(txn, b"a", b"1")?;
        db.tput(txn, b"b", b"2")?;
        Ok(())
    })
    .unwrap();
    assert_eq!(db.get(b"a").unwrap().as_deref(), Some(&b"1"[..]));
    assert_eq!(db.get(b"b").unwrap().as_deref(), Some(&b"2"[..]));

    // and rolls back when the body errors
    let r: Result<(), libdb::Error> = env.transact(10, |txn| {
        db.tput(txn, b"c", b"3")?;
        Err(libdb::Error::Usage("deliberate"))
    });
    assert!(r.is_err());
    assert_eq!(db.get(b"c").unwrap(), None);

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn deadlock_retry_via_transact() {
    // Two threads lock the two keys in opposite order under contention. Using
    // DB_TXN_NOWAIT, a lock that would block fails *immediately* as a deadlock
    // rather than waiting -- so a genuine cycle is resolved at once, transact()
    // retries, and the test is GUARANTEED to terminate (no dependence on the
    // deadlock-detector's timing). The env also sets a detector as the
    // belt-and-suspenders path for the non-NOWAIT case.
    //
    // The env and the single shared db handle are opened once and borrowed
    // into scoped threads (the db is free-threaded under DB_THREAD); opening a
    // db per call would serialise on the open path, so we don't.
    let home = tmp_home("deadlock");
    let env = Env::builder()
        .cachesize(0, 16 << 20)
        .lock_detect(libdb::LockDetect::Default)
        .open(&home)
        .unwrap();
    let db = env.database().btree().open("d.db").unwrap();
    db.put(b"x", b"0").unwrap();
    db.put(b"y", b"0").unwrap();

    let retried = std::sync::atomic::AtomicUsize::new(0);
    let nowait = TxnOptions {
        nowait: true,
        ..Default::default()
    };
    const ITERS: usize = 60;
    std::thread::scope(|s| {
        for i in 0..2 {
            let env = &env;
            let db = &db;
            let retried = &retried;
            s.spawn(move || {
                // thread 0 locks x then y; thread 1 locks y then x -> cyclic.
                let (first, second): (&[u8], &[u8]) =
                    if i == 0 { (b"x", b"y") } else { (b"y", b"x") };
                for _ in 0..ITERS {
                    let mut tries = 0u32;
                    env.transact_opts(1000, nowait, |txn| {
                        tries += 1;
                        db.tput(txn, first, b"1")?;
                        std::thread::yield_now();
                        db.tput(txn, second, b"1")?;
                        Ok(())
                    })
                    .expect("transact should retry past every deadlock/lock conflict");
                    if tries > 1 {
                        retried.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    }
                }
            });
        }
    });

    assert_eq!(db.get(b"x").unwrap().as_deref(), Some(&b"1"[..]));
    assert_eq!(db.get(b"y").unwrap().as_deref(), Some(&b"1"[..]));
    // Over 400 contended transactions at least one must have hit a lock
    // conflict and retried, proving transact() recovered (not merely
    // serialised). This is the teeth: a binding that swallowed the retry would
    // either hang or lose an update.
    assert!(
        retried.load(std::sync::atomic::Ordering::SeqCst) >= 1,
        "at least one transaction must have hit a conflict and retried"
    );

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn cursor_scan_equals_inserted_set_both_directions() {
    let (env, home) = open("scan");
    let db = env.database().btree().open("d.db").unwrap();
    let keys: Vec<&[u8]> = vec![b"alpha", b"bravo", b"charlie", b"delta", b"echo"];
    for (i, k) in keys.iter().enumerate() {
        db.put(k, format!("v{i}").as_bytes()).unwrap();
    }

    // forward == sorted insertion set
    let fwd: Vec<Vec<u8>> = db
        .cursor()
        .unwrap()
        .iter(Direction::Forward)
        .map(|r| r.unwrap().0)
        .collect();
    let mut expect: Vec<Vec<u8>> = keys.iter().map(|k| k.to_vec()).collect();
    expect.sort();
    assert_eq!(fwd, expect);

    // backward == reverse
    let bwd: Vec<Vec<u8>> = db
        .cursor()
        .unwrap()
        .iter(Direction::Backward)
        .map(|r| r.unwrap().0)
        .collect();
    let mut rexpect = expect.clone();
    rexpect.reverse();
    assert_eq!(bwd, rexpect);

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn cursor_set_and_set_range() {
    let (env, home) = open("set");
    let db = env.database().btree().open("d.db").unwrap();
    for k in [b"b".as_slice(), b"d", b"f"] {
        db.put(k, b"x").unwrap();
    }
    let c = db.cursor().unwrap();
    // exact
    assert_eq!(c.set(b"d").unwrap().map(|p| p.0), Some(b"d".to_vec()));
    assert_eq!(c.set(b"c").unwrap(), None); // no exact match
                                            // range: smallest key >= "c" is "d"
    let c2 = db.cursor().unwrap();
    assert_eq!(
        c2.set_range(b"c").unwrap().map(|p| p.0),
        Some(b"d".to_vec())
    );
    assert_eq!(c2.set_range(b"z").unwrap(), None);

    drop(c);
    drop(c2);
    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn cursor_put_del_count_dup() {
    let (env, home) = open("dup");
    let db = env.database().sorted_duplicates().open("d.db").unwrap();
    // Cursor writes in a transactional env need a transaction.
    env.transact(5, |txn| {
        let c = db.cursor_txn(txn)?;
        c.put(b"k", b"a")?;
        c.put_dup(b"k", b"b")?;
        c.put_dup(b"k", b"c")?;
        Ok(())
    })
    .unwrap();

    env.transact(5, |txn| {
        let c = db.cursor_txn(txn)?;
        // position on k and count its duplicates
        c.set(b"k")?;
        assert_eq!(c.count()?, 3);

        // walk the duplicates
        let mut dups = Vec::new();
        let mut cur = c.set(b"k")?;
        while let Some((_, v)) = cur {
            dups.push(v);
            cur = c.next_dup()?;
        }
        assert_eq!(dups, vec![b"a".to_vec(), b"b".to_vec(), b"c".to_vec()]);

        // delete the current (last positioned) record
        c.set(b"k")?;
        c.del()?;
        Ok(())
    })
    .unwrap();

    env.transact(5, |txn| {
        let c = db.cursor_txn(txn)?;
        c.set(b"k")?;
        assert_eq!(c.count()?, 2);
        Ok(())
    })
    .unwrap();

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn cursor_in_txn() {
    let (env, home) = open("curtxn");
    let db = env.database().btree().open("d.db").unwrap();
    env.transact(5, |txn| {
        db.tput(txn, b"1", b"one")?;
        db.tput(txn, b"2", b"two")?;
        let c = db.cursor_txn(txn)?;
        let mut n = 0;
        let mut cur = c.first()?;
        while cur.is_some() {
            n += 1;
            cur = c.next()?;
        }
        assert_eq!(n, 2);
        // cursor dropped here, before the txn commits (lifetime enforces it)
        Ok(())
    })
    .unwrap();

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}

#[test]
fn snapshot_and_nowait_options_begin() {
    let home = tmp_home("opts");
    let env = Env::builder()
        .cachesize(0, 16 << 20)
        .extra_open_flags(libdb::flag::MULTIVERSION)
        .open(&home)
        .unwrap();
    let _db = env.database().btree().open("d.db").unwrap();
    // begin a snapshot txn and a nowait txn (exercise the flags path)
    let t1 = env
        .begin_opts(
            None,
            TxnOptions {
                snapshot: true,
                ..Default::default()
            },
        )
        .unwrap();
    t1.commit().unwrap();
    let t2 = env
        .begin_opts(
            None,
            TxnOptions {
                nowait: true,
                ..Default::default()
            },
        )
        .unwrap();
    t2.abort().unwrap();

    drop(_db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);
}
