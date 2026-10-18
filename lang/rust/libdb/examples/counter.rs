// SPDX-License-Identifier: Sleepycat
//! A transactional counter driven by the `env.transact` retry helper.
//!
//! Every increment is a read-modify-write inside `env.transact(..)`, which
//! commits on success and, on a deadlock, aborts and retries. This example is
//! single-threaded so it is deterministic and always terminates — it shows the
//! *API*. The genuine multi-threaded deadlock-and-retry behaviour (two threads
//! locking two keys in opposite order, the detector forcing a victim, and
//! transact() recovering) is exercised with teeth in the integration test
//! `tests/txncursor.rs::deadlock_retry_via_transact`.
//!
//! Run: `cargo run --example counter`
use libdb::{Env, LockDetect};

const INCREMENTS: u64 = 1000;
const KEY: &[u8] = b"count";

fn main() -> std::process::ExitCode {
    let home = std::env::temp_dir().join(format!("libdb_counter_{}", std::process::id()));
    std::fs::create_dir_all(&home).ok();

    // The deadlock detector lets transact() recover from a deadlock by
    // retrying; essential for any concurrent writer (harmless here).
    let env = Env::builder()
        .cachesize(0, 32 << 20)
        .lock_detect(LockDetect::Default)
        .open(&home)
        .unwrap();
    let db = env.database().btree().open("counter.db").unwrap();
    db.put(KEY, b"0").unwrap();

    // Each increment is an atomic read-modify-write committed by transact().
    for _ in 0..INCREMENTS {
        env.transact(100, |txn| {
            let cur: u64 = match db.tget(txn, KEY)? {
                Some(v) => String::from_utf8_lossy(&v).parse().unwrap_or(0),
                None => 0,
            };
            db.tput(txn, KEY, (cur + 1).to_string().as_bytes())?;
            Ok(())
        })
        .expect("transact should commit (and retry past any deadlock)");
    }

    let final_count: u64 = String::from_utf8_lossy(&db.get(KEY).unwrap().unwrap())
        .parse()
        .unwrap();
    println!("final count = {final_count} (expected {INCREMENTS})");

    drop(db);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);

    if final_count == INCREMENTS {
        println!("OK: {INCREMENTS} transactional increments, none lost");
        std::process::ExitCode::SUCCESS
    } else {
        eprintln!("FAIL: expected {INCREMENTS}, got {final_count}");
        std::process::ExitCode::FAILURE
    }
}
