// SPDX-License-Identifier: Sleepycat
//! A tiny key/value store CLI over libdb.
//!
//! Usage:
//!   cargo run --example kvstore -- <home-dir> put <key> <value>
//!   cargo run --example kvstore -- <home-dir> get <key>
//!   cargo run --example kvstore -- <home-dir> del <key>
//!   cargo run --example kvstore -- <home-dir> list
//!
//! With no args it runs a self-contained demo in a temp directory.
use std::process::ExitCode;

use libdb::{Direction, Env};

fn run(home: &str, args: &[String]) -> libdb::Result<()> {
    std::fs::create_dir_all(home).ok();
    let env = Env::builder().cachesize(0, 16 << 20).open(home)?;
    let db = env.database().btree().open("kv.db")?;

    match args.first().map(String::as_str) {
        Some("put") => {
            let (k, v) = (&args[1], &args[2]);
            db.put(k.as_bytes(), v.as_bytes())?;
            println!("ok");
        }
        Some("get") => match db.get(args[1].as_bytes())? {
            Some(v) => println!("{}", String::from_utf8_lossy(&v)),
            None => println!("(not found)"),
        },
        Some("del") => {
            let removed = db.del(args[1].as_bytes())?;
            println!("{}", if removed { "deleted" } else { "(not found)" });
        }
        Some("list") => {
            for rec in db.cursor()?.iter(Direction::Forward) {
                let (k, v) = rec?;
                println!(
                    "{} = {}",
                    String::from_utf8_lossy(&k),
                    String::from_utf8_lossy(&v)
                );
            }
        }
        other => {
            eprintln!("unknown command: {other:?}");
        }
    }
    Ok(())
}

fn demo() -> libdb::Result<()> {
    let home = std::env::temp_dir().join(format!("libdb_kvstore_{}", std::process::id()));
    let home = home.to_string_lossy().into_owned();
    println!("# demo in {home}");
    run(&home, &["put".into(), "name".into(), "ada".into()])?;
    run(&home, &["put".into(), "lang".into(), "rust".into()])?;
    run(&home, &["get".into(), "name".into()])?;
    run(&home, &["list".into()])?;
    run(&home, &["del".into(), "name".into()])?;
    run(&home, &["list".into()])?;
    let _ = std::fs::remove_dir_all(&home);
    Ok(())
}

fn main() -> ExitCode {
    let argv: Vec<String> = std::env::args().skip(1).collect();
    let result = if argv.is_empty() {
        demo()
    } else {
        run(&argv[0], &argv[1..])
    };
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("error: {e}");
            ExitCode::FAILURE
        }
    }
}
