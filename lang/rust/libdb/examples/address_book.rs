// SPDX-License-Identifier: Sleepycat
//! A secondary-index address book.
//!
//! The primary database maps a person id to a `name|city` record. A secondary
//! database indexes people by city, so you can look up everyone in a city with
//! a single `pget`-style cursor scan — the index is maintained automatically
//! by the `associate` extractor. People with a blank city are skipped from the
//! index (the extractor returns `None`, i.e. `DB_DONOTINDEX`).
//!
//! Run: `cargo run --example address_book`
use libdb::{opcode, Env};

fn main() -> std::process::ExitCode {
    let home = std::env::temp_dir().join(format!("libdb_addrbook_{}", std::process::id()));
    std::fs::create_dir_all(&home).ok();
    let env = Env::builder().cachesize(0, 16 << 20).open(&home).unwrap();

    let people = env.database().btree().open("people.db").unwrap();
    let by_city = env
        .database()
        .btree()
        .sorted_duplicates()
        .open("by_city.db")
        .unwrap();

    // Index each record by its city (the field after '|'); skip blank cities.
    people
        .associate(&by_city, |_id, rec| {
            let bar = rec.iter().position(|&c| c == b'|')?;
            let city = &rec[bar + 1..];
            if city.is_empty() {
                None // DB_DONOTINDEX
            } else {
                Some(city.to_vec())
            }
        })
        .unwrap();

    for (id, rec) in [
        ("1", "ada|london"),
        ("2", "bjarne|aarhus"),
        ("3", "grace|london"),
        ("4", "anon|"), // blank city -> not indexed
    ] {
        people.put(id.as_bytes(), rec.as_bytes()).unwrap();
    }

    // Direct secondary lookup: who is the first person in London?
    let (id, rec) = by_city.pget(b"london").unwrap().unwrap();
    println!(
        "first in london: id={} rec={}",
        String::from_utf8_lossy(&id),
        String::from_utf8_lossy(&rec)
    );

    // Scan everyone in London via a secondary cursor.
    println!("everyone in london:");
    let c = by_city.cursor().unwrap();
    let mut londoners = 0;
    let mut cur = c.pget(opcode::FIRST).unwrap();
    while let Some((city, id, rec)) = cur {
        if city == b"london" {
            londoners += 1;
            println!(
                "  id={} rec={}",
                String::from_utf8_lossy(&id),
                String::from_utf8_lossy(&rec)
            );
        }
        cur = c.pget(opcode::NEXT).unwrap();
    }

    drop(c);
    drop(by_city);
    drop(people);
    drop(env);
    let _ = std::fs::remove_dir_all(&home);

    // ada + grace are in London; anon has a blank city and was not indexed.
    if londoners == 2 {
        println!("OK: 2 londoners, blank-city record correctly skipped");
        std::process::ExitCode::SUCCESS
    } else {
        eprintln!("FAIL: expected 2 londoners, got {londoners}");
        std::process::ExitCode::FAILURE
    }
}
