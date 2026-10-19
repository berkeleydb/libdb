# 5. Secondary indices

A secondary index lets you look records up by something other than their primary
key. You open a second database and `associate` it with the primary, giving an
*extractor* that derives the secondary key from each primary record. libdb keeps
the index in sync automatically as the primary changes.

```rust
# let dir = std::env::temp_dir().join(format!("guide05_{}", std::process::id()));
# std::fs::create_dir_all(&dir).unwrap();
# let env = libdb::Env::builder().open(&dir)?;
let people = env.database().btree().open("people.db")?;
let by_city = env.database().btree().sorted_duplicates().open("by_city.db")?;

// index each "name|city" record by its city field
people.associate(&by_city, |_id, rec| {
    let bar = rec.iter().position(|&c| c == b'|')?;
    Some(rec[bar + 1..].to_vec())
})?;

people.put(b"1", b"ada|london")?;
people.put(b"2", b"grace|london")?;

// look the primary up through the secondary key
let (primary_key, record) = by_city.pget(b"london")?.unwrap();
assert_eq!(primary_key, b"1");
assert_eq!(record, b"ada|london");
# Ok::<(), libdb::Error>(())
```

## Skipping records

Return `None` from the extractor to leave a record out of the index — this is
libdb's `DB_DONOTINDEX`. For example, a record with no city should not appear in
the by-city index:

```rust
# fn extract(rec: &[u8]) -> Option<Vec<u8>> {
let bar = rec.iter().position(|&c| c == b'|')?;
let city = &rec[bar + 1..];
if city.is_empty() { None } else { Some(city.to_vec()) }
# }
```

## Scanning a secondary

A cursor over the secondary database supports `pget(opcode)`, returning
`(secondary_key, primary_key, value)`. Walk it with `opcode::FIRST` then
`opcode::NEXT` to list, say, everyone in a city — see
`examples/address_book.rs` for the complete program.

Next: [Bulk retrieval](06-bulk.md).
