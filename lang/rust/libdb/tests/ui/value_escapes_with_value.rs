// SPDX-License-Identifier: Sleepycat
// MUST NOT COMPILE: the borrowed value in `with_value` cannot escape the
// closure. The slice is valid only for the call; returning it would alias
// freed/overwritten memory, so the closure's return type may not borrow it.
fn main() {
    let env = libdb::Env::builder().open("h").unwrap();
    let db = env.database().btree().open("d.db").unwrap();
    db.put(b"k", b"v").unwrap();
    // Try to smuggle the borrowed slice out of the closure — error.
    let escaped: Option<&[u8]> = db.with_value(b"k", |v| v).unwrap();
    let _ = escaped;
}
