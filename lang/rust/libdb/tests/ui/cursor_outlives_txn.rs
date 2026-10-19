// SPDX-License-Identifier: Sleepycat
// MUST NOT COMPILE: a cursor opened in a transaction cannot outlive it. The
// C library requires every cursor be closed before its txn resolves; the
// binding's lifetimes enforce it — the cursor borrows the txn, so returning
// (and thus keeping) the cursor past the txn's drop is a borrow-check error.
fn main() {
    let env = libdb::Env::builder().open("h").unwrap();
    let db = env.database().btree().open("d.db").unwrap();
    let cursor = {
        let txn = env.begin().unwrap();
        db.cursor_txn(&txn).unwrap()
        // txn dropped here; `cursor` borrows it and escapes — error.
    };
    let _ = cursor.first();
}
