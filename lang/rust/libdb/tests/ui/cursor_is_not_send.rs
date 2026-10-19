// SPDX-License-Identifier: Sleepycat
// MUST NOT COMPILE: two cursors opened on ONE transaction cannot be split
// across threads. The C library requires a transaction and all its cursors to
// be used by a single thread; sending a cursor to another thread while another
// cursor on the same txn runs here drives libdb into a fatal region error.
//
// The binding makes a cursor `!Send` structurally: it borrows the `Txn`, and
// because `Txn` is `!Sync`, `&Txn` is `!Send`, so the cursor is `!Send`. This
// `std::thread::scope` spawn therefore fails to compile — exactly the C
// single-thread-affinity rule, enforced for free.
fn main() {
    let env = libdb::Env::builder().open("h").unwrap();
    let db = env.database().btree().open("d.db").unwrap();
    let txn = env.begin().unwrap();
    let c1 = db.cursor_txn(&txn).unwrap();
    let c2 = db.cursor_txn(&txn).unwrap();
    std::thread::scope(|s| {
        // moving c2 (a !Send cursor sharing `txn` with c1) into another thread
        // is a compile error.
        s.spawn(move || {
            let _ = c2.first();
        });
        let _ = c1.first();
    });
}
