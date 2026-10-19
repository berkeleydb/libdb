// SPDX-License-Identifier: Sleepycat
// A Db is Send + Sync, and libdb may invoke its comparator from any thread
// that holds the handle. So a comparator capturing a !Send/!Sync value (here an
// Rc) must be rejected at the builder, or a Send+Sync Db would smuggle it
// across threads. This must FAIL to compile.
use std::rc::Rc;

fn main() {
    let env = libdb::Env::builder().open("/tmp/never-opened").unwrap();
    let rc = Rc::new(0u32);
    let _db = env
        .database()
        .btree()
        .bt_compare(move |a, b| {
            let _ = &rc;
            a.cmp(b)
        })
        .open("x.db");
}
