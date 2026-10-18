// SPDX-License-Identifier: Sleepycat
// MUST NOT COMPILE: a transaction handle is dead after commit. `commit`
// consumes `self`, so any later use is a use-of-moved-value error — the C
// rule "the handle may not be accessed again after commit", enforced.
fn main() {
    let env = libdb::Env::builder().open("h").unwrap();
    let txn = env.begin().unwrap();
    txn.commit().unwrap();
    let _ = txn.id(); // use after move — error.
}
