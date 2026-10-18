// SPDX-License-Identifier: Sleepycat
//! The type system IS the test: these cases must FAIL to compile. `trybuild`
//! compiles each `ui/*.rs` and checks it errors (and, where a `.stderr` file
//! is present, that the message matches). This proves the lifetime encoding of
//! the C lifecycle rules — a cursor cannot outlive its transaction, and a
//! transaction handle is dead after `commit`/`abort` (both consume `self`).
#[test]
fn ui_lifecycle_rules_are_compile_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/cursor_outlives_txn.rs");
    t.compile_fail("tests/ui/txn_use_after_commit.rs");
    t.compile_fail("tests/ui/value_escapes_with_value.rs");
}
