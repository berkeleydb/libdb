// SPDX-License-Identifier: Sleepycat
//! The DRIFT GATE, with teeth.
//!
//! This test emits a C program *from the binding's own constant list*
//! ([`libdb_sys::CONSTANTS`]) in which each entry becomes a
//! `_Static_assert((C_NAME) == (baked value))`. It compiles that program
//! against the installed `db.h`. If any baked value disagrees with the header,
//! the C compile FAILs with a `static_assertion failed` diagnostic naming the
//! constant, which fails this test.
//!
//! Sabotage drill (house rule): bump any value in `CONSTANTS` and this test
//! prints the compiler's failure line and fails. Restore it and it passes.
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

/// Resolve the libdb include directory the same way `build.rs` does.
fn include_dir() -> PathBuf {
    if let Ok(dir) = std::env::var("LIBDB_DIR") {
        let inc = PathBuf::from(&dir).join("include");
        if inc.join("db.h").exists() {
            return inc;
        }
    }
    let well_known = PathBuf::from("/usr/local/BerkeleyDB.2026.0/include");
    if well_known.join("db.h").exists() {
        return well_known;
    }
    // Last resort: assume the compiler's default search path has db.h.
    PathBuf::from(".")
}

#[test]
fn constants_match_installed_header() {
    let inc = include_dir();

    // Generate the C from the binding's constant table.
    let mut c = String::new();
    // db.h uses u_int/u_long (BSD type names). On glibc they are only visible
    // with a feature macro + <sys/types.h>; on the BSDs they are default. Pull
    // them in before db.h so this check compiles on Linux and the BSDs alike
    // (a C program using db.h on Linux must do the same).
    c.push_str("#define _DEFAULT_SOURCE 1\n");
    c.push_str("#include <sys/types.h>\n");
    c.push_str("#include <db.h>\n");
    for (name, value) in libdb_sys::CONSTANTS {
        // Cast both sides to long long so a negative DB_* code compares as its
        // signed value, matching how we store it (i64) in CONSTANTS.
        c.push_str(&format!(
            "_Static_assert((long long)({name}) == (long long){value}, \
             \"DRIFT: {name} != {value}\");\n"
        ));
    }
    c.push_str("int main(void){return 0;}\n");

    let out = std::env::var("OUT_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| std::env::temp_dir());
    let cfile = out.join("drift_gate_check.c");
    let mut f = std::fs::File::create(&cfile).expect("create drift C file");
    f.write_all(c.as_bytes()).expect("write drift C file");
    drop(f);

    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".into());
    let output = Command::new(&cc)
        .arg("-std=c11")
        .arg("-fsyntax-only")
        .arg(format!("-I{}", inc.display()))
        .arg(&cfile)
        .output()
        .expect("run cc for the drift gate");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Surface the compiler's FAIL line(s) — the house rule asks that the
        // sabotage FAIL be visible and pastable.
        panic!(
            "DRIFT GATE FAILED — a baked constant disagrees with the installed db.h:\n{}",
            stderr
        );
    }
}
