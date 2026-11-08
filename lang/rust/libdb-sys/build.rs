// SPDX-License-Identifier: Sleepycat
// build.rs — locate an installed libdb and link it.
//
// Honours (in order): LIBDB_DIR (a BerkeleyDB install prefix), then the
// well-known autoconf prefix, then the system linker path. The library is
// `libdb-<MAJOR>.<MINOR>` in both the autoconf and meson builds (rfc/0020).
// The compatibility levels are listed newest first; a program is built against
// exactly one of them.
use std::env;
use std::path::Path;
use std::process::Command;

fn compile_shim(include: &str) {
    // Compile the flat method shim against the real db.h and archive it.
    let out = env::var("OUT_DIR").unwrap();
    let obj = format!("{}/shim.o", out);
    let lib = format!("{}/libdbshim.a", out);
    let cc = env::var("CC").unwrap_or_else(|_| "cc".into());
    let st = Command::new(&cc)
        .args([
            "-O2",
            "-fPIC",
            "-c",
            "src/shim.c",
            "-o",
            &obj,
            &format!("-I{}", include),
        ])
        .status()
        .expect("run cc for shim.c");
    assert!(st.success(), "shim.c failed to compile");
    let ar = env::var("AR").unwrap_or_else(|_| "ar".into());
    let st = Command::new(&ar)
        .args(["crs", &lib, &obj])
        .status()
        .expect("run ar");
    assert!(st.success(), "ar failed");
    println!("cargo:rustc-link-search=native={}", out);
    println!("cargo:rustc-link-lib=static=dbshim");
    println!("cargo:rerun-if-changed=src/shim.c");
}

/// Compatibility levels this binding supports, newest first (rfc/0020).
const LEVELS: &[&str] = &["2026.1", "2026.0"];

fn main() {
    println!("cargo:rerun-if-env-changed=LIBDB_DIR");
    // Explicit prefix wins.
    if let Ok(dir) = env::var("LIBDB_DIR") {
        let p = Path::new(&dir);
        let lib = p.join("lib");
        let inc = p.join("include");
        for lvl in LEVELS {
            if lib.join(format!("libdb-{}.so", lvl)).exists()
                || lib.join(format!("libdb-{}.a", lvl)).exists()
            {
                println!("cargo:rustc-link-search=native={}", lib.display());
                println!("cargo:rustc-link-lib=db-{}", lvl);
                println!("cargo:include={}", inc.display());
                compile_shim(inc.to_str().unwrap());
                return;
            }
        }
    }
    // Well-known autoconf install prefixes.
    for lvl in LEVELS {
        let prefix = format!("/usr/local/BerkeleyDB.{}", lvl);
        if Path::new(&prefix)
            .join(format!("lib/libdb-{}.so", lvl))
            .exists()
        {
            println!("cargo:rustc-link-search=native={}/lib", prefix);
            println!("cargo:rustc-link-lib=db-{}", lvl);
            println!("cargo:include={}/include", prefix);
            compile_shim(&format!("{}/include", prefix));
            return;
        }
    }
    // Fall back to the system linker's default search path, newest level.
    println!("cargo:rustc-link-lib=db-{}", LEVELS[0]);
}
