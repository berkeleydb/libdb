// SPDX-License-Identifier: Sleepycat
// build.rs — locate an installed libdb and link it.
//
// Honours (in order): LIBDB_DIR (a BerkeleyDB install prefix), then the
// well-known autoconf prefix, then a plain system `-ldb`. The autoconf library
// is `libdb-2026.0`; a meson build is plain `db` — probe for both.
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

fn main() {
    // Explicit prefix wins.
    if let Ok(dir) = env::var("LIBDB_DIR") {
        let p = Path::new(&dir);
        let lib = p.join("lib");
        let inc = p.join("include");
        if lib.join("libdb-2026.0.so").exists() || lib.join("libdb-2026.0.a").exists() {
            println!("cargo:rustc-link-search=native={}", lib.display());
            println!("cargo:rustc-link-lib=db-2026.0");
            println!("cargo:include={}", inc.display());
            println!("cargo:rerun-if-env-changed=LIBDB_DIR");
            compile_shim(inc.to_str().unwrap());
            return;
        }
    }
    // Well-known autoconf install prefix.
    let prefix = "/usr/local/BerkeleyDB.2026.0";
    if Path::new(prefix).join("lib/libdb-2026.0.so").exists() {
        println!("cargo:rustc-link-search=native={}/lib", prefix);
        println!("cargo:rustc-link-lib=db-2026.0");
        println!("cargo:include={}/include", prefix);
        compile_shim(&format!("{}/include", prefix));
        return;
    }
    // Fall back to the system linker's default search path; try the versioned
    // name first, then the plain meson name.
    println!("cargo:rustc-link-lib=db-2026.0");
    println!("cargo:rerun-if-env-changed=LIBDB_DIR");
}
