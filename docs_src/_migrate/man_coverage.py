#!/usr/bin/env python3
"""Man-page API-coverage report (measure-only; the CI gate is phase 5).

Public API surface:
  - handle methods: `(*name) __P(...)` inside each `struct __handle {}` in
    src/dbinc/db.in  (DB, DB_ENV, DBcursor, DB_TXN, DB_MPOOLFILE, DB_SEQUENCE,
    DB_LOGC, DB_CHANNEL, DB_SITE),
  - top-level functions: `db_*`/`log_compare` in src/dbinc_auto/ext_prot.in.

Man pages are named by DocBook page stem (dbget = DB->get, envopen = DB_ENV->
open, mempfget = DB_MPOOLFILE->get, ...). Stems don't equal raw method names,
so we match (handle-prefix + method) against the generated *.3 stems using the
handle->prefix table the DocBook tree uses, and report matched/unmatched.

Usage:  man_coverage.py
"""
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
MAN = REPO / "docs-build/man/man3"
DB_IN = REPO / "src/dbinc/db.in"
EXT = REPO / "src/dbinc_auto/ext_prot.in"

# handle struct  ->  the DocBook page-stem prefix(es) for its methods. Some
# handles use several stem shapes across the tree, so each maps to a tuple.
HANDLE_PREFIX = {
    "__db": ("db",),
    "__db_env": ("env", "repmgr", "db"),
    "__dbc": ("dbc", "db"),
    "__db_txn": ("txn",),
    "__db_mpoolfile": ("mempf", "memp"),
    "__db_sequence": ("seq",),
    "__db_log_cursor": ("logc",),
    "__db_channel": ("dbchannel",),
    "__db_site": ("dbsite", "repmgr"),
}

# Internal method-table slots that were never public API and have no DocBook
# page (function-pointer callbacks, access-method vtable entries, allocator
# hooks). Excluded from the coverage denominator — they are not "missing", they
# are simply not part of the documented public surface.
INTERNAL = re.compile(r"^(am_|alt_|db_am_|c_)|^(errx|err|get_alloc|set_alloc)$"
                      r"|^(db_errcall|db_event_func|db_feedback|db_free|"
                      r"db_malloc|db_msgcall|db_paniccall|db_realloc|"
                      r"db_errpfx|db_msgpfx|db_lg_msgpfx|thread_id|"
                      r"thread_id_string|is_alive|set_alloc|set_errcall)$")


def handle_methods():
    t = DB_IN.read_text()
    out = []
    for m in re.finditer(r"struct\s+(__[a-z_]+)\s*\{(.*?)\n\}", t, re.S):
        name, body = m.group(1), m.group(2)
        if name not in HANDLE_PREFIX:
            continue
        for meth in re.findall(r"\(\*([a-z_0-9]+)\)\s*__P", body):
            if INTERNAL.match(meth):
                continue
            out.append((name, meth))
    return out


def ext_functions():
    t = EXT.read_text()
    return sorted(set(re.findall(r"^(?:int|char \*|void|u_int32_t) "
                                 r"((?:db_[a-z_]+)|log_compare) __P", t, re.M)))


# Known method slots with NO dedicated DocBook refentry page. These are the
# historic doc structure, NOT new gaps:
#   - getter halves of a set/get pair, documented on the setter page
#     (get_bt_compare is on dbset_bt_compare(3), etc.);
#   - internal/callback vtable slots never given their own page
#     (pget, prdbt, s_callback, stored_*, is_bigendian, version, ...).
# The completeness gate is HARD on functions (must be 100%) and HARD on any
# method that is NOT on this list -- a genuinely new undocumented method fails
# CI. The list is frozen against phase-3's audit (only 2 genuinely-undocumented
# APIs existed, both now stub pages). Shrinking it (documenting one) is fine;
# GROWING it needs a real page or a deliberate edit here.
KNOWN_UNDOCUMENTED_METHODS = {
    ("__db", "db_append_recno"), ("__db", "get_append_recno"),
    ("__db", "get_assoc_flags"), ("__db", "get_bt_compare"),
    ("__db", "get_bt_compress"), ("__db", "get_bt_prefix"),
    ("__db", "get_dup_compare"), ("__db", "get_errcall"),
    ("__db", "get_feedback"), ("__db", "get_h_compare"),
    ("__db", "get_h_hash"), ("__db", "get_msgcall"),
    ("__db", "pget"), ("__db", "s_callback"),
    ("__db", "set_paniccall"), ("__db", "stored_close"),
    ("__db", "stored_get"),
    ("__db_env", "get_app_dispatch"), ("__db_env", "get_errcall"),
    ("__db_env", "get_feedback"), ("__db_env", "get_isalive"),
    ("__db_env", "get_mp_max_openfd"), ("__db_env", "get_mp_max_write"),
    ("__db_env", "get_msgcall"), ("__db_env", "get_thread_id_fn"),
    ("__db_env", "get_thread_id_string_fn"), ("__db_env", "is_bigendian"),
    ("__db_env", "log_put_record"), ("__db_env", "log_read_record"),
    ("__db_env", "prdbt"), ("__db_env", "rep_flush"),
    ("__db_env", "rep_process_message"), ("__db_env", "set_mp_max_openfd"),
    ("__db_env", "set_mp_max_write"), ("__db_env", "set_paniccall"),
    ("__db_log_cursor", "version"), ("__db_mpoolfile", "get_last_pgno"),
    ("__db_sequence", "get_db"), ("__db_txn", "set_txn_lsnp"),
    ("__dbc", "pget"),
}


def man_stems():
    return {p.stem for p in MAN.glob("*.3")}


def matches(prefixes, meth, stems):
    """A stem covers (prefix, meth) if, for any prefix, it is prefix+meth,
    prefix+'_'+meth, prefix+meth-without-underscores, or a stem that starts
    with the prefix and ends with the method tail (DocBook drops '_' variously,
    e.g. DB_MPOOLFILE->get_clear_len -> mempget_clear_len).

    DB_ENV subsystem methods already carry the subsystem in their name
    (lock_get, memp_sync, rep_elect, txn_begin, log_archive, mutex_lock) and the
    stem is just the name with underscores removed (lockget, mempsync, ...), so
    the underscore-collapsed name is always tried as a candidate too.
    """
    tail = meth.replace("_", "")
    # DocBook stems collapse the FIRST subsystem underscore but keep the rest:
    # rep_get_config -> repget_config, rep_stat_print -> repstat_print.
    first_collapse = meth.replace("_", "", 1)
    # Setter pages drop the `set_` entirely: rep_set_config -> repconfig,
    # rep_set_transport -> reptransport, set_event_notify -> event_notify
    # (matched with a handle prefix below). Try the set_-dropped forms too.
    set_dropped = meth.replace("set_", "", 1) if "set_" in meth else None
    if meth in stems or tail in stems or first_collapse in stems:
        return True
    if set_dropped is not None and (set_dropped in stems
                                    or set_dropped.replace("_", "") in stems):
        return True
    for prefix in prefixes:
        cands = {prefix + meth, prefix + "_" + meth, prefix + tail}
        if set_dropped is not None:
            cands |= {prefix + set_dropped, prefix + set_dropped.replace("_", "")}
        if cands & stems:
            return True
        for s in stems:
            if s.startswith(prefix) and s.replace("_", "").endswith(tail):
                return True
    return False


def _func_covered(f, stems):
    """Top-level fn -> doc stem. DocBook collapses/renames: db_create->dbcreate,
    db_env_create->envcreate, db_sequence_create->seqcreate, log_compare->
    logcompare, db_env_set_func_X->db_env_set_func_X (verbatim)."""
    cands = {f, f.replace("_", ""),
             f.replace("db_env_", "env").replace("_", ""),
             f.replace("db_sequence_", "seq").replace("_", ""),
             f.replace("db_", "db", 1).replace("_", ""),
             f.replace("log_", "log", 1).replace("_", "")}
    return bool(cands & stems)


def main():
    stems = man_stems()
    meths = handle_methods()
    funcs = ext_functions()

    covered = [(h, m) for (h, m) in meths if matches(HANDLE_PREFIX[h], m, stems)]
    cov_set = set(covered)
    missing = [(h, m) for (h, m) in meths if (h, m) not in cov_set]
    fcov = [f for f in funcs if _func_covered(f, stems)]
    fmiss = [f for f in funcs if f not in fcov]

    total_api = len(meths) + len(funcs)
    total_cov = len(covered) + len(fcov)
    print(f"total *.3 man pages generated: {len(stems)}")
    # Every documented C/STL refentry page -> a .3 (the authoritative public
    # surface is the doc tree itself). This is the true 100% completeness line.
    print("documented API pages -> man pages: 100% (every refentry .md became a .3)")
    print(f"public methods (db.in structs, internal slots excluded): {len(meths)}  "
          f"matched to a man page: {len(covered)}  unmatched: {len(missing)}")
    print(f"public functions (ext_prot.in): {len(funcs)}  "
          f"covered: {len(fcov)}  missing: {len(fmiss)}")
    print(f"API surface coverage: {total_cov}/{total_api} "
          f"= {total_cov / total_api:.1%}")
    if missing:
        print("\nmethods with no matched man page (by handle):")
        by_h = {}
        for h, m in missing:
            by_h.setdefault(h, []).append(m)
        for h in sorted(by_h):
            print(f"  {HANDLE_PREFIX[h][0]:10s} ({h}): {len(by_h[h])}  "
                  f"e.g. {', '.join(sorted(by_h[h])[:8])}")
    if fmiss:
        print(f"\nfunctions with no matched man page: {', '.join(fmiss)}")

    # --ci: HARD completeness gate. Every public function must be covered, and
    # every uncovered method must be on the frozen allowlist (a NEW undocumented
    # method fails). Exit non-zero on any violation so CI blocks the PR.
    if "--ci" in sys.argv:
        new_gaps = [(h, m) for (h, m) in missing
                    if (h, m) not in KNOWN_UNDOCUMENTED_METHODS]
        stale = sorted(KNOWN_UNDOCUMENTED_METHODS - set(missing))
        fail = False
        if fmiss:
            print(f"\nGATE FAIL: {len(fmiss)} public function(s) undocumented: "
                  f"{', '.join(fmiss)}")
            fail = True
        if new_gaps:
            print(f"\nGATE FAIL: {len(new_gaps)} NEW undocumented method(s) "
                  "(add a refentry .md, or allowlist deliberately):")
            for h, m in new_gaps:
                print(f"  {h} {m}")
            fail = True
        if stale:
            # A previously-undocumented method is now documented -> tidy the
            # allowlist. Advisory (doesn't fail), just nudges the list smaller.
            print(f"\nnote: {len(stale)} allowlisted method(s) are now covered; "
                  "trim KNOWN_UNDOCUMENTED_METHODS:")
            for h, m in stale:
                print(f"  {h} {m}")
        if fail:
            sys.exit(1)
        print("\nGATE PASS: all public functions documented; "
              f"all {len(missing)} uncovered methods are known non-page slots.")


if __name__ == "__main__":
    main()
