#!/usr/bin/env python3
"""Generate a Berkeley DB header the way dist/configure does: concatenate one
or more template (.in) files and apply @VAR@ substitutions.

Usage: gen_header.py <out.h> <subs.json> <in1> [in2 ...]
"""
import json
import os
import re
import sys


def main():
    out, subs_path = sys.argv[1], sys.argv[2]
    inputs = sys.argv[3:]
    subs = json.load(open(subs_path))

    # dist/RELEASE is the single source of truth for the version triplet and the
    # CalVer identity.  db_subs.json used to carry its own copies, and they drifted:
    # RELEASE said 2026.0.9 (which every shipped release has used -- the soname has
    # been libdb-2026.0.so since v2026.04) while db_subs.json still said 5.3.37, so
    # the meson build emitted a db.h claiming 5.3.29 and produced a library that
    # could not attach an environment created by the autoconf build.  abidiff and
    # the region-signature gate both stay green through that, because each build is
    # internally consistent.  Overriding from RELEASE here means the two build
    # systems cannot disagree again, no matter what the JSON says.
    rel = os.path.join(os.path.dirname(os.path.abspath(subs_path)), "..", "RELEASE")
    if os.path.exists(rel):
        for line in open(rel, encoding="utf-8"):
            line = line.strip()
            if not line or line.startswith("#") or "=" not in line:
                continue
            key, _, val = line.partition("=")
            key = key.strip()
            if key not in ("DB_VERSION_MAJOR", "DB_VERSION_MINOR",
                           "DB_VERSION_PATCH", "DB_CALVER", "DB_RELEASE_DATE"):
                continue
            val = val.strip().strip('"')
            if "$" in val:          # unexpanded shell reference; leave the JSON's
                continue
            subs[key] = val
        # NOT derived here: DB_VERSION_UNIQUE_NAME.  options.m4:479-486 leaves it
        # EMPTY unless --with-uniquename is given, so inventing a value here would
        # mangle every public symbol in the meson build only.
        cal = subs.get("DB_CALVER", "")
        date = subs.get("DB_RELEASE_DATE", "")
        if cal and date:
            # db.in has NO quotes around @DB_VERSION_STRING@, so the substituted
            # value must carry its own -- that is why db_subs.json stores the
            # quotes inside the string.  Omitting them emits
            #   #define DB_VERSION_STRING libdb 2026.09.7 (September 18, 2026)
            # which is a syntax error ("too many decimal points in number").
            ver = '"libdb %s (%s)"' % (cal, date)
            subs["DB_VERSION_STRING"] = ver
            subs["DB_VERSION_FULL_STRING"] = ver
    text = "".join(open(f, encoding="utf-8").read() for f in inputs)
    # Replace known @VAR@; leave unknown tokens untouched so they're visible.
    text = re.sub(r"@([A-Za-z_0-9]+)@",
                  lambda m: subs.get(m.group(1), m.group(0)), text)
    with open(out, "w", encoding="utf-8") as fh:
        fh.write(text)


if __name__ == "__main__":
    main()
