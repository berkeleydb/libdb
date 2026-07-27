#!/usr/bin/env python3
"""kr2ansi.py -- collapse Berkeley DB K&R function definitions to ANSI so that
Coccinelle's spatch (1.3.0) can parse the function bodies.

spatch cannot parse the classic K&R definition BDB uses everywhere:

    int
    __memp_fput(dbmfp, ip, pgaddr, priority)
            DB_MPOOLFILE *dbmfp;
            DB_THREAD_INFO *ip;
            void *pgaddr;
            DB_CACHE_PRIORITY priority;
    {
            ...body...
    }

It reads "name(args)" as a call and the following "TYPE a;" lines as an
unexpected declaration, desyncs, and skips the whole function -- measured
directory-scale recall was ~13% without a fix.

This rewrite moves the opening "{" up onto the signature line and blanks the
line the "{" was on, so the parameter-declaration lines become the function's
first (redundant) local declarations and spatch parses the body:

    __memp_fput(dbmfp, ip, pgaddr, priority) {
            DB_MPOOLFILE *dbmfp;
            ...
                      <- was "{", now blank
            ...body...
    }

The transform is LINE-COUNT PRESERVING, so line N in the shimmed copy maps to
line N in the original -- spatch's reported line numbers stay accurate.

Usage: kr2ansi.py <in.c> <out.c>
"""
import re
import sys


def shim(src: str) -> str:
    lines = src.split("\n")
    out = []
    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]
        # A K&R signature: "ident(args)" alone on a line, no ; { }.
        m = re.match(r"^([A-Za-z_]\w*)\(([^;{}]*)\)\s*$", line)
        if m and i + 1 < n and re.match(r"^\s+\S", lines[i + 1]):
            j = i + 1
            # Consume indented "TYPE name;" declaration lines.
            while (
                j < n
                and lines[j].strip()
                and lines[j].rstrip().endswith(";")
                and re.match(r"^\s", lines[j])
            ):
                j += 1
            if j < n and lines[j].strip() == "{":
                out.append(line + " {")      # brace on signature line
                out.extend(lines[i + 1 : j]) # decls unchanged
                out.append("")               # was "{" -> blank; count preserved
                i = j + 1
                continue
        out.append(line)
        i += 1
    return "\n".join(out)


def main() -> None:
    if len(sys.argv) != 3:
        sys.exit("usage: kr2ansi.py <in.c> <out.c>")
    with open(sys.argv[1]) as f:
        data = f.read()
    with open(sys.argv[2], "w") as f:
        f.write(shim(data))


if __name__ == "__main__":
    main()
