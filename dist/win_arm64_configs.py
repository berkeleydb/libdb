#!/usr/bin/env python3
"""Add ARM64 platform configurations to the bundled MSVC projects.

One-shot mechanical transform, kept in dist/ for reproducibility: every
construct conditioned on `|x64` is duplicated as its `|ARM64` twin, with the
linker's /machine:x64 switched to /machine:arm64.  Run from the repo root:

    python3 dist/win_arm64_configs.py [--check]

--check reports whether a run would change anything (exit 1 if it would),
without writing.

Only the library plus the C utilities are given ARM64 configurations; the
Java/Tcl/SQL/PHP/STL/example projects stay x64-only because they need external
SDKs we do not cross-build.

Everything is done on BYTES.  build_windows files are CRLF (and
application.props has some bare CRs); the repo's .gitattributes pins *.sln and
*.vcxproj to eol=crlf, so any accidental LF normalisation shows up as a
multi-thousand-line phantom diff.  Duplicated lines are copied verbatim, so
whatever terminator a line had is preserved.

NOTE: build_windows/VS10/*.vcxproj and the two .props files are hand-maintained
AHEAD OF dist/s_windows_dsp -- that XQilla generator is stale (regenerating
drops src/os/os_atomic.c, src/os/os_aio*.c and src/os_windows/os_csprng.c,
which were added to the projects by hand after it was last run).  Do not
"regenerate" these files; patch them.
"""
import argparse
import pathlib
import re
import sys

# Projects that get ARM64: the library + the C command-line utilities.
ARM64_PROJECTS = [
    "db",
    "db_archive", "db_checkpoint", "db_deadlock", "db_dump", "db_hotbackup",
    "db_load", "db_log_verify", "db_printlog", "db_recover", "db_replicate",
    "db_stat", "db_tuner", "db_upgrade", "db_verify",
]

ROOT = pathlib.Path(__file__).resolve().parent.parent
VS10 = ROOT / "build_windows" / "VS10"
SLN = ROOT / "build_windows" / "Berkeley_DB_vs2010.sln"

# Elements whose |x64 instances get an ARM64 twin.  Multi-line elements are
# handled as a span from the opening tag to the matching close tag.
SPAN_TAGS = ("ProjectConfiguration", "PropertyGroup", "ItemDefinitionGroup")
LINE_TAGS = ("TargetName", "PreprocessorDefinitions")


def to_arm64(chunk):
    return (chunk.replace(b"|x64", b"|ARM64")
                 .replace(b"<Platform>x64</Platform>",
                          b"<Platform>ARM64</Platform>")
                 .replace(b"/machine:x64", b"/machine:arm64"))


def split_lines(data):
    """Split into lines keeping their terminators verbatim.

    Handles \r\n, \n, bare \r and -- because build_windows/VS10/*.props really
    do contain them -- \r\r\n.  The \r*\n branch must come first so a \r\r\n
    stays ONE line: splitting it into "text\r" + "\r\n" would make duplicated
    lines run together on one physical line.
    """
    return re.findall(rb"[^\r\n]*(?:\r*\n|\r|$)", data)[:-1]


def patch_xml(data):
    """Duplicate |x64 config elements as ARM64 twins.  Returns new bytes."""
    if b"|ARM64" in data:
        return data
    lines = split_lines(data)
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        # Single-line element with a |x64 condition.
        if b"|x64" in line and any(b"<" + t.encode() in line for t in LINE_TAGS) \
           and b"</" in line:
            out.append(line)
            out.append(to_arm64(line))
            i += 1
            continue
        # Multi-line span: <Tag ... |x64 ...> ... </Tag>
        tag = next((t for t in SPAN_TAGS
                    if line.lstrip().startswith(b"<" + t.encode())
                    and b"|x64" in line), None)
        if tag:
            close = b"</" + tag.encode() + b">"
            j = i
            while j < len(lines) and close not in lines[j]:
                j += 1
            span = lines[i:j + 1]
            out.extend(span)
            out.extend(to_arm64(s) for s in span)
            i = j + 1
            continue
        out.append(line)
        i += 1
    return b"".join(out)


def patch_sln(data, guids):
    """Duplicate the |x64 solution/project configuration lines as ARM64."""
    if b"|ARM64" in data:
        return data
    sol_cfg = re.compile(rb"^[\s]*[\w ]+\|x64 = [\w ]+\|x64[\s]*$")
    prj_cfg = re.compile(rb"^[\s]*\{([0-9A-Fa-f-]+)\}\.[\w ]+\|x64\.")
    out = []
    for line in split_lines(data):
        out.append(line)
        if sol_cfg.match(line):
            out.append(to_arm64(line))
            continue
        m = prj_cfg.match(line)
        if m and m.group(1).upper().decode() in guids:
            out.append(to_arm64(line))
    return b"".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true",
                    help="report needed changes, write nothing")
    args = ap.parse_args()

    guids = set()
    targets = []      # (path, new_bytes)
    for name in ARM64_PROJECTS:
        p = VS10 / (name + ".vcxproj")
        if not p.exists():
            sys.exit("missing project: %s" % p)
        data = p.read_bytes()
        m = re.search(rb"<ProjectGuid>\{([0-9A-Fa-f-]+)\}</ProjectGuid>", data)
        if not m:
            sys.exit("no ProjectGuid in %s" % p)
        guids.add(m.group(1).upper().decode())
        targets.append((p, patch_xml(data)))

    for props in ("library.props", "application.props"):
        p = VS10 / props
        targets.append((p, patch_xml(p.read_bytes())))

    targets.append((SLN, patch_sln(SLN.read_bytes(), guids)))

    changed = 0
    for path, new in targets:
        if new == path.read_bytes():
            continue
        changed += 1
        rel = path.relative_to(ROOT)
        if args.check:
            print("would patch %s" % rel)
        else:
            path.write_bytes(new)
            print("patched %s" % rel)

    if args.check:
        print("%d file(s) need ARM64 configurations" % changed)
        return 1 if changed else 0
    print("%d file(s) patched" % changed)
    return 0


if __name__ == "__main__":
    sys.exit(main())
