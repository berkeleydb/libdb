#!/usr/bin/env python3
"""Build the Berkeley DB historical chain on an orphan `historical` branch.

For each release (in release order): clear the tree, extract the upstream
tarball into the repo root, commit, and tag vX.Y.Z. Each upstream patch is
applied (GNU patch, -p0 from the source root) as its own commit tagged
vX.Y.Z.N. No-crypto (.NC) variants are committed as a one-commit side branch
off the base release tag and tagged vX.Y.Z-NC.

Idempotent: deletes the historical branch and all managed tags before running.
master and the existing v5.3.x tags are never touched.
"""
import os
import re
import shutil
import subprocess
import sys
import tempfile
from datetime import datetime, timedelta

REPO = "/Users/gregburd/oss/libdb-historical"
MIRROR = "/Users/gregburd/oss/bdb-mirror"
PATCH = "gpatch"

# Ordered release timeline. Each entry: version, tarball, optional patches
# (label, filename), optional patch subdir, optional NC tarball, optional
# fallback date (for releases whose README carries no parseable date).
RELEASES = [
    {"v": "1.85", "tar": "db.1.85.tar.gz", "psub": "1.85", "date": "1992-08-01",
     "patches": [("1", "patch.1.1"), ("2", "patch.1.2"), ("3", "patch.1.3"), ("4", "patch.1.4")]},
    {"v": "1.86", "tar": "db.1.86.tar.gz", "date": "1996-01-01"},
    {"v": "2.7.7", "tar": "db-2.7.7.tar.gz"},
    {"v": "3.0.55", "tar": "db-3.0.55.tar.gz", "patches": [("1", "patch.3.0.55.1")]},
    {"v": "3.1.17", "tar": "db-3.1.17.tar.gz"},
    {"v": "3.2.9", "tar": "db-3.2.9.tar.gz", "patches": [("1", "patch.3.2.9.1"), ("2", "patch.3.2.9.2")]},
    {"v": "3.3.11", "tar": "db-3.3.11.tar.gz", "patches": [("1", "patch.3.3.11.1"), ("2", "patch.3.3.11.2")]},
    {"v": "4.0.14", "tar": "db-4.0.14.tar.gz"},
    {"v": "4.1.25", "tar": "db-4.1.25.tar.gz", "nc": "db-4.1.25.NC.tar.gz",
     "patches": [("1", "patch.4.1.25.1"), ("2", "patch.4.1.25.2"), ("3", "patch.4.1.25.3")]},
    {"v": "4.2.52", "tar": "db-4.2.52.tar.gz", "nc": "db-4.2.52.NC.tar.gz",
     "patches": [("1", "patch.4.2.52.1"), ("2", "patch.4.2.52.2"), ("3", "patch.4.2.52.3"),
                 ("4", "patch.4.2.52.4"), ("5", "patch.4.2.52.5")]},
    {"v": "4.3.29", "tar": "db-4.3.29.tar.gz", "patches": [("1", "patch.4.3.29.1")]},
    {"v": "4.4.20", "tar": "db-4.4.20.tar.gz",
     "patches": [("1", "patch.4.4.20.1"), ("2", "patch.4.4.20.2"), ("3", "patch.4.4.20.3"), ("4", "patch.4.4.20.4")]},
    {"v": "4.5.20", "tar": "db-4.5.20.tar.gz", "patches": [("1", "patch.4.5.20.1"), ("2", "patch.4.5.20.2")]},
    {"v": "4.6.21", "tar": "db-4.6.21.tar.gz",
     "patches": [("1", "patch.4.6.21.1"), ("2", "patch.4.6.21.2"), ("3", "patch.4.6.21.3"), ("4", "patch.4.6.21.4")]},
    {"v": "4.7.25", "tar": "db-4.7.25.tar.gz",
     "patches": [("1", "patch.4.7.25.1"), ("2", "patch.4.7.25.2"), ("3", "patch.4.7.25.3"), ("4", "patch.4.7.25.4")]},
    {"v": "4.8.30", "tar": "db-4.8.30.tar.gz"},
    {"v": "5.0.32", "tar": "db-5.0.32.tar.gz"},
    {"v": "5.1.29", "tar": "db-5.1.29.tar.gz"},
]

log_lines = []


def log(msg):
    print(msg, flush=True)
    log_lines.append(msg)


def run(cmd, cwd=REPO, env=None, check=True, capture=False):
    res = subprocess.run(cmd, cwd=cwd, env=env, text=True,
                         capture_output=True)
    if check and res.returncode != 0:
        raise RuntimeError(f"cmd failed ({res.returncode}): {' '.join(cmd)}\n"
                           f"STDOUT:\n{res.stdout}\nSTDERR:\n{res.stderr}")
    if capture:
        return res
    return res


def git(*args, **kw):
    return run(["git", *args], **kw)


def clear_tree():
    for name in os.listdir(REPO):
        if name == ".git":
            continue
        path = os.path.join(REPO, name)
        if os.path.isdir(path) and not os.path.islink(path):
            shutil.rmtree(path)
        else:
            os.remove(path)


def extract_into_repo(tarball):
    with tempfile.TemporaryDirectory() as td:
        run(["tar", "xzf", os.path.join(MIRROR, tarball), "-C", td], cwd=td)
        entries = [e for e in os.listdir(td) if not e.startswith(".")]
        # Single top-level directory is the norm for these tarballs.
        if len(entries) == 1 and os.path.isdir(os.path.join(td, entries[0])):
            srcroot = os.path.join(td, entries[0])
        else:
            srcroot = td
        for name in os.listdir(srcroot):
            src = os.path.join(srcroot, name)
            dst = os.path.join(REPO, name)
            if os.path.islink(src):
                os.symlink(os.readlink(src), dst)
            elif os.path.isdir(src):
                shutil.copytree(src, dst, symlinks=True)
            else:
                shutil.copy2(src, dst)


def read_release_date(fallback):
    for readme in ("README", "README.md"):
        p = os.path.join(REPO, readme)
        if os.path.isfile(p):
            with open(p, encoding="utf-8", errors="replace") as fh:
                head = fh.read(2000)
            m = re.search(r"\(([A-Z][a-z]+ +\d+, +\d{4})\)", head)
            if m:
                try:
                    return datetime.strptime(re.sub(r" +", " ", m.group(1)),
                                             "%B %d, %Y")
                except ValueError:
                    pass
    if fallback:
        return datetime.strptime(fallback, "%Y-%m-%d")
    return None


def parse_patch_date(patchfile, base_dt, n):
    """Best-effort date from the patch's target header; fall back to base+n days."""
    try:
        with open(os.path.join(MIRROR, patchfile), encoding="utf-8", errors="replace") as fh:
            text = fh.read(4000)
    except OSError:
        return base_dt + timedelta(days=n)
    cands = re.findall(r"^(?:---|\+\+\+|\*\*\*) .*?\t(.+)$", text, re.MULTILINE)
    for raw in cands:
        raw = raw.strip()
        m = re.match(r"(\d{4}-\d{2}-\d{2})", raw)
        if m:
            dt = datetime.strptime(m.group(1), "%Y-%m-%d")
            if dt.year > 1970:
                return dt
        m = re.match(r"[A-Z][a-z]{2} +([A-Z][a-z]{2}) +(\d+) +[\d:]+ +(\d{4})", raw)
        if m and int(m.group(3)) > 1970:
            try:
                return datetime.strptime(f"{m.group(1)} {int(m.group(2)):02d} {m.group(3)}",
                                         "%b %d %Y")
            except ValueError:
                pass
    return base_dt + timedelta(days=n)


def commit(message, dt):
    env = dict(os.environ)
    iso = dt.strftime("%Y-%m-%dT12:00:00")
    env["GIT_AUTHOR_DATE"] = iso
    env["GIT_COMMITTER_DATE"] = iso
    git("add", "-A")
    git("commit", "-q", "--no-verify", "-m", message, env=env)


def tag(name, message, dt):
    env = dict(os.environ)
    env["GIT_COMMITTER_DATE"] = dt.strftime("%Y-%m-%dT12:00:00")
    git("tag", "-a", name, "-m", message, env=env)


def apply_patch(patchfile, special_makefile):
    """Apply one upstream patch. Returns (ok, detail)."""
    if special_makefile:
        # patch.1.1 targets a bare `Makefile`; apply to every PORT/*/Makefile.
        import glob
        mks = sorted(glob.glob(os.path.join(REPO, "PORT", "*", "Makefile")))
        applied = 0
        for mk in mks:
            with open(os.path.join(MIRROR, patchfile), "rb") as fh:
                res = subprocess.run([PATCH, "-p0", "--no-backup-if-mismatch", mk],
                                     cwd=REPO, stdin=fh, text=True, capture_output=True)
            if res.returncode == 0:
                applied += 1
        return (applied > 0, f"applied to {applied}/{len(mks)} PORT/*/Makefile")
    with open(os.path.join(MIRROR, patchfile), "rb") as fh:
        res = subprocess.run([PATCH, "-p0", "--no-backup-if-mismatch"],
                             cwd=REPO, stdin=fh, text=True, capture_output=True)
    return (res.returncode == 0, (res.stdout + res.stderr).strip().replace("\n", " | "))


def reset_branch_and_tags():
    # Remove managed tags (everything except the pre-existing v5.3.x master tags).
    res = git("tag", "-l", capture=True)
    keep = {"v5.3.21", "v5.3.28", "v5.3.29"}
    for t in res.stdout.split():
        if t not in keep:
            git("tag", "-d", t)
    git("checkout", "-fq", "master")
    run(["git", "clean", "-fdxq"])
    res = git("branch", "--list", "historical", capture=True)
    if res.stdout.strip():
        git("branch", "-D", "historical")
    for b in ("_nc",):
        r = git("branch", "--list", b, capture=True)
        if r.stdout.strip():
            git("branch", "-D", b)


def main():
    reset_branch_and_tags()
    git("checkout", "-q", "--orphan", "historical")
    git("rm", "-rfq", "--ignore-unmatch", ".")
    clear_tree()

    summary = []
    for rel in RELEASES:
        v = rel["v"]
        clear_tree()
        extract_into_repo(rel["tar"])
        dt = read_release_date(rel.get("date"))
        if dt is None:
            dt = datetime(2000, 1, 1)
        commit(f"import: Berkeley DB {v} ({dt.strftime('%Y-%m-%d')})\n\n"
               f"Upstream source tarball {rel['tar']} imported verbatim.", dt)
        tag(f"v{v}", f"Berkeley DB {v}", dt)
        base_tag = f"v{v}"
        log(f"[base] v{v}  ({dt.strftime('%Y-%m-%d')})")
        summary.append(f"v{v}")

        for i, (label, pf) in enumerate(rel.get("patches", []), start=1):
            special = (pf == "patch.1.1")
            psub = rel.get("psub")
            srcpf = os.path.join(psub, pf) if psub else pf
            ok, detail = apply_patch(srcpf, special)
            if not ok:
                log(f"  [PATCH FAILED] v{v}.{label} <- {srcpf}: {detail}")
                summary.append(f"v{v}.{label} FAILED")
                raise SystemExit(f"patch {srcpf} failed; aborting for review")
            pdt = parse_patch_date(srcpf, dt, i)
            if pdt < dt:
                pdt = dt + timedelta(days=i)
            commit(f"patch: Berkeley DB {v} patch {label}\n\n"
                   f"Upstream patch file {pf} ({detail}).", pdt)
            tag(f"v{v}.{label}", f"Berkeley DB {v} patch {label}", pdt)
            log(f"  [patch] v{v}.{label}  ({pdt.strftime('%Y-%m-%d')})  {detail}")
            summary.append(f"v{v}.{label}")

        if rel.get("nc"):
            git("checkout", "-fq", "-b", "_nc", base_tag)
            run(["git", "clean", "-fdxq"])
            clear_tree()
            extract_into_repo(rel["nc"])
            ncdt = dt + timedelta(hours=1)
            commit(f"import: Berkeley DB {v} (NC, no-crypto export variant)\n\n"
                   f"Upstream source tarball {rel['nc']} imported verbatim. "
                   f"Tagged off the v{v} base release.", ncdt)
            tag(f"v{v}-NC", f"Berkeley DB {v} no-crypto (NC) variant", ncdt)
            git("checkout", "-fq", "historical")
            git("branch", "-D", "_nc")
            log(f"  [nc] v{v}-NC")
            summary.append(f"v{v}-NC")

    log("\n=== TAG SUMMARY (%d tags) ===" % len(summary))
    log(" ".join(summary))
    with open(os.path.join(MIRROR, "import_log.txt"), "w") as fh:
        fh.write("\n".join(log_lines) + "\n")


if __name__ == "__main__":
    main()
