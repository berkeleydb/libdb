#!/usr/bin/env python3
"""Completeness / no-loss gate for the reverse-DocBook migration.

For each old page docs/api_reference/C/<x>.html and its new
docs-src/api/c/<x>.md, compare CONTENT so we can prove nothing was dropped:

  1. Visible-word multiset retention: intersection(new, old) / |old|, over the
     visible text of each (boilerplate navheader/navfooter excluded from OLD by
     reusing the same content-div isolation the extractor uses; front-matter and
     link/HTML noise excluded from NEW). A page below --threshold is flagged.
  2. Code blocks: count of `programlisting` <pre> in OLD vs ``` fences in NEW.
  3. Parameter/Error/sub-section headers: count of <h4 class="title"> in OLD vs
     `####` headings in NEW.

Any drop in (2) or (3), or retention below threshold in (1), is an OUTLIER.

Usage:  verify.py [--threshold 0.97]
Exit non-zero if any hard drop (code block / sub-section) is detected, so CI can
gate on it. Low word-retention is reported but (this phase) not fatal unless it
also drops structure \u2014 pandoc reflow/normalization loses stopwords legitimately.
"""
import html as htmllib
import re
import sys
from collections import Counter
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
OLD = REPO / "docs/api_reference/C"
NEW = REPO / "docs-src/api/c"

# Reuse the extractor's body isolation so OLD text excludes the same boilerplate.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from extract import BodyExtractor, preprocess_html  # noqa: E402

WORD = re.compile(r"[A-Za-z_][A-Za-z0-9_]+")
TAG = re.compile(r"<[^>]+>")


def old_visible_text(path):
    ex = BodyExtractor()
    ex.feed(path.read_text(encoding="utf-8", errors="replace"))
    inner = preprocess_html(ex.inner_html())
    # Drop code (compared separately) and link title="..." attrs (duplicate the
    # link text; not visible prose) so OLD prose matches what NEW prose counts.
    inner = re.sub(r"<pre\b[^>]*>.*?</pre>", " ", inner, flags=re.S)
    inner = re.sub(r'\s+title="[^"]*"', " ", inner)
    text = TAG.sub(" ", inner)
    return htmllib.unescape(text)


def old_code_count(path):
    return path.read_text(errors="replace").count('class="programlisting"')


def old_h4_count(path):
    return len(re.findall(r'<h4 class="title"', path.read_text(errors="replace")))


def strip_front_matter(md):
    if md.startswith("---"):
        end = md.find("\n---", 3)
        if end != -1:
            return md[end + 4:]
    return md


def new_visible_text(md):
    md = strip_front_matter(md)
    md = re.sub(r"```.*?```", " ", md, flags=re.S)   # drop code (compared separately)
    md = re.sub(r'\s+title="[^"]*"', " ", md)         # drop link title= (dup text)
    md = TAG.sub(" ", md)                             # drop residual raw HTML tags
    return htmllib.unescape(md)


def new_code_count(md):
    # Fences may be indented (inside a list). Count opening fences only.
    fences = re.findall(r"^ *```", strip_front_matter(md), flags=re.M)
    return len(fences) // 2


def new_h4_count(md):
    return len(re.findall(r"^#### ", strip_front_matter(md), flags=re.M))


def words(text):
    return Counter(w.lower() for w in WORD.findall(text))


def retention(old_c, new_c):
    total = sum(old_c.values())
    if total == 0:
        return 1.0
    kept = sum(min(new_c[w], c) for w, c in old_c.items())
    return kept / total


def main():
    thr = 0.97
    if "--threshold" in sys.argv:
        thr = float(sys.argv[sys.argv.index("--threshold") + 1])

    files = sorted(OLD.glob("*.html"))
    outliers = []
    hard_drops = []
    ret_sum = 0.0
    n = 0
    for hp in files:
        mp = NEW / f"{hp.stem}.md"
        if not mp.exists():
            hard_drops.append((hp.stem, "MISSING .md"))
            continue
        md = mp.read_text(encoding="utf-8")
        oc, nc = words(old_visible_text(hp)), words(new_visible_text(md))
        r = retention(oc, nc)
        ret_sum += r
        n += 1
        ocode, ncode = old_code_count(hp), new_code_count(md)
        oh4, nh4 = old_h4_count(hp), new_h4_count(md)
        reasons = []
        if r < thr:
            reasons.append(f"words {r:.1%}")
        if ncode < ocode:
            reasons.append(f"code {ocode}->{ncode}")
            hard_drops.append((hp.stem, f"code blocks {ocode}->{ncode}"))
        if nh4 < oh4:
            reasons.append(f"sub-sections {oh4}->{nh4}")
            hard_drops.append((hp.stem, f"sub-sections {oh4}->{nh4}"))
        if reasons:
            outliers.append((hp.stem, r, "; ".join(reasons)))

    print(f"pages compared: {n}")
    print(f"mean word retention: {ret_sum / n:.2%}" if n else "no pages")
    print(f"outliers (< {thr:.0%} words OR structural drop): {len(outliers)}")
    for stem, r, why in sorted(outliers, key=lambda t: t[1]):
        print(f"  {stem:32s} {r:6.1%}  {why}")
    if hard_drops:
        print(f"\nHARD DROPS (code/sub-section/missing): {len(hard_drops)}")
        for stem, why in hard_drops:
            print(f"  {stem:32s} {why}")
        sys.exit(1)
    print("\nno hard drops: every code block and parameter/error sub-section retained.")


if __name__ == "__main__":
    main()
