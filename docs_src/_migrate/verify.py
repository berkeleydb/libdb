#!/usr/bin/env python3
"""Completeness / no-loss gate for the reverse-DocBook migration.

For each old page docs/api_reference/C/<x>.html and its new
docs_src/api/c/<x>.md, compare CONTENT so we can prove nothing was dropped:

  1. Visible-word multiset retention: intersection(new, old) / |old|, over the
     visible text of each (boilerplate navheader/navfooter excluded from OLD by
     reusing the same content-div isolation the extractor uses; front-matter and
     link/HTML noise excluded from NEW). A page below --threshold is flagged.
  2. Code blocks: count of `programlisting` <pre> in OLD vs ``` fences in NEW.
  3. Parameter/Error/sub-section headers: count of <h4 class="title"> in OLD vs
     `####` headings in NEW.

Any drop in (2) or (3), or retention below threshold in (1), is an OUTLIER.

Usage:  verify.py [OLD_HTML_DIR] [NEW_MD_DIR] [--threshold 0.97]
Defaults: docs/api_reference/C  vs  docs_src/api/c
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


def _positionals(argv):
    """argv without --flags and their values (only --threshold takes a value)."""
    out, skip = [], False
    for a in argv:
        if skip:
            skip = False
            continue
        if a == "--threshold":
            skip = True
            continue
        if a.startswith("--"):
            continue
        out.append(a)
    return out


_pos = _positionals(sys.argv[1:])
OLD = Path(_pos[0]) if len(_pos) > 0 else REPO / "docs/api_reference/C"
NEW = Path(_pos[1]) if len(_pos) > 1 else REPO / "docs_src/api/c"

# Reuse the extractor's body isolation so OLD text excludes the same boilerplate.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from extract import BodyExtractor, preprocess_html  # noqa: E402

WORD = re.compile(r"[A-Za-z_][A-Za-z0-9_]+")
# Only match genuine HTML tags (`<a ...>`, `</h2>`, `<br/>`, `<!-- -->`). A bare
# `<` in prose — e.g. gfm's `operator\<=` — is NOT a tag and must survive, else
# a stray `<` swallows all text up to the next `>` and tanks the word count.
TAG = re.compile(r"</?[a-zA-Z][^>]*>|<!--.*?-->", re.S)


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
    # gfm escapes literal punctuation as `\<`, `\>`, `\_`, `\*`, ...
    # Two cases:  `\<` / `\>` are literal angle brackets (template/comparison
    # syntax) — if they reach the TAG regex they open a phantom tag and eat the
    # words after them (`pair\<Key_type, ...`), so blank them.  Every other
    # escape (`\_`, `\*`, ...) is punctuation *inside* a word (`\_DB_STL_value`)
    # — drop only the backslash so the word survives intact.
    md = re.sub(r"\\[<>]", " ", md)
    md = re.sub(r"\\([*_`~\[\](){}#+\-.!|\\])", r"\1", md)
    md = TAG.sub(" ", md)                             # drop residual raw HTML tags
    return htmllib.unescape(md)


def new_code_count(md):
    # Fences may be indented (inside a list) or follow a list marker
    # (`1.  \`\`\` c`). Count every fence line — open and close — and pair them.
    fences = re.findall(r"^[ \t]*(?:[0-9]+\.|[-*+])?[ \t]*```", strip_front_matter(md), flags=re.M)
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


def _selfcheck():
    """Guard the subtle bits: escaped-punct neutralization + tag-only stripping.
    A bare `\\<` must not swallow following words; `\\_word` must keep the word."""
    t = new_visible_text('---\nx: 1\n---\npair\\<Key_type, ElementRef\\<T\\> \\> and \\_DB_STL_value here')
    w = words(t)
    assert w["key_type"] == 1, w
    assert w["elementref"] == 1, w
    assert w["_db_stl_value"] == 1, w
    # a real residual tag is still stripped; its text content survives
    assert "span" not in words(new_visible_text('a <span id="z">body</span> c'))
    assert words(new_visible_text('a <span id="z">body</span> c'))["body"] == 1
    print("selfcheck ok")


if __name__ == "__main__":
    if "--selfcheck" in sys.argv:
        _selfcheck()
    else:
        main()
