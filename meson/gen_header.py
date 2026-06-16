#!/usr/bin/env python3
"""Generate a Berkeley DB header the way dist/configure does: concatenate one
or more template (.in) files and apply @VAR@ substitutions.

Usage: gen_header.py <out.h> <subs.json> <in1> [in2 ...]
"""
import json
import re
import sys


def main():
    out, subs_path = sys.argv[1], sys.argv[2]
    inputs = sys.argv[3:]
    subs = json.load(open(subs_path))
    text = "".join(open(f, encoding="utf-8").read() for f in inputs)
    # Replace known @VAR@; leave unknown tokens untouched so they're visible.
    text = re.sub(r"@([A-Za-z_0-9]+)@",
                  lambda m: subs.get(m.group(1), m.group(0)), text)
    with open(out, "w", encoding="utf-8") as fh:
        fh.write(text)


if __name__ == "__main__":
    main()
