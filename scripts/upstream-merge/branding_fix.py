#!/usr/bin/env python3
"""
In-place branding fixer for upstream merges.

Replaces the brand name 'Codex' -> 'Code' only within single-line quoted
string literals (", ', `). Non-quoted occurrences are left untouched.

Usage:
  python3 scripts/upstream-merge/branding_fix.py <file> [<file> ...]
"""
from __future__ import annotations
import io
import os
import re
import sys

# Matches a single-line quoted string: double, single, or backtick, with escapes.
Q = re.compile(r'("(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\'|`(?:[^`\\]|\\.)*`)')

def fix_text(text: str) -> str:
    def repl(m: re.Match[str]) -> str:
        s = m.group(0)
        return s.replace("Codex", "Code")
    return Q.sub(repl, text)

def process_file(path: str) -> bool:
    try:
        with io.open(path, 'r', encoding='utf-8') as f:
            original = f.read()
    except (OSError, UnicodeDecodeError):
        return False
    updated = fix_text(original)
    if updated != original:
        with io.open(path, 'w', encoding='utf-8', newline='') as f:
            f.write(updated)
        return True
    return False

def main(argv: list[str]) -> int:
    if len(argv) < 2:
        print("usage: branding_fix.py <file> [<file> ...]", file=sys.stderr)
        return 2
    changed = False
    for p in argv[1:]:
        if os.path.isfile(p):
            if process_file(p):
                changed = True
    return 0 if changed else 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))

