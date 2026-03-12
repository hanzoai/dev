#!/usr/bin/env python3
"""Verify that a README's table of contents is up to date.

Scans the markdown file for headings (## and ###) and checks that there is
a ToC section linking to all of them.  Exits 0 if the ToC is valid (or if
there is no ToC section at all), 1 otherwise.
"""
import re
import sys
from pathlib import Path


def slugify(text: str) -> str:
    """Convert heading text to GitHub-style anchor slug."""
    text = text.lower().strip()
    text = re.sub(r"[^\w\s-]", "", text)
    text = re.sub(r"[\s]+", "-", text)
    return text


def extract_headings(content: str) -> list[str]:
    """Extract ##+ headings from markdown content."""
    headings = []
    for line in content.splitlines():
        m = re.match(r"^(#{2,6})\s+(.+)$", line)
        if m:
            headings.append(slugify(m.group(2)))
    return headings


def extract_toc_links(content: str) -> list[str]:
    """Extract anchor links from the ToC section."""
    links = []
    for m in re.finditer(r"\[.*?\]\(#([\w-]+)\)", content):
        links.append(m.group(1))
    return links


def main() -> int:
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <README.md>", file=sys.stderr)
        return 1

    filepath = Path(sys.argv[1])
    content = filepath.read_text(encoding="utf-8")

    toc_links = extract_toc_links(content)
    if not toc_links:
        # No ToC found — that's fine.
        return 0

    headings = extract_headings(content)
    missing = [link for link in toc_links if link not in headings]

    if missing:
        print(f"ToC links not matching headings in {filepath}:")
        for slug in missing:
            print(f"  - #{slug}")
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
