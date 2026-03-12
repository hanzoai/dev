#!/usr/bin/env python3
"""Check that a file contains only ASCII and safe Unicode code points.

Allowed: ASCII printable, tabs, newlines, and common punctuation marks
(em/en dashes, smart quotes, bullets, arrows, etc.)
"""
import sys

ALLOWED_RANGES = [
    (0x0009, 0x000A),  # Tab, newline
    (0x000D, 0x000D),  # Carriage return
    (0x0020, 0x007E),  # ASCII printable
    (0x00A0, 0x00FF),  # Latin-1 Supplement
    (0x2010, 0x2027),  # General punctuation (dashes, quotes, bullets)
    (0x2030, 0x205E),  # More general punctuation
    (0x2190, 0x21FF),  # Arrows
    (0x2500, 0x257F),  # Box drawing
    (0x2580, 0x259F),  # Block elements
    (0x25A0, 0x25FF),  # Geometric shapes
    (0x2600, 0x26FF),  # Miscellaneous symbols
    (0x2700, 0x27BF),  # Dingbats
    (0xFE00, 0xFE0F),  # Variation selectors
    (0x1F300, 0x1F9FF),  # Emoji (miscellaneous symbols, emoticons, etc.)
    (0x2100, 0x214F),  # Letterlike symbols (ℹ, ™, etc.)
]


def is_allowed(ch: str) -> bool:
    cp = ord(ch)
    return any(lo <= cp <= hi for lo, hi in ALLOWED_RANGES)


def main() -> int:
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <file>", file=sys.stderr)
        return 1

    filepath = sys.argv[1]
    errors = 0

    with open(filepath, encoding="utf-8") as f:
        for lineno, line in enumerate(f, 1):
            for col, ch in enumerate(line, 1):
                if not is_allowed(ch):
                    errors += 1
                    if errors <= 20:
                        print(
                            f"{filepath}:{lineno}:{col}: "
                            f"disallowed character U+{ord(ch):04X} ({ch!r})"
                        )

    if errors:
        print(f"\n{errors} disallowed character(s) found in {filepath}")
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
