#!/usr/bin/env python3
import os
import re
import sys
from typing import Set, Tuple


def find_version_references(directory: str) -> Tuple[Set[str], Set[str]]:
    dev_versions = set()
    runtime_versions = set()

    version_pattern_dev = re.compile(r'dev:(\d{1})\.(\d{2})')
    version_pattern_runtime = re.compile(r'runtime:(\d{1})\.(\d{2})')

    for root, _, files in os.walk(directory):
        # Skip .git directory
        if '.git' in root:
            continue

        for file in files:
            if file.endswith(
                ('.md', '.yml', '.yaml', '.txt', '.html', '.py', '.js', '.ts')
            ):
                file_path = os.path.join(root, file)
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        content = f.read()

                        # Find all dev version references
                        matches = version_pattern_dev.findall(content)
                        dev_versions.update(matches)

                        # Find all runtime version references
                        matches = version_pattern_runtime.findall(content)
                        runtime_versions.update(matches)
                except Exception as e:
                    print(f'Error reading {file_path}: {e}', file=sys.stderr)

    return dev_versions, runtime_versions


def main():
    repo_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
    dev_versions, runtime_versions = find_version_references(repo_root)

    exit_code = 0

    if len(dev_versions) > 1:
        print('Error: Multiple dev versions found:', file=sys.stderr)
        print('Found versions:', sorted(dev_versions), file=sys.stderr)
        exit_code = 1
    elif len(dev_versions) == 0:
        print('Warning: No dev version references found', file=sys.stderr)

    if len(runtime_versions) > 1:
        print('Error: Multiple runtime versions found:', file=sys.stderr)
        print('Found versions:', sorted(runtime_versions), file=sys.stderr)
        exit_code = 1
    elif len(runtime_versions) == 0:
        print('Warning: No runtime version references found', file=sys.stderr)

    sys.exit(exit_code)


if __name__ == '__main__':
    main()
