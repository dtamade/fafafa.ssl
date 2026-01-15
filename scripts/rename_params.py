#!/usr/bin/env python3
"""
Parameter naming convention migration script.
Converts lowercase aXxx parameters to uppercase AXxx.
"""

import re
import sys
import os
from pathlib import Path

# Parameters to rename (lowercase -> uppercase)
# Only rename parameters that start with lowercase 'a' followed by uppercase letter
PARAM_PATTERN = re.compile(r'\b(a)([A-Z][A-Za-z0-9]*)\b')

def rename_params_in_line(line):
    """Rename parameters in a single line."""
    def replacer(match):
        return 'A' + match.group(2)
    return PARAM_PATTERN.sub(replacer, line)

def process_file(filepath):
    """Process a single Pascal file."""
    with open(filepath, 'r', encoding='utf-8') as f:
        original_content = f.read()

    lines = original_content.split('\n')
    new_lines = []
    changes = []

    for i, line in enumerate(lines, 1):
        new_line = rename_params_in_line(line)
        if new_line != line:
            changes.append((i, line.strip(), new_line.strip()))
        new_lines.append(new_line)

    new_content = '\n'.join(new_lines)

    return original_content, new_content, changes

def preview_changes(filepath):
    """Preview changes without applying them."""
    original, new_content, changes = process_file(filepath)

    if not changes:
        print(f"No changes needed in {filepath}")
        return False

    print(f"\n=== {filepath} ({len(changes)} changes) ===")
    for line_num, old, new in changes[:10]:  # Show first 10 changes
        print(f"  Line {line_num}:")
        print(f"    - {old}")
        print(f"    + {new}")

    if len(changes) > 10:
        print(f"  ... and {len(changes) - 10} more changes")

    return True

def apply_changes(filepath, backup=True):
    """Apply changes to a file."""
    original, new_content, changes = process_file(filepath)

    if not changes:
        return 0

    if backup:
        backup_path = filepath + '.bak'
        with open(backup_path, 'w', encoding='utf-8') as f:
            f.write(original)

    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(new_content)

    return len(changes)

def main():
    if len(sys.argv) < 2:
        print("Usage: python rename_params.py [--preview|--apply] [files...]")
        print("\nOptions:")
        print("  --preview  Show changes without applying")
        print("  --apply    Apply changes (creates .bak backups)")
        sys.exit(1)

    mode = sys.argv[1]
    files = sys.argv[2:] if len(sys.argv) > 2 else []

    # Default files if none specified
    if not files:
        src_dir = Path(__file__).parent.parent / 'src'
        files = list(src_dir.glob('fafafa.ssl*.pas'))

    if mode == '--preview':
        print("Preview mode - no changes will be made\n")
        has_changes = False
        for f in files:
            if preview_changes(str(f)):
                has_changes = True
        if not has_changes:
            print("No changes needed in any files.")

    elif mode == '--apply':
        print("Applying changes...\n")
        total_changes = 0
        for f in files:
            changes = apply_changes(str(f))
            if changes > 0:
                print(f"  {f}: {changes} parameters renamed")
                total_changes += changes
        print(f"\nTotal: {total_changes} parameters renamed")

    else:
        print(f"Unknown mode: {mode}")
        sys.exit(1)

if __name__ == '__main__':
    main()
