#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] register library explicit factory contract"

python3 - "$ROOT_DIR" <<'PY'
from pathlib import Path
import sys

root = Path(sys.argv[1])
needle = 'TSSLFactory.RegisterLibrary('
violations = []

for rel in sorted(list(Path('src').rglob('*.pas')) + list(Path('tests').rglob('*.pas'))):
    if rel.name.endswith('.inc'):
        continue
    text = (root / rel).read_text()
    start = 0
    while True:
        idx = text.find(needle, start)
        if idx < 0:
            break
        depth = 1
        comma_count = 0
        i = idx + len(needle)
        in_string = False
        while i < len(text):
            ch = text[i]
            if in_string:
                if ch == "'":
                    if i + 1 < len(text) and text[i + 1] == "'":
                        i += 2
                        continue
                    in_string = False
                i += 1
                continue
            if ch == "'":
                in_string = True
                i += 1
                continue
            if ch == '(':
                depth += 1
            elif ch == ')':
                depth -= 1
                if depth == 0:
                    break
            elif ch == ',' and depth == 1:
                comma_count += 1
            i += 1

        prefix = text[max(0, idx - 32):idx]
        if 'class procedure ' not in prefix and comma_count < 4:
            line = text.count('\n', 0, idx) + 1
            violations.append(f"{rel}:{line}")
        start = i + 1

if violations:
    print('[INFO] class-only RegisterLibrary call(s) still found:')
    for item in violations:
        print(f'  - {item}')
    sys.exit(1)

print('[PASS] all repo-local RegisterLibrary calls pass explicit factory callbacks')
PY
