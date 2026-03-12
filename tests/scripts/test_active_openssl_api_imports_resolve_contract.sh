#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

python3 - <<'PY'
from pathlib import Path
import re
import sys
roots = [Path('src'), Path('tests'), Path('examples')]
unit_re = re.compile(r'\bfafafa\.ssl\.openssl\.api\.[A-Za-z0-9_\.]+\b')
comment_re = re.compile(r'\{.*?\}|\(\*.*?\*\)|//.*?$', re.S | re.M)
missing = {}
for root in roots:
    for path in root.rglob('*.pas'):
        text = path.read_text(errors='ignore')
        text = comment_re.sub(' ', text)
        for m in re.finditer(r'\buses\b(.*?);', text, re.S | re.I):
            block = m.group(1)
            for unit in unit_re.findall(block):
                target = Path('src') / f'{unit}.pas'
                if not target.exists():
                    missing.setdefault(unit, []).append(str(path))
if missing:
    for unit, refs in sorted(missing.items()):
        print(unit)
        for ref in refs:
            print(f'  {ref}')
    sys.exit(1)
print('[PASS] active OpenSSL api imports resolve to source units')
PY
