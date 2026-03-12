#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

python3 - <<'PY'
from pathlib import Path
import re
import sys
files = [Path('tests/test_all_modules_comprehensive.pas')]
block_comment_re = re.compile(r'\{.*?\}|\(\*.*?\*\)', re.S)
line_comment_re = re.compile(r'//.*?$', re.M)
pat = re.compile(r"AddModule\('([^']+)'\s*,\s*'([^']+)'\s*,\s*'([^']+)'\s*,\s*(\d+)\)")
missing = {}
for path in files:
    text = path.read_text(errors='ignore')
    text = block_comment_re.sub(' ', text)
    text = line_comment_re.sub('', text)
    for name, unit, category, priority in pat.findall(text):
        if unit.startswith('fafafa.ssl.openssl.api.'):
            target = Path('src') / f'{unit}.pas'
            if not target.exists():
                missing.setdefault(unit, []).append(f'{path}:{name}')
if missing:
    for unit, refs in sorted(missing.items()):
        print(unit)
        for ref in refs:
            print(f'  {ref}')
    sys.exit(1)
print('[PASS] active module inventory points at real source units')
PY
