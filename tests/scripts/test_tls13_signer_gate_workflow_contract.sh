#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOW="$ROOT_DIR/.github/workflows/tls13-signer-gate.yml"

fail() {
  echo "[FAIL] $1"
  exit 1
}

if [[ ! -f "$WORKFLOW" ]]; then
  fail "missing workflow: .github/workflows/tls13-signer-gate.yml"
fi

tmp_root="$(mktemp -d "${TMPDIR:-/tmp}/tls13_signer_workflow_contract.XXXXXX")"
trap 'rm -rf "$tmp_root"' EXIT

summary_script="$tmp_root/append_step_summary.sh"

python3 - "$WORKFLOW" "$summary_script" <<'PY'
import pathlib
import re
import sys

workflow = pathlib.Path(sys.argv[1])
out_path = pathlib.Path(sys.argv[2])
lines = workflow.read_text(encoding="utf-8").splitlines()

step_idx = None
for idx, line in enumerate(lines):
    if line.strip() == "- name: Append step summary":
        step_idx = idx
        break

if step_idx is None:
    raise SystemExit("missing append step summary block")

run_idx = None
for idx in range(step_idx + 1, len(lines)):
    if re.match(r"^\s*run:\s*\|$", lines[idx]):
        run_idx = idx
        break
    if re.match(r"^\s*-\s+name:", lines[idx]):
        raise SystemExit("append step summary block has no run: | section")

if run_idx is None:
    raise SystemExit("append step summary block has no run: | section")

base_indent = None
block = []
for line in lines[run_idx + 1:]:
    if line.strip() == "":
      block.append("")
      continue
    indent = len(line) - len(line.lstrip(" "))
    if base_indent is None:
      base_indent = indent
    if indent < base_indent:
      break
    block.append(line[base_indent:])

if not block:
    raise SystemExit("append step summary block is empty")

out_path.write_text("\n".join(block) + "\n", encoding="utf-8")
PY

if ! bash -n "$summary_script" >/dev/null 2>&1; then
  echo "[INFO] extracted append-step-summary script:" >&2
  cat "$summary_script" >&2
  fail "tls13 signer gate append-step-summary shell must parse cleanly"
fi

echo "[PASS] tls13 signer gate workflow contract passed"
