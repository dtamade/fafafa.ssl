#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_b_macos_gate.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

RUN_ID="wave_b_macos_dryrun_obs_$$"
OUT="$(
  cd /tmp
  bash "$SCRIPT" --dry-run --run-id "$RUN_ID" --output-dir tmp/test_wave_b_macos_gate_dryrun_obs 2>&1
)"

[[ "$OUT" == *"[DRY-RUN] run_id=$RUN_ID"* ]] || {
  echo "$OUT"
  fail "dry-run should expose run_id"
}

[[ "$OUT" == *"[DRY-RUN] output_dir=tmp/test_wave_b_macos_gate_dryrun_obs"* ]] || {
  echo "$OUT"
  fail "dry-run should expose output_dir"
}

[[ "$OUT" == *"[DRY-RUN] summary=tmp/test_wave_b_macos_gate_dryrun_obs/wave_b_macos_gate_summary_${RUN_ID}.md"* ]] || {
  echo "$OUT"
  fail "dry-run should expose summary path"
}

[[ "$OUT" == *"[DRY-RUN] probe_json=tmp/test_wave_b_macos_gate_dryrun_obs/wave_b_macos_gate_probe_${RUN_ID}.json"* ]] || {
  echo "$OUT"
  fail "dry-run should expose probe json path"
}

[[ "$OUT" == *"[DRY-RUN] examples_json=tmp/test_wave_b_macos_gate_dryrun_obs/examples_compile_gate_macos_${RUN_ID}.json"* ]] || {
  echo "$OUT"
  fail "dry-run should expose examples json path"
}

echo "[PASS] wave_b macos gate dry-run observability contract passed"
