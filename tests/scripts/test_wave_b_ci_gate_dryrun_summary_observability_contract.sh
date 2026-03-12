#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

RUN_ID="wave_b_ci_gate_dryrun_summary_$$"
REPORTS_REL="tmp/test_wave_b_ci_gate_dryrun_summary"

OUT="$(FAFAFA_WAVE_B_CI_GATE_RUN_ID="$RUN_ID" bash scripts/run_wave_b_ci_gate.sh --dry-run --skip-compile --skip-modules --reports-dir "$REPORTS_REL" 2>&1)"

[[ "$OUT" == *"[DRY-RUN] run_id=$RUN_ID"* ]] || {
  echo "[FAIL] dry-run should expose run_id"
  printf '%s\n' "$OUT"
  exit 1
}

[[ "$OUT" == *"[DRY-RUN] summary_out=$REPORTS_REL/wave_b_ci_gate_summary_${RUN_ID}.md"* ]] || {
  echo "[FAIL] dry-run should expose summary output path"
  printf '%s\n' "$OUT"
  exit 1
}

echo "[PASS] wave b ci gate dry-run summary observability contract passed"
