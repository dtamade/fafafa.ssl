#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

OUT="$(bash scripts/run_wave_b_ci_gate.sh --dry-run --skip-compile --skip-modules --reports-dir tmp/test_wave_b_ci_gate_dryrun_observability 2>&1)"

if [[ "$OUT" != *"[DRY-RUN] examples_selection=current_alias"* ]]; then
  echo "[FAIL] dry-run should expose default examples selection"
  printf '%s\n' "$OUT"
  exit 1
fi

if [[ "$OUT" != *"[DRY-RUN] examples_warning=none"* ]]; then
  echo "[FAIL] dry-run should expose default examples warning"
  printf '%s\n' "$OUT"
  exit 1
fi

OUT_OVERRIDE="$(FAFAFA_WAVE_B_EXAMPLES_REPORT_REL=tmp/override_examples.json bash scripts/run_wave_b_ci_gate.sh --dry-run --skip-compile --skip-modules --reports-dir tmp/test_wave_b_ci_gate_dryrun_observability 2>&1)"

if [[ "$OUT_OVERRIDE" != *"[DRY-RUN] examples_selection=explicit_override"* ]]; then
  echo "[FAIL] dry-run should expose explicit override selection"
  printf '%s\n' "$OUT_OVERRIDE"
  exit 1
fi

if [[ "$OUT_OVERRIDE" != *"[DRY-RUN] examples_warning=explicit override in use; verify owner run_id/path manually"* ]]; then
  echo "[FAIL] dry-run should expose explicit override warning"
  printf '%s\n' "$OUT_OVERRIDE"
  exit 1
fi

echo "[PASS] wave b ci gate dry-run examples observability contract passed"
