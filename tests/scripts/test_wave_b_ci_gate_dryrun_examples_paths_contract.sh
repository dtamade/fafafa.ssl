#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

OUT="$(bash scripts/run_wave_b_ci_gate.sh --dry-run --skip-compile --skip-modules --reports-dir tmp/test_wave_b_ci_gate_dryrun_paths 2>&1)"

[[ "$OUT" == *"[DRY-RUN] examples_report=tmp/test_wave_b_ci_gate_dryrun_paths/examples_compile_ci_gate.json"* ]] || {
  echo "[FAIL] dry-run should expose default examples report path"
  printf '%s\n' "$OUT"
  exit 1
}
[[ "$OUT" == *"[DRY-RUN] examples_current_alias=tmp/test_wave_b_ci_gate_dryrun_paths/examples_compile_ci_gate.json"* ]] || {
  echo "[FAIL] dry-run should expose current alias path"
  printf '%s\n' "$OUT"
  exit 1
}
[[ "$OUT" == *"[DRY-RUN] examples_run_scoped=tmp/test_wave_b_ci_gate_dryrun_paths/examples_compile_ci_gate_"* ]] || {
  echo "[FAIL] dry-run should expose run-scoped copy path"
  printf '%s\n' "$OUT"
  exit 1
}
[[ "$OUT" == *"[DRY-RUN] examples_archive=tmp/test_wave_b_ci_gate_dryrun_paths/examples-compile-history/examples_compile_ci_gate_"* ]] || {
  echo "[FAIL] dry-run should expose archive copy path"
  printf '%s\n' "$OUT"
  exit 1
}

OUT_OVERRIDE="$(FAFAFA_WAVE_B_EXAMPLES_REPORT_REL=tmp/custom_examples_override.json bash scripts/run_wave_b_ci_gate.sh --dry-run --skip-compile --skip-modules --reports-dir tmp/test_wave_b_ci_gate_dryrun_paths 2>&1)"

[[ "$OUT_OVERRIDE" == *"[DRY-RUN] examples_report=tmp/custom_examples_override.json"* ]] || {
  echo "[FAIL] dry-run should expose explicit examples report path"
  printf '%s\n' "$OUT_OVERRIDE"
  exit 1
}
[[ "$OUT_OVERRIDE" == *"[DRY-RUN] examples_current_alias=tmp/custom_examples_override.json"* ]] || {
  echo "[FAIL] dry-run should expose explicit current alias path"
  printf '%s\n' "$OUT_OVERRIDE"
  exit 1
}
[[ "$OUT_OVERRIDE" == *"[DRY-RUN] examples_run_scoped=tmp/test_wave_b_ci_gate_dryrun_paths/examples_compile_ci_gate_"* ]] || {
  echo "[FAIL] dry-run should keep run-scoped copy path under reports dir"
  printf '%s\n' "$OUT_OVERRIDE"
  exit 1
}
[[ "$OUT_OVERRIDE" == *"[DRY-RUN] examples_archive=tmp/test_wave_b_ci_gate_dryrun_paths/examples-compile-history/examples_compile_ci_gate_"* ]] || {
  echo "[FAIL] dry-run should keep archive copy path under reports dir"
  printf '%s\n' "$OUT_OVERRIDE"
  exit 1
}

echo "[PASS] wave b ci gate dry-run examples paths contract passed"
