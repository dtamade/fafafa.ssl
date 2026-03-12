#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh"
WORK_REL="tmp/test_wave_b_cross_dryrun_examples_observability_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_cross_dryrun_$$"
OVERRIDE_REL="$REPORTS_REL/custom_examples_override.json"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave-b cross-platform summary dry-run examples observability contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | ok |
| run_all_module_tests | **PASS** | ok |
| verify_examples_compile | **PASS** | ok |
EOF_SUMMARY

cat > "$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json" <<EOF_JSON
{
  "run_id": "$RUN_ID",
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
EOF_JSON

OUT_DEFAULT="$(FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" bash "$SCRIPT" --run-id "$RUN_ID" --dry-run 2>&1)"

[[ "$OUT_DEFAULT" == *"[DRY-RUN] linux_examples_json=$REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json"* ]] || fail "dry-run should expose default linux examples path"
[[ "$OUT_DEFAULT" == *"[DRY-RUN] linux_examples_selection=run_scoped_exact"* ]] || fail "dry-run should expose default linux examples selection"
[[ "$OUT_DEFAULT" == *"[DRY-RUN] linux_examples_warning=none"* ]] || fail "dry-run should expose default linux examples warning"

cat > "$ROOT_DIR/$OVERRIDE_REL" <<EOF_JSON
{
  "run_id": "$RUN_ID",
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
EOF_JSON

OUT_OVERRIDE="$(FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" FAFAFA_WAVE_B_EXAMPLES_REPORT_REL="$OVERRIDE_REL" bash "$SCRIPT" --run-id "$RUN_ID" --dry-run 2>&1)"

[[ "$OUT_OVERRIDE" == *"[DRY-RUN] linux_examples_json=$OVERRIDE_REL"* ]] || fail "dry-run should expose explicit override linux examples path"
[[ "$OUT_OVERRIDE" == *"[DRY-RUN] linux_examples_selection=explicit_override"* ]] || fail "dry-run should expose explicit override selection"
[[ "$OUT_OVERRIDE" == *"[DRY-RUN] linux_examples_warning=explicit override in use; verify owner run_id/path manually"* ]] || fail "dry-run should expose explicit override warning"

echo "[PASS] wave-b cross-platform summary dry-run examples observability contract passed"
