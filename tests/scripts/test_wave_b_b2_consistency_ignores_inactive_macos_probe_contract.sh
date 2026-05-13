#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_ignores_inactive_macos_probe_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_ignore_inactive_macos_probe"
PROBE_REL="test-reports/wave_b_macos_gate_probe_${RUN_ID}.json"
PROBE_ABS="$ROOT_DIR/$PROBE_REL"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"

mkdir -p "$WORK_DIR" "$(dirname "$PROBE_ABS")"
trap 'rm -rf "$WORK_DIR"; rm -f "$PROBE_ABS"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | ok |
| run_all_module_tests | **PASS** | ok |
| verify_examples_compile | **PASS** | ok |
EOF

cat > "$WORK_DIR/examples.json" <<'EOF'
{
  "summary": {
    "total": 75,
    "passed": 75,
    "failed": 0,
    "skipped": 0,
    "pass_rate": "100%"
  }
}
EOF

cat > "$WORK_DIR/macos_summary.md" <<EOF
# Wave B macOS Gate Summary

- run_id: $RUN_ID
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | ok |
| modules | **PASS** | ok |
| examples | **PASS** | ok |
EOF

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --output "$WORK_REL/cross_summary.md" >/dev/null

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --output "$WORK_REL/closure.md" >/dev/null

printf '{ bad json\n' > "$PROBE_ABS"

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure.md" \
  --strict \
  --output "$OUTPUT_REL" >/dev/null 2>&1
exit_code=$?
set -e

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected consistency report to be generated"
fi

if [[ "$exit_code" -ne 0 ]]; then
  fail "inactive macOS probe should not break strict consistency when macOS summary is authoritative"
fi

if rg -n "^\\| macos_probe \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should ignore inactive macOS probe artifacts when cross summary uses macOS summary"
fi

if ! rg -n "consistency_status: \\*\\*CONSISTENT\\*\\*" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should stay CONSISTENT when only an inactive stale macOS probe is malformed"
fi

echo "[PASS] wave-b-b2 consistency ignores inactive macOS probe contract passed"
