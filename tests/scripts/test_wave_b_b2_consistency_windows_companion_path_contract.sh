#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_windows_companion_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_windows_companion"
WINDOWS_DIR="$WORK_DIR/windows_evidence"
WINDOWS_SUMMARY_REL="$WORK_REL/windows_evidence/wave_b_windows_gate_summary_${RUN_ID}.md"
WINDOWS_QUICK_REL="$WORK_REL/windows_evidence/winssl_quick_smoke_${RUN_ID}.log"
WINDOWS_RUNTIME_REL="$WORK_REL/windows_evidence/winssl_runtime_suite_${RUN_ID}.log"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"

mkdir -p "$WORK_DIR" "$WINDOWS_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
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

cat > "$ROOT_DIR/$WINDOWS_SUMMARY_REL" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

printf 'quick ok\n' > "$ROOT_DIR/$WINDOWS_QUICK_REL"
printf 'suite ok\n' > "$ROOT_DIR/$WINDOWS_RUNTIME_REL"

cat > "$WORK_DIR/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
- linux_summary: $WORK_REL/linux_summary.md
- linux_examples_json: $WORK_REL/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/linux_summary.md |
| macos | PENDING | no evidence |
| windows | PASS | summary: $WINDOWS_SUMMARY_REL (overall=PASS) |
EOF

cat > "$WORK_DIR/closure.md" <<EOF
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **CLOSED**
- strict_mode: false

## Platform Status

| platform | state | note | summary |
|----------|-------|------|---------|
| linux | PASS | ok | $WORK_REL/linux_summary.md |
| macos | PENDING | no evidence | |
| windows | PASS | ok | $WINDOWS_SUMMARY_REL |
EOF

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --windows-summary "$WINDOWS_SUMMARY_REL" \
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
  fail "custom windows summary sibling runtime artifacts should keep strict consistency green"
fi

if ! rg -n "^\\| windows_quick_log \\| $WINDOWS_QUICK_REL \\| YES \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should derive windows_quick_log from the custom windows summary directory"
fi

if ! rg -n "^\\| windows_runtime_transcript \\| $WINDOWS_RUNTIME_REL \\| YES \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should derive windows_runtime_transcript from the custom windows summary directory"
fi

if ! rg -n "consistency_status: \\*\\*CONSISTENT\\*\\*" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should stay CONSISTENT when sibling Windows runtime artifacts exist"
fi

echo "[PASS] wave-b-b2 consistency windows companion path contract passed"
