#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_macos_probe_missing_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_cross_summary_macos_probe_missing"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"
PROBE_REL="$WORK_REL/probe_missing.json"

mkdir -p "$WORK_DIR"
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

cat > "$WORK_DIR/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
- generated_at: fake
- linux_summary: $WORK_REL/linux_summary.md
- linux_examples_json: $WORK_REL/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/linux_summary.md |
| macos | PENDING | probe: $PROBE_REL (missing file) |
| windows | PENDING | no evidence |
EOF

cat > "$WORK_DIR/closure.md" <<EOF
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **IN_PROGRESS**
- strict_mode: false

## Platform Status

| platform | state | note | summary |
|----------|-------|------|---------|
| linux | PASS | ok | $WORK_REL/linux_summary.md |
| macos | PENDING | no evidence | |
| windows | PENDING | no evidence | |
EOF

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure.md" \
  --strict \
  --output "$OUTPUT_REL" >/dev/null 2>&1
exit_code=$?
set -e

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected consistency report to be generated"
fi

if [[ "$exit_code" -eq 0 ]]; then
  fail "consistency should fail strict mode when cross summary declares an active macOS probe path but the probe artifact is missing"
fi

if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should become INCONSISTENT when the active macOS probe path from cross summary is missing"
fi

if ! rg -n "^\\| macos_probe \\| $PROBE_REL \\| NO \\| n/a \\| n/a \\| missing \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should inherit the active macOS probe path from cross summary and mark it missing"
fi

echo "[PASS] wave-b-b2 consistency cross summary macOS probe missing contract passed"
