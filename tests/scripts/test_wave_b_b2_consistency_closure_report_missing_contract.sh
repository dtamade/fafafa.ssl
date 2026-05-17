#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_closure_report_missing_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_closure_report_missing_truth"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"

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
- linux_summary: $WORK_REL/linux_summary.md
- linux_examples_json: $WORK_REL/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/linux_summary.md |
| macos | PENDING | no evidence |
| windows | PENDING | no evidence |
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
  fail "expected consistency report when closure report is missing"
fi

if [[ "$exit_code" -eq 0 ]]; then
  fail "strict consistency should reject missing closure report"
fi

if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency should become INCONSISTENT when closure report is missing"
fi

if ! rg -n "^- required_missing: 1$" "$OUTPUT_ABS" >/dev/null; then
  fail "missing closure report should count as one required missing artifact"
fi

if ! rg -n "^- runid_mismatch_or_parse_issue: 0$" "$OUTPUT_ABS" >/dev/null; then
  fail "missing closure report should stay a required-missing issue instead of a parse mismatch"
fi

if ! rg -n "^- closure_status_note: closure_report missing$" "$OUTPUT_ABS" >/dev/null; then
  fail "top-level closure_status_note should surface missing closure report"
fi

if ! rg -n "^\\| closure_report \\| $WORK_REL/closure\\.md \\| NO \\| n/a \\| NO \\| missing \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "closure_report row should expose the missing report file"
fi

if rg -n "当前 closure 已闭环|closure_status_note=IN_PROGRESS" "$OUTPUT_ABS" >/dev/null; then
  fail "next actions should not claim a closed or in-progress closure state when the closure report is missing"
fi

if ! rg -n "当前 evidence consistency 与 closure 元数据至少有一层未对齐" "$OUTPUT_ABS" >/dev/null; then
  fail "next actions should fall back to the generic metadata-misaligned guidance when the closure report is missing"
fi

echo "[PASS] wave-b-b2 consistency closure_report missing contract passed"
