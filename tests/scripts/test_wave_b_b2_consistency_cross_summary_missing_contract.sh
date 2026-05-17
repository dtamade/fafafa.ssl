#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_missing_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_cross_summary_missing_truth"
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
  fail "expected consistency report when cross summary is missing"
fi

if [[ "$exit_code" -eq 0 ]]; then
  fail "strict consistency should reject missing cross summary"
fi

if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency should become INCONSISTENT when cross summary is missing"
fi

if ! rg -n "^- required_missing: 1$" "$OUTPUT_ABS" >/dev/null; then
  fail "missing cross summary should count as one required missing artifact"
fi

if ! rg -n "^- runid_mismatch_or_parse_issue: 0$" "$OUTPUT_ABS" >/dev/null; then
  fail "missing cross summary should stay a required-missing issue instead of a parse mismatch"
fi

if ! rg -n "^- closure_status_note: IN_PROGRESS$" "$OUTPUT_ABS" >/dev/null; then
  fail "closure status should stay tied to the valid closure report when only the cross summary is missing"
fi

if ! rg -n "^\\| cross_summary \\| $WORK_REL/cross_summary\\.md \\| NO \\| n/a \\| NO \\| missing \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "cross_summary row should expose the missing report file"
fi

if ! rg -n "closure_status_note=IN_PROGRESS" "$OUTPUT_ABS" >/dev/null; then
  fail "next actions should preserve the in-progress closure guidance when only cross summary evidence is missing"
fi

if ! rg -n "回填/修复相关 evidence 后，复跑 Wave B/B2 handoff bundle 准备流程" "$OUTPUT_ABS" >/dev/null; then
  fail "next actions should still direct operators to backfill evidence and rerun the handoff bundle preparation"
fi

echo "[PASS] wave-b-b2 consistency cross_summary missing contract passed"
