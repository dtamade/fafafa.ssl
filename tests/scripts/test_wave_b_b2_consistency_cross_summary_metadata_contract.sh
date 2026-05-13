#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_metadata_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_cross_summary_metadata_truth"
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
- generated_at: fake
- linux_summary: $WORK_REL/linux_summary.md

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/linux_summary.md |
| macos | PENDING | no evidence |
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
  fail "consistency should fail strict mode when cross summary is missing required linux_examples_json metadata even if the actual linux examples artifact still exists"
fi

if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should become INCONSISTENT when cross summary metadata is malformed"
fi

if ! rg -n "^- runid_mismatch_or_parse_issue: 1$" "$OUTPUT_ABS" >/dev/null; then
  fail "missing required cross summary metadata should count as one parse issue"
fi

if ! rg -n "^\\| linux_examples_json \\| $WORK_REL/examples\\.json \\| YES \\| n/a \\| n/a \\| json_valid=YES \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "fixture should keep the actual linux examples artifact valid so the failure isolates malformed cross summary metadata"
fi

if ! rg -n "^\\| cross_summary \\| $WORK_REL/cross_summary\\.md \\| YES \\| $RUN_ID \\| YES \\| linux_examples_json missing \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should surface the missing cross summary metadata on the cross_summary row"
fi

echo "[PASS] wave-b-b2 consistency cross summary metadata contract passed"
