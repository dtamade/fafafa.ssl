#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh"
WORK_REL="tmp/test_wave_b_b2_evidence_windows_blocker_linkage"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,260p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b b2 evidence consistency windows blocker linkage contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

RUN_ID="wb2_blocker_linkage_$$"
BLOCKER_REL="$WORK_REL/winssl_blocker_batch_${RUN_ID}.md"
OUTPUT_FAIL_REL="$WORK_REL/consistency_fail.md"
OUTPUT_PASS_REL="$WORK_REL/consistency_pass.md"
OUTPUT_SKIP_REL="$WORK_REL/consistency_skip.md"

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF

cat > "$WORK_DIR/examples.json" <<'EOF'
{
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
EOF

cat > "$WORK_DIR/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
EOF

cat > "$WORK_DIR/closure_report.md" <<EOF
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **IN_PROGRESS**
EOF

cat > "$WORK_DIR/windows_summary_pass.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS

## Steps

| step | exit | status | evidence |
|------|------|--------|----------|
| winssl_blocker_batch | 0 | PASS | $BLOCKER_REL |
| winssl | 0 | PASS | test-reports/winssl_${RUN_ID}.log |
| openssl | 0 | PASS | test-reports/openssl_${RUN_ID}.log |
| modules | 0 | PASS | test-reports/modules_${RUN_ID}.log |
EOF

# Case 1: PASS + referenced blocker report missing => strict must fail
set +e
OUT_FAIL="$(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --windows-summary "$WORK_REL/windows_summary_pass.md" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure_report.md" \
  --output "$OUTPUT_FAIL_REL" \
  --strict 2>&1)"
EC_FAIL=$?
set -e

if [[ $EC_FAIL -eq 0 ]]; then
  echo "$OUT_FAIL"
  fail "strict mode should fail when windows blocker report is referenced but missing"
fi

assert_contains "$ROOT_DIR/$OUTPUT_FAIL_REL" "| windows_blocker_batch_report | $BLOCKER_REL | NO | n/a | NO | missing |"

# Case 2: PASS + referenced blocker report exists => strict must pass
cat > "$ROOT_DIR/$BLOCKER_REL" <<EOF
# Windows WinSSL Blocker Batch (Draft)

- run_id: $RUN_ID
- overall: **PASS**
EOF

cd "$ROOT_DIR"
bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --windows-summary "$WORK_REL/windows_summary_pass.md" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure_report.md" \
  --output "$OUTPUT_PASS_REL" \
  --strict >/dev/null

assert_contains "$ROOT_DIR/$OUTPUT_PASS_REL" "| windows_blocker_batch_report | $BLOCKER_REL | YES | $RUN_ID | YES | ok |"
assert_contains "$ROOT_DIR/$OUTPUT_PASS_REL" "- consistency_status: **CONSISTENT**"

# Case 3: SKIPPED + <none> => blocker report not required
cat > "$WORK_DIR/windows_summary_skip.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS

## Steps

| step | exit | status | evidence |
|------|------|--------|----------|
| winssl_blocker_batch | SKIP | SKIPPED | <none> |
| winssl | 0 | PASS | test-reports/winssl_${RUN_ID}.log |
| openssl | 0 | PASS | test-reports/openssl_${RUN_ID}.log |
| modules | 0 | PASS | test-reports/modules_${RUN_ID}.log |
EOF

bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --windows-summary "$WORK_REL/windows_summary_skip.md" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure_report.md" \
  --output "$OUTPUT_SKIP_REL" \
  --strict >/dev/null

assert_contains "$ROOT_DIR/$OUTPUT_SKIP_REL" "| windows_blocker_batch_report | <none> | NO | n/a | NO | blocker skipped in windows summary |"
assert_contains "$ROOT_DIR/$OUTPUT_SKIP_REL" "- consistency_status: **CONSISTENT**"

echo "[PASS] wave-b b2 evidence consistency windows blocker linkage contract passed"
