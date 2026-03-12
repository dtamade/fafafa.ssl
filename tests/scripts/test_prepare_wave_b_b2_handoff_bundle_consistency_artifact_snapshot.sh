#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh"
WORK_REL="tmp/test_wave_b_b2_handoff_consistency_artifact_snapshot"
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
    sed -n '1,420p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b b2 handoff bundle consistency artifact snapshot"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

RUN_ID="wb2_handoff_consistency_$$"
OUT_DIR_REL="$WORK_REL/out"
BLOCKER_REL="$WORK_REL/winssl_blocker_batch_${RUN_ID}.md"
CONSISTENCY_REL="$OUT_DIR_REL/wave_b_b2_evidence_consistency_${RUN_ID}.md"
CLOSURE_REL="$OUT_DIR_REL/wave_b_b2_closure_readiness_${RUN_ID}.md"

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | 157/157 |
| run_all_module_tests | **PASS** | 100% |
| verify_examples_compile | **PASS** | 71/75 |
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

cat > "$WORK_DIR/windows_summary.md" <<EOF
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

cd "$ROOT_DIR"
bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --windows-summary "$WORK_REL/windows_summary.md" \
  --output-dir "$OUT_DIR_REL" >/dev/null

BUNDLE_FILE="$ROOT_DIR/$OUT_DIR_REL/wave_b_b2_handoff_bundle_${RUN_ID}.md"
[[ -f "$BUNDLE_FILE" ]] || fail "handoff bundle should be generated"

assert_contains "$BUNDLE_FILE" "## Consistency Artifact Snapshot"
assert_contains "$BUNDLE_FILE" "- source: $CONSISTENCY_REL"
assert_contains "$BUNDLE_FILE" "| closure_report | $CLOSURE_REL | YES | YES | ok |"
assert_contains "$BUNDLE_FILE" "| windows_blocker_batch_report | $BLOCKER_REL | NO | NO | missing |"

echo "[PASS] wave-b b2 handoff bundle consistency artifact snapshot passed"
