#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh"
WORK_REL="tmp/test_wave_b_handoff_linked_alert_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_BIN_DIR="$WORK_DIR/fakebin"
RUN_ID="wave_b_handoff_linked_alert_$$"
OUT_DIR_REL="$WORK_REL/out"
OUT_DIR="$ROOT_DIR/$OUT_DIR_REL"
CONSISTENCY_REL="$OUT_DIR_REL/wave_b_b2_evidence_consistency_${RUN_ID}.md"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,400p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b handoff linked evidence alert sync contract"

mkdir -p "$FAKE_BIN_DIR" "$WORK_DIR"

cat > "$WORK_DIR/linux_summary.md" <<EOF_LINUX
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF_LINUX

cat > "$WORK_DIR/examples.json" <<EOF_EXAMPLES
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
EOF_EXAMPLES

cat > "$FAKE_BIN_DIR/bash" <<'EOF_FAKE_BASH'
#!/bin/bash
set -euo pipefail
RUN_ID="${FAFAFA_TEST_RUN_ID:?}"
WORK_REL="${FAFAFA_TEST_WORK_REL:?}"
OUT_DIR_REL="${FAFAFA_TEST_OUT_DIR_REL:?}"
script="$1"
shift || true
case "$script" in
  */scripts/check_wave_b_b2_evidence_consistency.sh)
    output=""
    while [[ $# -gt 0 ]]; do
      case "$1" in
        --output)
          output="$2"
          shift 2
          ;;
        *)
          shift
          ;;
      esac
    done
    [[ -n "$output" ]] || exit 2
    mkdir -p "$(dirname "$output")"
    cat > "$output" <<EOF_REPORT
# Wave B / B2 Evidence Consistency

- run_id: $RUN_ID
- generated_at: 2026-03-09 00:00:00 +0000
- consistency_status: **INCONSISTENT**
- strict_mode: false
- required_missing: 0
- runid_mismatch_or_parse_issue: 0
- linked_evidence_mismatch: 1
- closure_status_note: IN_PROGRESS
- linux_examples_selection: run_scoped_exact
- linux_examples_warning: none

## Artifact Matrix

| artifact | path | exists | parsed_run_id | run_id_match | note |
|----------|------|--------|---------------|--------------|------|
| linux_summary | $WORK_REL/linux_summary.md | YES | $RUN_ID | YES | ok |
| linux_examples_json | $WORK_REL/examples.json | YES | $RUN_ID | YES | ok |
| cross_summary | $OUT_DIR_REL/wave_b_cross_platform_summary_${RUN_ID}.md | YES | $RUN_ID | YES | ok; linked linux_examples_json mismatch |
| closure_report | $OUT_DIR_REL/wave_b_b2_closure_readiness_${RUN_ID}.md | YES | $RUN_ID | YES | ok |
EOF_REPORT
    echo "[PASS] evidence consistency report generated: $output"
    ;;
  *)
    exec /bin/bash "$script" "$@"
    ;;
esac
EOF_FAKE_BASH
chmod +x "$FAKE_BIN_DIR/bash"

PATH="$FAKE_BIN_DIR:$PATH" \
FAFAFA_TEST_RUN_ID="$RUN_ID" \
FAFAFA_TEST_WORK_REL="$WORK_REL" \
FAFAFA_TEST_OUT_DIR_REL="$OUT_DIR_REL" \
  bash "$SCRIPT" \
    --run-id "$RUN_ID" \
    --linux-summary "$WORK_REL/linux_summary.md" \
    --linux-examples "$WORK_REL/examples.json" \
    --output-dir "$OUT_DIR_REL" >/dev/null

BUNDLE_FILE="$OUT_DIR/wave_b_b2_handoff_bundle_${RUN_ID}.md"
[[ -f "$BUNDLE_FILE" ]] || fail "handoff bundle should be generated"

assert_contains "$BUNDLE_FILE" "- source: $CONSISTENCY_REL"
assert_contains "$BUNDLE_FILE" "- required_missing: 0"
assert_contains "$BUNDLE_FILE" "- runid_mismatch_or_parse_issue: 0"
assert_contains "$BUNDLE_FILE" "- linked_evidence_mismatch: 1"
assert_contains "$BUNDLE_FILE" "- alert_state: **WARN**"
assert_contains "$BUNDLE_FILE" "- consistency_status: INCONSISTENT"
assert_contains "$BUNDLE_FILE" "| cross_summary | YES | YES | ok; linked linux_examples_json mismatch |"

echo "[PASS] wave-b handoff linked evidence alert sync contract passed"
