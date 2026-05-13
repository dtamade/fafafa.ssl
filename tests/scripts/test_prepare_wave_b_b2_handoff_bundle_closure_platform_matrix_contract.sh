#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
RUN_ID="handoff_closure_platform_matrix_truth"
REPORT_ABS="$WORK_DIR/out/wave_b_b2_handoff_bundle_${RUN_ID}.md"

mkdir -p "$FAKE_SCRIPTS" "$WORK_DIR/out"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" "$FAKE_SCRIPTS/prepare_wave_b_b2_handoff_bundle.sh"

cat > "$FAKE_SCRIPTS/generate_wave_b_cross_platform_summary.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
OUTPUT=""
RUN_ID=""
LINUX_SUMMARY=""
LINUX_EXAMPLES=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output) OUTPUT="$2"; shift 2 ;;
    --run-id) RUN_ID="$2"; shift 2 ;;
    --linux-summary) LINUX_SUMMARY="$2"; shift 2 ;;
    --linux-examples) LINUX_EXAMPLES="$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "$(dirname "$OUTPUT")"
cat > "$OUTPUT" <<OUT
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
- linux_summary: $LINUX_SUMMARY
- linux_examples_json: $LINUX_EXAMPLES
OUT
EOF

cat > "$FAKE_SCRIPTS/check_wave_b_b2_closure_readiness.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
OUTPUT=""
RUN_ID=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output) OUTPUT="$2"; shift 2 ;;
    --run-id) RUN_ID="$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "$(dirname "$OUTPUT")"
cat > "$OUTPUT" <<OUT
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **CLOSED**
- strict_mode: false

## Platform Status

| platform | state | note | summary |
|----------|-------|------|---------|
| linux | PASS | fake | linux.md |
| macos | PASS | fake | macos.md |
OUT
EOF

cat > "$FAKE_SCRIPTS/check_wave_b_b2_evidence_consistency.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
OUTPUT=""
RUN_ID=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output) OUTPUT="$2"; shift 2 ;;
    --run-id) RUN_ID="$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "$(dirname "$OUTPUT")"
cat > "$OUTPUT" <<OUT
# Wave B / B2 Evidence Consistency

- run_id: $RUN_ID
- generated_at: fake
- consistency_status: **CONSISTENT**
- strict_mode: false
- required_missing: 0
- runid_mismatch_or_parse_issue: 0
- closure_status_note: CLOSED
OUT
EOF

chmod +x "$FAKE_SCRIPTS/"*.sh

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

(cd "$FAKE_ROOT" && bash "$FAKE_SCRIPTS/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_DIR/linux_summary.md" \
  --linux-examples "$WORK_DIR/examples.json" \
  --output-dir "$WORK_DIR/out" >/dev/null)

if [[ ! -f "$REPORT_ABS" ]]; then
  fail "expected handoff bundle report"
fi

if ! rg -n "^- handoff_state: \\*\\*NEEDS_REPORT_REPAIR\\*\\*$" "$REPORT_ABS" >/dev/null; then
  fail "handoff bundle should reject a closure report whose platform status table is incomplete"
fi

if ! rg -n "report_chain_note: .*windows platform state missing" "$REPORT_ABS" >/dev/null; then
  fail "handoff bundle should record which closure platform state is missing"
fi

if rg -n "^- handoff_state: \\*\\*CLOSED\\*\\*$" "$REPORT_ABS" >/dev/null; then
  fail "handoff bundle must not report CLOSED when the closure platform matrix is malformed"
fi

echo "[PASS] prepare_wave_b_b2 handoff bundle closure platform matrix contract passed"
