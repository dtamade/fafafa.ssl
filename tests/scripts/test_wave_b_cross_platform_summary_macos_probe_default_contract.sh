#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_cross_platform_summary_macos_probe_default_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="cross_summary_macos_probe_default"
PROBE_REL="test-reports/wave_b_macos_gate_probe_${RUN_ID}.json"
PROBE_ABS="$ROOT_DIR/$PROBE_REL"

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

cat > "$PROBE_ABS" <<'EOF'
{
  "status": "error"
}
EOF

(cd /tmp && bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --output "$WORK_DIR/cross_summary.md" >/dev/null)

OUTPUT_FILE="$WORK_DIR/cross_summary.md"
if [[ ! -f "$OUTPUT_FILE" ]]; then
  fail "expected cross summary to be generated"
fi

if ! rg -n "\\| macos \\| PROBE_ONLY \\| probe: $PROBE_REL \\(status=error\\) \\|" "$OUTPUT_FILE" >/dev/null; then
  fail "cross summary should auto-detect the run-specific macOS probe when no macOS summary is provided"
fi

echo "[PASS] wave-b cross-platform summary macOS probe default contract passed"
