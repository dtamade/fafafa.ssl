#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_strict_metadata_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="prepare_strict_metadata_truth"
OUTPUT_DIR_REL="$WORK_REL/out"
OUTPUT_DIR_ABS="$ROOT_DIR/$OUTPUT_DIR_REL"
CLOSURE_ABS="$OUTPUT_DIR_ABS/wave_b_b2_closure_readiness_${RUN_ID}.md"
CONSISTENCY_ABS="$OUTPUT_DIR_ABS/wave_b_b2_evidence_consistency_${RUN_ID}.md"
BUNDLE_ABS="$OUTPUT_DIR_ABS/wave_b_b2_handoff_bundle_${RUN_ID}.md"

mkdir -p "$WORK_DIR" "$OUTPUT_DIR_ABS"
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

set +e
bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --output-dir "$OUTPUT_DIR_REL" \
  --strict >/dev/null 2>&1
exit_code=$?
set -e

if [[ $exit_code -eq 0 ]]; then
  fail "prepare --strict should exit non-zero when macOS and Windows closure are still pending"
fi

for file in "$CLOSURE_ABS" "$CONSISTENCY_ABS" "$BUNDLE_ABS"; do
  if [[ ! -f "$file" ]]; then
    fail "expected generated artifact under strict prepare run: $file"
  fi
done

if ! rg -n "^- strict_mode: true$" "$BUNDLE_ABS" >/dev/null; then
  fail "handoff bundle should record strict_mode true under prepare --strict"
fi

if ! rg -n "^- strict_mode: true$" "$CLOSURE_ABS" >/dev/null; then
  fail "closure readiness report should record strict_mode true under prepare --strict"
fi

if ! rg -n "^- strict_mode: true$" "$CONSISTENCY_ABS" >/dev/null; then
  fail "evidence consistency report should record strict_mode true under prepare --strict"
fi

echo "[PASS] prepare_wave_b_b2 strict metadata contract passed"
