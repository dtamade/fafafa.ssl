#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_TAG="contract_$(date +%s)_$$"
BUNDLE_FILE="$PROJECT_ROOT/tmp/test-reports/wave_c_quick_sprint_bundle_${RUN_TAG}.md"
CONTINUITY_FILE="$PROJECT_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${RUN_TAG}.md"
DRIFT_FILE="$PROJECT_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_TAG}.md"
B123_OUT="$PROJECT_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${RUN_TAG}_out.md"
B124_OUT="$PROJECT_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_TAG}_out.md"

mkdir -p "$PROJECT_ROOT/tmp/test-reports"
trap 'rm -f "$BUNDLE_FILE" "$CONTINUITY_FILE" "$DRIFT_FILE" "$B123_OUT" "$B124_OUT"' EXIT

cat > "$BUNDLE_FILE" <<EOF_BUNDLE
# Wave C Quick Sprint Bundle

- run_id: $RUN_TAG
- overall: **PASS**
EOF_BUNDLE

cat > "$CONTINUITY_FILE" <<EOF_CONT
# Wave C B123 Local-First Continuity Check

- run_id: $RUN_TAG
- local_first_state: **LOCAL_READY**
EOF_CONT

cat > "$DRIFT_FILE" <<EOF_DRIFT
# Wave C B124 Local-First Drift Watch

- run_id: $RUN_TAG
- local_drift_state: **LOCAL_STABLE**
EOF_DRIFT

touch "$BUNDLE_FILE" "$CONTINUITY_FILE" "$DRIFT_FILE"

cd "$PROJECT_ROOT"

bash scripts/check_wave_c_local_first_continuity.sh --run-id "${RUN_TAG}_b123" --output "$B123_OUT"

if ! rg -F --quiet -- "- latest_bundle: tmp/test-reports/wave_c_quick_sprint_bundle_${RUN_TAG}.md" "$B123_OUT"; then
  echo "[FAIL] B123 should pick latest bundle from tmp/test-reports"
  sed -n '1,220p' "$B123_OUT" || true
  exit 1
fi

bash scripts/check_wave_c_local_drift_watch.sh --run-id "${RUN_TAG}_b124" --output "$B124_OUT"

if ! rg -F --quiet -- "| latest_bundle_file | tmp/test-reports/wave_c_quick_sprint_bundle_${RUN_TAG}.md | required | PASS |" "$B124_OUT"; then
  echo "[FAIL] B124 should pick latest bundle from tmp/test-reports"
  sed -n '1,260p' "$B124_OUT" || true
  exit 1
fi

if ! rg --quiet -- "\\| latest_continuity_file \\| tmp/test-reports/wave_c_b123_local_first_continuity_.* \\| required \\| PASS \\|" "$B124_OUT"; then
  echo "[FAIL] B124 should pick latest continuity report from tmp/test-reports"
  sed -n '1,260p' "$B124_OUT" || true
  exit 1
fi

echo "[PASS] Wave C local scripts tmp/test-reports lookup contract passed"
