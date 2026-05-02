#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_ID="weekly_bundle_$(date +%Y%m%d_%H%M%S)_$$"
SANDBOX_ROOT="$(mktemp -d)"
OUT_FILE="$SANDBOX_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.md"

mkdir -p \
  "$SANDBOX_ROOT/scripts" \
  "$SANDBOX_ROOT/.github/workflows" \
  "$SANDBOX_ROOT/docs/test_reports" \
  "$SANDBOX_ROOT/tmp/test-reports"
cp "$PROJECT_ROOT/scripts/check_wave_c_local_drift_watch.sh" "$SANDBOX_ROOT/scripts/"
trap 'rm -rf "$SANDBOX_ROOT"' EXIT

touch "$SANDBOX_ROOT/.github/workflows/wave-c-quick-sprint-manual.yml.disabled"

cat > "$SANDBOX_ROOT/docs/test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md" <<'EOF_B121'
# B121
EOF_B121

cat > "$SANDBOX_ROOT/docs/test_reports/WAVE_C_B122_CI_DEFERRED_LOCAL_MODE_2026-02-08.md" <<'EOF_B122'
# B122
EOF_B122

cat > "$SANDBOX_ROOT/docs/test_reports/WAVE_C_B123_LOCAL_FIRST_CONTINUITY_RESULT_2026-02-09.md" <<'EOF_B123_DOC'
# B123 Result
EOF_B123_DOC

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_quick_sprint_bundle_${RUN_ID}.md" <<EOF_BUNDLE
# Wave C Quick Sprint Bundle

- run_id: $RUN_ID
- overall: **PASS**
EOF_BUNDLE

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${RUN_ID}.md" <<EOF_CONT
# Wave C B123 Local-First Continuity Check

- run_id: $RUN_ID
- local_first_state: **LOCAL_READY**
EOF_CONT

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_prev.md" <<'EOF_PREV'
# Wave C B124 Local-First Drift Watch

- run_id: prev
- local_drift_state: **LOCAL_STABLE**
EOF_PREV

touch -d '4 days ago' "$SANDBOX_ROOT/tmp/test-reports/wave_c_quick_sprint_bundle_${RUN_ID}.md"
touch -d '2 hours ago' "$SANDBOX_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_prev.md"

cd "$SANDBOX_ROOT"

bash scripts/check_wave_c_local_drift_watch.sh --run-id "$RUN_ID" --strict --output "$OUT_FILE"

if [[ ! -f "$OUT_FILE" ]]; then
  echo "[FAIL] B124 weekly-window output not generated"
  exit 1
fi

if ! rg -F --quiet -- '- local_drift_state: **LOCAL_STABLE**' "$OUT_FILE"; then
  echo "[FAIL] B124 default bundle age window should tolerate a 4-day-old bundle under weekly cadence"
  sed -n '1,260p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '| bundle_age_hours |' "$OUT_FILE"; then
  echo "[FAIL] B124 should still surface bundle age evidence"
  sed -n '1,260p' "$OUT_FILE" || true
  exit 1
fi

echo "[PASS] check_wave_c_local_drift_watch weekly bundle window contract passed"
