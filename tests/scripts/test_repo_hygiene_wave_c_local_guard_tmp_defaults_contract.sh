#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] missing pattern '$pattern' in $file"
    sed -n '1,240p' "$file" || true
    fail "expected pattern not found"
  fi
}

echo "[TEST] repo hygiene wave c local guard tmp defaults contract"

B123="$ROOT_DIR/scripts/check_wave_c_local_first_continuity.sh"
B124="$ROOT_DIR/scripts/check_wave_c_local_drift_watch.sh"
B125="$ROOT_DIR/scripts/run_wave_c_local_first_guard_bundle.sh"
B126="$ROOT_DIR/scripts/summarize_wave_c_local_guard_history.sh"
B129="$ROOT_DIR/scripts/run_wave_c_local_guard_oncall_check.sh"
B132="$ROOT_DIR/scripts/generate_wave_c_local_first_status_snapshot.sh"
B138="$ROOT_DIR/scripts/run_wave_c_pre_ci_reenable_full_gate.sh"
B139="$ROOT_DIR/scripts/cleanup_wave_c_local_guard_reports.sh"
B140="$ROOT_DIR/scripts/check_wave_c_local_guard_consistency.sh"
B142="$ROOT_DIR/scripts/export_wave_c_local_guard_status_json.sh"
B144="$ROOT_DIR/scripts/run_wave_c_local_guard_ops_pack.sh"

assert_contains "$B123" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B123" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_ID}.md"'

assert_contains "$B124" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B124" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b124_local_drift_watch_${RUN_ID}.md"'
assert_contains "$B124" 'latest_continuity="$(ls -1t "$REPORTS_DIR"/wave_c_b123_local_first_continuity_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B124" 'latest_prev_drift="$(ls -1t "$REPORTS_DIR"/wave_c_b124_local_drift_watch_*.md 2>/dev/null | head -1 || true)"'

assert_contains "$B125" 'DEFAULT_REPORTS_DIR="tmp/wave_c_local_guard_reports"'
assert_contains "$B125" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"'

assert_contains "$B126" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B126" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b126_local_guard_history_${RUN_ID}.md"'
assert_contains "$B126" 'mapfile -t reports < <(ls -1t "$REPORTS_DIR"/wave_c_b125_local_guard_bundle_*.md 2>/dev/null | head -n "$LIMIT" || true)'

assert_contains "$B129" 'DEFAULT_REPORTS_DIR="tmp/wave_c_local_guard_reports"'
assert_contains "$B129" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"'

assert_contains "$B132" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B132" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b132_local_first_status_snapshot_${RUN_ID}.md"'
assert_contains "$B132" 'latest_b123="$(ls -1t "$REPORTS_DIR"/wave_c_b123_local_first_continuity_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B132" 'latest_b129="$(ls -1t "$REPORTS_DIR"/wave_c_b129_oncall_check_*.md 2>/dev/null | head -1 || true)"'

assert_contains "$B138" 'DEFAULT_REPORTS_DIR="tmp/wave_c_local_guard_reports"'
assert_contains "$B138" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"'

assert_contains "$B139" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B139" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b139_local_guard_cleanup_plan_${RUN_ID}.md"'
assert_contains "$B139" '"$REPORTS_DIR/wave_c_b129_oncall_check_*.md"'
assert_contains "$B139" '"$REPORTS_DIR/wave_c_b124_local_drift_watch_*.md"'

assert_contains "$B140" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B140" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b140_local_guard_consistency_${RUN_ID}.md"'

assert_contains "$B142" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B142" 'OUTPUT_JSON="$REPORTS_DIR/wave_c_b142_local_guard_status_${RUN_ID}.json"'
assert_contains "$B142" 'latest_oncall="$(ls -1t "$REPORTS_DIR"/wave_c_b129_oncall_check_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B142" 'latest_consistency="$(ls -1t "$REPORTS_DIR"/wave_c_b140_local_guard_consistency_*.md 2>/dev/null | head -1 || true)"'

assert_contains "$B144" 'DEFAULT_REPORTS_DIR="tmp/wave_c_local_guard_reports"'
assert_contains "$B144" 'REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"'

echo "[PASS] repo hygiene wave c local guard tmp defaults contract passed"
