#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ARCHIVE_DIR="$ROOT_DIR/docs/archive/reports/wave-c-local-first-guard-history"
MANIFEST="$ROOT_DIR/docs/archive/reports/2026-03-test-reports-migration-manifest.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene historical wave_c local-first/local-guard bucket contract"

if git -C "$ROOT_DIR" ls-files   "test-reports/wave_c_b123_local_first_continuity*" \
  "test-reports/wave_c_b124_local_drift_watch*" \
  "test-reports/wave_c_b125_local_guard_bundle*" \
  "test-reports/wave_c_b126_local_guard_history*" \
  "test-reports/wave_c_b129_oncall_check*" \
  "test-reports/wave_c_b132_local_first_status_snapshot*" \
  "test-reports/wave_c_b139_local_guard_cleanup_plan*" \
  "test-reports/wave_c_b140_local_guard_consistency*" \
  "test-reports/wave_c_b142_local_guard_status*" \
  "test-reports/wave_c_b143_alert_thresholds*" \
  "test-reports/wave_c_b144_local_guard_ops_pack*" | grep -q .; then
  echo "[INFO] remaining tracked wave_c local-first/local-guard bucket sample:"
  git -C "$ROOT_DIR" ls-files     "test-reports/wave_c_b123_local_first_continuity*" \
    "test-reports/wave_c_b124_local_drift_watch*" \
    "test-reports/wave_c_b125_local_guard_bundle*" \
    "test-reports/wave_c_b126_local_guard_history*" \
    "test-reports/wave_c_b129_oncall_check*" \
    "test-reports/wave_c_b132_local_first_status_snapshot*" \
    "test-reports/wave_c_b139_local_guard_cleanup_plan*" \
    "test-reports/wave_c_b140_local_guard_consistency*" \
    "test-reports/wave_c_b142_local_guard_status*" \
    "test-reports/wave_c_b143_alert_thresholds*" \
    "test-reports/wave_c_b144_local_guard_ops_pack*" | sed -n "1,80p"
  fail "historical wave_c local-first/local-guard bucket should not remain tracked under test-reports"
fi

[[ -d "$ARCHIVE_DIR" ]] || fail "missing archive dir: docs/archive/reports/wave-c-local-first-guard-history"
[[ -f "$MANIFEST" ]] || fail "missing migration manifest"

expected_refs=(
  "wave_c_b123_local_first_continuity_20260209_030722.md"
  "wave_c_b123_local_first_continuity_20260209_031724.md"
  "wave_c_b123_local_first_continuity_20260209_032023.md"
  "wave_c_b123_local_first_continuity_20260209_032257.md"
  "wave_c_b123_local_first_continuity_20260209_032433.md"
  "wave_c_b123_local_first_continuity_20260209_032549.md"
  "wave_c_b123_local_first_continuity_20260209_032652.md"
  "wave_c_b123_local_first_continuity_20260209_032925.md"
  "wave_c_b123_local_first_continuity_20260209_033028.md"
  "wave_c_b123_local_first_continuity_20260209_033133.md"
  "wave_c_b123_local_first_continuity_20260209_033224.md"
  "wave_c_b123_local_first_continuity_20260209_045450.md"
  "wave_c_b123_local_first_continuity_20260209_050311.md"
  "wave_c_b123_local_first_continuity_20260209_051129.md"
  "wave_c_b124_local_drift_watch_20260209_031724.md"
  "wave_c_b124_local_drift_watch_20260209_032023.md"
  "wave_c_b124_local_drift_watch_20260209_032257.md"
  "wave_c_b124_local_drift_watch_20260209_032433.md"
  "wave_c_b124_local_drift_watch_20260209_032549.md"
  "wave_c_b124_local_drift_watch_20260209_032652.md"
  "wave_c_b124_local_drift_watch_20260209_032925.md"
  "wave_c_b124_local_drift_watch_20260209_033028.md"
  "wave_c_b124_local_drift_watch_20260209_033133.md"
  "wave_c_b124_local_drift_watch_20260209_033224.md"
  "wave_c_b124_local_drift_watch_20260209_045450.md"
  "wave_c_b124_local_drift_watch_20260209_050311.md"
  "wave_c_b124_local_drift_watch_20260209_051129.md"
  "wave_c_b125_local_guard_bundle_20260209_031724.md"
  "wave_c_b125_local_guard_bundle_20260209_032023.md"
  "wave_c_b125_local_guard_bundle_20260209_032257.md"
  "wave_c_b125_local_guard_bundle_20260209_032433.md"
  "wave_c_b125_local_guard_bundle_20260209_032549.md"
  "wave_c_b125_local_guard_bundle_20260209_032652.md"
  "wave_c_b125_local_guard_bundle_20260209_032925.md"
  "wave_c_b125_local_guard_bundle_20260209_033028.md"
  "wave_c_b125_local_guard_bundle_20260209_033133.md"
  "wave_c_b125_local_guard_bundle_20260209_033224.md"
  "wave_c_b125_local_guard_bundle_20260209_045450.md"
  "wave_c_b125_local_guard_bundle_20260209_050311.md"
  "wave_c_b125_local_guard_bundle_20260209_051129.md"
  "wave_c_b126_local_guard_history_20260209_031849.md"
  "wave_c_b126_local_guard_history_20260209_032433.md"
  "wave_c_b126_local_guard_history_20260209_032652.md"
  "wave_c_b126_local_guard_history_20260209_051129.md"
  "wave_c_b129_oncall_check_20260209_032433.md"
  "wave_c_b129_oncall_check_20260209_032652.md"
  "wave_c_b129_oncall_check_20260209_051129.md"
  "wave_c_b132_local_first_status_snapshot_20260209_032806.md"
  "wave_c_b132_local_first_status_snapshot_20260209_051129.md"
  "wave_c_b139_local_guard_cleanup_plan_20260209_050311.md"
  "wave_c_b139_local_guard_cleanup_plan_20260209_051129.md"
  "wave_c_b140_local_guard_consistency_20260209_050311.md"
  "wave_c_b140_local_guard_consistency_20260209_051129.md"
  "wave_c_b142_local_guard_status_20260209_051129.json"
  "wave_c_b143_alert_thresholds_20260209_051129.md"
  "wave_c_b144_local_guard_ops_pack_20260209_051129.md"
)

expected_removed=(
  "wave_c_b126_local_guard_history_20260209_032023.md"
  "wave_c_b126_local_guard_history_20260209_032257.md"
  "wave_c_b126_local_guard_history_20260209_032549.md"
  "wave_c_b126_local_guard_history_20260209_032925.md"
  "wave_c_b126_local_guard_history_20260209_033028.md"
  "wave_c_b126_local_guard_history_20260209_033133.md"
  "wave_c_b126_local_guard_history_20260209_033224.md"
  "wave_c_b126_local_guard_history_20260209_045450.md"
  "wave_c_b126_local_guard_history_20260209_050311.md"
  "wave_c_b129_oncall_check_20260209_032549.md"
  "wave_c_b129_oncall_check_20260209_032925.md"
  "wave_c_b129_oncall_check_20260209_033028.md"
  "wave_c_b129_oncall_check_20260209_033133.md"
  "wave_c_b129_oncall_check_20260209_033224.md"
  "wave_c_b129_oncall_check_20260209_045450.md"
  "wave_c_b129_oncall_check_20260209_050311.md"
  "wave_c_b132_local_first_status_snapshot_20260209_032925.md"
  "wave_c_b132_local_first_status_snapshot_20260209_033028.md"
  "wave_c_b132_local_first_status_snapshot_20260209_033133.md"
  "wave_c_b132_local_first_status_snapshot_20260209_033224.md"
  "wave_c_b132_local_first_status_snapshot_20260209_045450.md"
  "wave_c_b132_local_first_status_snapshot_20260209_050311.md"
  "wave_c_b140_local_guard_consistency_20260209_052849.md"
)

for name in "${expected_refs[@]}"; do
  [[ -f "$ARCHIVE_DIR/$name" ]] || fail "missing archived retained Wave C local-first/local-guard artifact: $name"
done

for name in "${expected_refs[@]}"; do
  if rg -n -F -- "test-reports/$name" "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_wave_c_local_guard_refs.txt 2>/dev/null; then
    echo "[INFO] stale Wave C local-first/local-guard refs outside archive for $name:"
    sed -n "1,160p" /tmp/fafafa_historical_wave_c_local_guard_refs.txt
    fail "stale test-reports/$name references should be migrated to archive paths"
  fi
done

for name in "${expected_removed[@]}"; do
  if rg -n -F -- "test-reports/$name" "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_wave_c_local_guard_removed_refs.txt 2>/dev/null; then
    echo "[INFO] stale removed Wave C local-first/local-guard refs outside archive for $name:"
    sed -n "1,160p" /tmp/fafafa_historical_wave_c_local_guard_removed_refs.txt
    fail "stale test-reports/$name references should not remain outside historical artifacts"
  fi
done

echo "[PASS] repo hygiene historical wave_c local-first/local-guard bucket contract passed"
