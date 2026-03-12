#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ARCHIVE_DIR="$ROOT_DIR/docs/archive/reports/wave-c-quick-enablement-history"
MANIFEST="$ROOT_DIR/docs/archive/reports/2026-03-test-reports-migration-manifest.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene historical wave_c quick enablement bucket contract"

if git -C "$ROOT_DIR" ls-files \
  "test-reports/wave_c_quick_sprint_bundle*" \
  "test-reports/wave_c_b101_validation*" \
  "test-reports/wave_c_b107_threshold_eval*" \
  "test-reports/wave_c_b108_default_on_readiness*" \
  "test-reports/wave_c_b109_canary_rollout*" \
  "test-reports/wave_c_b110_recheck*" \
  "test-reports/wave_c_b110_rollback_drill*" \
  "test-reports/wave_c_b115_workflow_enable_prereq*" \
  "test-reports/wave_c_b119_first_run_preflight*" \
  "test-reports/wave_c_b120_post_trigger_observability*" | grep -q .; then
  echo "[INFO] remaining tracked wave_c quick/enablement bucket sample:"
  git -C "$ROOT_DIR" ls-files \
    "test-reports/wave_c_quick_sprint_bundle*" \
    "test-reports/wave_c_b101_validation*" \
    "test-reports/wave_c_b107_threshold_eval*" \
    "test-reports/wave_c_b108_default_on_readiness*" \
    "test-reports/wave_c_b109_canary_rollout*" \
    "test-reports/wave_c_b110_recheck*" \
    "test-reports/wave_c_b110_rollback_drill*" \
    "test-reports/wave_c_b115_workflow_enable_prereq*" \
    "test-reports/wave_c_b119_first_run_preflight*" \
    "test-reports/wave_c_b120_post_trigger_observability*" | sed -n "1,40p"
  fail "historical wave_c quick/enablement bucket should not remain tracked under test-reports"
fi

[[ -d "$ARCHIVE_DIR" ]] || fail "missing archive dir: docs/archive/reports/wave-c-quick-enablement-history"
[[ -f "$MANIFEST" ]] || fail "missing migration manifest"

expected_refs=(
  "wave_c_b101_validation_20260208_043500.md"
  "wave_c_b101_validation_20260208_045835.md"
  "wave_c_b101_validation_20260208_050421.md"
  "wave_c_b101_validation_20260208_051419.md"
  "wave_c_b101_validation_20260208_173726.md"
  "wave_c_b107_threshold_eval_20260208_052000.md"
  "wave_c_b107_threshold_eval_20260208_053500.md"
  "wave_c_b107_threshold_eval_20260208_173726.md"
  "wave_c_b108_default_on_readiness_20260208_052300.md"
  "wave_c_b108_default_on_readiness_20260208_053500.md"
  "wave_c_b108_default_on_readiness_20260208_173726.md"
  "wave_c_b109_canary_rollout_20260208_052700.md"
  "wave_c_b109_canary_rollout_20260208_053500.md"
  "wave_c_b109_canary_rollout_20260208_173726.md"
  "wave_c_b110_recheck_20260208_053000.md"
  "wave_c_b110_recheck_20260208_053500.md"
  "wave_c_b110_recheck_20260208_173726.md"
  "wave_c_b110_rollback_drill_20260208_053000.md"
  "wave_c_b110_rollback_drill_20260208_053500.md"
  "wave_c_b110_rollback_drill_20260208_173726.md"
  "wave_c_b115_workflow_enable_prereq_20260208_174800.md"
  "wave_c_b115_workflow_enable_prereq_20260208_174800_strict.md"
  "wave_c_b119_first_run_preflight_20260208_175100.md"
  "wave_c_b120_post_trigger_observability_20260208_173726.md"
  "wave_c_quick_sprint_bundle_20260208_053500.md"
  "wave_c_quick_sprint_bundle_20260208_173726.md"
)

expected_removed=(
  "wave_c_b101_validation_latest.md"
  "wave_c_b107_threshold_eval_20260208_051900.md"
  "wave_c_b115_workflow_enable_prereq_20260208_174600.md"
  "wave_c_b115_workflow_enable_prereq_20260208_174600_strict.md"
)

for name in "${expected_refs[@]}"; do
  [[ -f "$ARCHIVE_DIR/$name" ]] || fail "missing archived referenced Wave C quick/enablement artifact: $name"
done

for name in "${expected_refs[@]}"; do
  if rg -n -F -- "test-reports/$name" "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_wave_c_quick_refs.txt 2>/dev/null; then
    echo "[INFO] stale Wave C quick refs outside archive for $name:"
    sed -n "1,160p" /tmp/fafafa_historical_wave_c_quick_refs.txt
    fail "stale test-reports/$name references should be migrated to archive paths"
  fi
done

for name in "${expected_removed[@]}"; do
  if rg -n -F -- "test-reports/$name" "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_wave_c_quick_removed_refs.txt 2>/dev/null; then
    echo "[INFO] stale removed Wave C quick refs outside archive for $name:"
    sed -n "1,160p" /tmp/fafafa_historical_wave_c_quick_removed_refs.txt
    fail "stale test-reports/$name references should not remain outside historical artifacts"
  fi
done

echo "[PASS] repo hygiene historical wave_c quick enablement bucket contract passed"
