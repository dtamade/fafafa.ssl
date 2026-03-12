#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ARCHIVE_DIR="$ROOT_DIR/docs/archive/reports/wave-c-pre-ci-submission-history"
MANIFEST="$ROOT_DIR/docs/archive/reports/2026-03-test-reports-migration-manifest.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene historical wave_c pre-ci submission bucket contract"

if git -C "$ROOT_DIR" ls-files \
  "test-reports/wave_c_b137_pre_ci_reenable_packet*" \
  "test-reports/wave_c_b138_pre_ci_reenable_full_gate*" \
  "test-reports/wave_c_b146_ci_reenable_submission_pack*" \
  "test-reports/wave_c_b147_submission_pack_check*" \
  "test-reports/wave_c_b148_ci_reenable_approval_brief*" \
  "test-reports/wave_c_b149_ci_reenable_submission_bundle*" | grep -q .; then
  echo "[INFO] remaining tracked wave_c pre-ci/submission bucket sample:"
  git -C "$ROOT_DIR" ls-files \
    "test-reports/wave_c_b137_pre_ci_reenable_packet*" \
    "test-reports/wave_c_b138_pre_ci_reenable_full_gate*" \
    "test-reports/wave_c_b146_ci_reenable_submission_pack*" \
    "test-reports/wave_c_b147_submission_pack_check*" \
    "test-reports/wave_c_b148_ci_reenable_approval_brief*" \
    "test-reports/wave_c_b149_ci_reenable_submission_bundle*" | sed -n "1,40p"
  fail "historical wave_c pre-ci/submission bucket should not remain tracked under test-reports"
fi

[[ -d "$ARCHIVE_DIR" ]] || fail "missing archive dir: docs/archive/reports/wave-c-pre-ci-submission-history"
[[ -f "$MANIFEST" ]] || fail "missing migration manifest"

expected_refs=(
  "wave_c_b137_pre_ci_reenable_packet_20260209_042549.md"
  "wave_c_b137_pre_ci_reenable_packet_20260209_045450.md"
  "wave_c_b137_pre_ci_reenable_packet_20260209_051129.md"
  "wave_c_b138_pre_ci_reenable_full_gate_20260209_045450.md"
  "wave_c_b138_pre_ci_reenable_full_gate_20260209_051129.md"
  "wave_c_b146_ci_reenable_submission_pack_20260209_052657.md"
  "wave_c_b147_submission_pack_check_20260209_052657.md"
  "wave_c_b148_ci_reenable_approval_brief_20260209_052657.md"
  "wave_c_b149_ci_reenable_submission_bundle_20260209_052657.md"
)

expected_removed=(
  "wave_c_b137_pre_ci_reenable_packet_20260209_042505.md"
  "wave_c_b137_pre_ci_reenable_packet_20260209_050311.md"
  "wave_c_b138_pre_ci_reenable_full_gate_20260209_050311.md"
  "wave_c_b146_ci_reenable_submission_pack_20260209_052849.md"
  "wave_c_b147_submission_pack_check_20260209_052849.md"
  "wave_c_b148_ci_reenable_approval_brief_20260209_052849.md"
  "wave_c_b149_ci_reenable_submission_bundle_20260209_052849.md"
)

for name in "${expected_refs[@]}"; do
  [[ -f "$ARCHIVE_DIR/$name" ]] || fail "missing archived retained Wave C pre-ci/submission artifact: $name"
done

for name in "${expected_refs[@]}"; do
  if rg -n -F -- "test-reports/$name" "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_wave_c_pre_ci_refs.txt 2>/dev/null; then
    echo "[INFO] stale Wave C pre-ci refs outside archive for $name:"
    sed -n "1,160p" /tmp/fafafa_historical_wave_c_pre_ci_refs.txt
    fail "stale test-reports/$name references should be migrated to archive paths"
  fi
done

for name in "${expected_removed[@]}"; do
  if rg -n -F -- "test-reports/$name" "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_wave_c_pre_ci_removed_refs.txt 2>/dev/null; then
    echo "[INFO] stale removed Wave C pre-ci refs outside archive for $name:"
    sed -n "1,160p" /tmp/fafafa_historical_wave_c_pre_ci_removed_refs.txt
    fail "stale test-reports/$name references should not remain outside historical artifacts"
  fi
done

echo "[PASS] repo hygiene historical wave_c pre-ci submission bucket contract passed"
