#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ARCHIVE_DIR="$ROOT_DIR/docs/archive/reports/wave-b-history"
MANIFEST="$ROOT_DIR/docs/archive/reports/2026-03-test-reports-migration-manifest.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene historical wave_b bucket contract"

if git -C "$ROOT_DIR" ls-files "test-reports/wave_b_*" | grep -q .; then
  echo "[INFO] remaining tracked wave_b bucket sample:"
  git -C "$ROOT_DIR" ls-files "test-reports/wave_b_*" | sed -n "1,24p"
  fail "historical wave_b_* bucket should not remain tracked under test-reports"
fi

[[ -d "$ARCHIVE_DIR" ]] || fail "missing archive dir: docs/archive/reports/wave-b-history"
[[ -f "$MANIFEST" ]] || fail "missing migration manifest"

expected_refs=(
  "wave_b_b2_closure_readiness_20260208_041500.md"
  "wave_b_b2_evidence_consistency_20260208_041500.md"
  "wave_b_b2_handoff_bundle_20260208_041500.md"
  "wave_b_ci_gate_summary_20260208_022636.md"
  "wave_b_ci_gate_summary_20260208_025426.md"
  "wave_b_ci_gate_summary_20260208_034029.md"
  "wave_b_cross_platform_summary_20260208_034029.md"
  "wave_b_cross_platform_summary_20260208_041500.md"
  "wave_b_macos_gate_probe_20260208.json"
  "wave_b_macos_gate_summary_20260208_0350.md"
  "wave_b_macos_gate_summary_20260208_041500.md"
)

for name in "${expected_refs[@]}"; do
  [[ -f "$ARCHIVE_DIR/$name" ]] || fail "missing archived referenced Wave B artifact: $name"
done

for name in "${expected_refs[@]}"; do
  if rg -n -- "test-reports/$name" "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_wave_b_refs.txt 2>/dev/null; then
    echo "[INFO] stale Wave B refs outside archive for $name:"
    sed -n "1,160p" /tmp/fafafa_historical_wave_b_refs.txt
    fail "stale test-reports/$name references should be migrated to archive paths"
  fi
done

echo "[PASS] repo hygiene historical wave_b bucket contract passed"
