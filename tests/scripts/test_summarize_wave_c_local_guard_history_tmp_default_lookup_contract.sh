#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_BASE="$(date +%Y%m%d_%H%M%S)_$$"
SANDBOX_ROOT="$(mktemp -d)"
TMP_BUNDLE="$SANDBOX_ROOT/tmp/test-reports/wave_c_b125_local_guard_bundle_${RUN_BASE}_tmp.md"
LEGACY_BUNDLE="$SANDBOX_ROOT/test-reports/wave_c_b125_local_guard_bundle_${RUN_BASE}_legacy.md"
OUT_TMP_FIRST="$SANDBOX_ROOT/tmp/test-reports/wave_c_b126_local_guard_history_${RUN_BASE}_tmp_first.md"
OUT_FALLBACK="$SANDBOX_ROOT/tmp/test-reports/wave_c_b126_local_guard_history_${RUN_BASE}_fallback.md"

mkdir -p "$SANDBOX_ROOT/scripts" "$SANDBOX_ROOT/tmp/test-reports" "$SANDBOX_ROOT/test-reports"
cp "$PROJECT_ROOT/scripts/summarize_wave_c_local_guard_history.sh" "$SANDBOX_ROOT/scripts/"
trap 'rm -rf "$SANDBOX_ROOT"' EXIT

cat > "$TMP_BUNDLE" <<EOF_TMP
# Wave C B125 Local-First Guard Bundle

- run_id: ${RUN_BASE}_tmp
- overall: **PASS**
EOF_TMP

cat > "$LEGACY_BUNDLE" <<EOF_LEGACY
# Wave C B125 Local-First Guard Bundle

- run_id: ${RUN_BASE}_legacy
- overall: **FAIL**
EOF_LEGACY

touch -t 202603180101 "$TMP_BUNDLE"
touch -t 202603180102 "$LEGACY_BUNDLE"

cd "$SANDBOX_ROOT"

bash scripts/summarize_wave_c_local_guard_history.sh --run-id "${RUN_BASE}_tmp_first" --limit 1

if [[ ! -f "$OUT_TMP_FIRST" ]]; then
  echo "[FAIL] B126 default output should land in tmp/test-reports"
  exit 1
fi

if ! rg -F --quiet -- "- trend_state: **STABLE**" "$OUT_TMP_FIRST"; then
  echo "[FAIL] B126 should prefer tmp/test-reports bundles before legacy ones"
  sed -n '1,220p' "$OUT_TMP_FIRST" || true
  exit 1
fi

if ! rg -F --quiet -- "tmp/test-reports/wave_c_b125_local_guard_bundle_${RUN_BASE}_tmp.md" "$OUT_TMP_FIRST"; then
  echo "[FAIL] B126 should summarize tmp/test-reports bundle when tmp data exists"
  sed -n '1,220p' "$OUT_TMP_FIRST" || true
  exit 1
fi

rm -f "$TMP_BUNDLE"

bash scripts/summarize_wave_c_local_guard_history.sh --run-id "${RUN_BASE}_fallback" --limit 1

if [[ ! -f "$OUT_FALLBACK" ]]; then
  echo "[FAIL] B126 fallback output should still land in tmp/test-reports"
  exit 1
fi

if ! rg -F --quiet -- "- trend_state: **DEGRADED**" "$OUT_FALLBACK"; then
  echo "[FAIL] B126 should fall back to legacy test-reports when tmp data is absent"
  sed -n '1,220p' "$OUT_FALLBACK" || true
  exit 1
fi

if ! rg -F --quiet -- "test-reports/wave_c_b125_local_guard_bundle_${RUN_BASE}_legacy.md" "$OUT_FALLBACK"; then
  echo "[FAIL] B126 fallback should summarize legacy bundle path"
  sed -n '1,220p' "$OUT_FALLBACK" || true
  exit 1
fi

echo "[PASS] summarize_wave_c_local_guard_history tmp default lookup contract passed"
