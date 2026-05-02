#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_BASE="history_recovery_$(date +%Y%m%d_%H%M%S)_$$"
SANDBOX_ROOT="$(mktemp -d)"
OUT_FILE="$SANDBOX_ROOT/tmp/test-reports/wave_c_b126_local_guard_history_${RUN_BASE}.md"
FAIL_BUNDLE="$SANDBOX_ROOT/tmp/test-reports/wave_c_b125_local_guard_bundle_${RUN_BASE}_fail.md"
PASS_BUNDLE="$SANDBOX_ROOT/tmp/test-reports/wave_c_b125_local_guard_bundle_${RUN_BASE}_pass.md"

mkdir -p "$SANDBOX_ROOT/scripts" "$SANDBOX_ROOT/tmp/test-reports"
cp "$PROJECT_ROOT/scripts/summarize_wave_c_local_guard_history.sh" "$SANDBOX_ROOT/scripts/"
trap 'rm -rf "$SANDBOX_ROOT"' EXIT

cat > "$FAIL_BUNDLE" <<EOF_FAIL
# Wave C B125 Local-First Guard Bundle

- run_id: ${RUN_BASE}_fail
- overall: **FAIL**
EOF_FAIL

cat > "$PASS_BUNDLE" <<EOF_PASS
# Wave C B125 Local-First Guard Bundle

- run_id: ${RUN_BASE}_pass
- overall: **PASS**
EOF_PASS

touch -d '2 hours ago' "$FAIL_BUNDLE"
touch -d '1 hour ago' "$PASS_BUNDLE"

cd "$SANDBOX_ROOT"

bash scripts/summarize_wave_c_local_guard_history.sh --run-id "$RUN_BASE" --limit 5 --strict --output "$OUT_FILE"

if [[ ! -f "$OUT_FILE" ]]; then
  echo "[FAIL] B126 recovery output not generated"
  exit 1
fi

if ! rg -F --quiet -- '- trend_state: **STABLE**' "$OUT_FILE"; then
  echo "[FAIL] B126 should recover to STABLE when the latest bundle is PASS"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- fail_count: 1' "$OUT_FILE"; then
  echo "[FAIL] B126 should keep historical fail_count visible after recovery"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- latest_state: PASS' "$OUT_FILE"; then
  echo "[FAIL] B126 should surface latest_state PASS in recovered trend"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

echo "[PASS] summarize_wave_c_local_guard_history latest pass recovery contract passed"
