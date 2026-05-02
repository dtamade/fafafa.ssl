#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

STABLE_RUN="stable_$(date +%s)_$$"
INFLIGHT_RUN="inflight_$(date +%s)_$$"
SANDBOX_ROOT="$(mktemp -d)"
OUT_FILE="$SANDBOX_ROOT/tmp/test-reports/wave_c_b132_local_first_status_snapshot_consistent_run_out.md"

mkdir -p "$SANDBOX_ROOT/scripts" "$SANDBOX_ROOT/.github/workflows" "$SANDBOX_ROOT/tmp/test-reports"
cp "$PROJECT_ROOT/scripts/generate_wave_c_local_first_status_snapshot.sh" "$SANDBOX_ROOT/scripts/"
trap 'rm -rf "$SANDBOX_ROOT"' EXIT

touch "$SANDBOX_ROOT/.github/workflows/wave-c-quick-sprint-manual.yml.disabled"

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${STABLE_RUN}.md" <<EOF_B123_STABLE
# B123
- local_first_state: **LOCAL_READY**
EOF_B123_STABLE

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${STABLE_RUN}.md" <<EOF_B124_STABLE
# B124
- local_drift_state: **LOCAL_STABLE**
EOF_B124_STABLE

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b125_local_guard_bundle_${STABLE_RUN}.md" <<EOF_B125_STABLE
# B125
- overall: **PASS**
EOF_B125_STABLE

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b126_local_guard_history_${STABLE_RUN}.md" <<EOF_B126_STABLE
# B126
- trend_state: **STABLE**
EOF_B126_STABLE

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b129_oncall_check_${STABLE_RUN}.md" <<EOF_B129_STABLE
# B129
- overall: **PASS**
EOF_B129_STABLE

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${INFLIGHT_RUN}.md" <<EOF_B123_INFLIGHT
# B123
- local_first_state: **HOLD**
EOF_B123_INFLIGHT

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${INFLIGHT_RUN}.md" <<EOF_B124_INFLIGHT
# B124
- local_drift_state: **HOLD**
EOF_B124_INFLIGHT

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b125_local_guard_bundle_${INFLIGHT_RUN}.md" <<EOF_B125_INFLIGHT
# B125
- overall: **FAIL**
EOF_B125_INFLIGHT

touch -d '2 hours ago' \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${STABLE_RUN}.md" \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${STABLE_RUN}.md" \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b125_local_guard_bundle_${STABLE_RUN}.md" \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b126_local_guard_history_${STABLE_RUN}.md" \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b129_oncall_check_${STABLE_RUN}.md"

touch -d '1 hour ago' \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${INFLIGHT_RUN}.md" \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${INFLIGHT_RUN}.md" \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b125_local_guard_bundle_${INFLIGHT_RUN}.md"

cd "$SANDBOX_ROOT"

bash scripts/generate_wave_c_local_first_status_snapshot.sh --run-id consistent_run --strict --output "$OUT_FILE"

if ! rg -F --quiet -- '- snapshot_state: **GREEN**' "$OUT_FILE"; then
  echo "[FAIL] B132 should stay GREEN by selecting the latest complete guard run"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

for line in \
  "- B123: tmp/test-reports/wave_c_b123_local_first_continuity_${STABLE_RUN}.md" \
  "- B124: tmp/test-reports/wave_c_b124_local_drift_watch_${STABLE_RUN}.md" \
  "- B125: tmp/test-reports/wave_c_b125_local_guard_bundle_${STABLE_RUN}.md" \
  "- B126: tmp/test-reports/wave_c_b126_local_guard_history_${STABLE_RUN}.md" \
  "- B129: tmp/test-reports/wave_c_b129_oncall_check_${STABLE_RUN}.md"; do
  if ! rg -F --quiet -- "$line" "$OUT_FILE"; then
    echo "[FAIL] B132 should use one complete run instead of mixing in-flight files"
    sed -n '1,220p' "$OUT_FILE" || true
    exit 1
  fi
done

echo "[PASS] Wave C B132 consistent-run contract passed"
