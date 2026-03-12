#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_local_first_guard_bundle.sh"
WORK_REL="tmp/test_wave_c_b125_only_platform_path_check_mode"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    echo "[INFO] top of output ($file):"
    sed -n '1,220p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b125 only platform path-check mode contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ONLY="b125_only_platform_$$"
OUT_ONLY_REL="$WORK_REL/reports/bundle_only.md"
OUT_ONLY="$ROOT_DIR/$OUT_ONLY_REL"

B123_REPORT="$WORK_DIR/reports/wave_c_b123_local_first_continuity_${RUN_ONLY}.md"
B124_REPORT="$WORK_DIR/reports/wave_c_b124_local_drift_watch_${RUN_ONLY}.md"
B123_LOG="$WORK_DIR/reports/wave_c_b123_local_first_continuity_${RUN_ONLY}.log"
B124_LOG="$WORK_DIR/reports/wave_c_b124_local_drift_watch_${RUN_ONLY}.log"
B125A_LOG="$WORK_DIR/reports/wave_c_b125_platform_path_checks_${RUN_ONLY}.log"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ONLY" \
  --reports-dir "$WORK_REL/reports" \
  --output "$OUT_ONLY_REL" \
  --only-platform-path-check-dryrun >/dev/null)

[[ -f "$OUT_ONLY" ]] || fail "only mode should generate bundle report"
[[ ! -f "$B123_REPORT" ]] || fail "only mode should not generate B123 report"
[[ ! -f "$B124_REPORT" ]] || fail "only mode should not generate B124 report"
[[ ! -f "$B123_LOG" ]] || fail "only mode should not generate B123 log"
[[ ! -f "$B124_LOG" ]] || fail "only mode should not generate B124 log"
[[ -f "$B125A_LOG" ]] || fail "only mode should generate B125A platform log"

assert_contains "$OUT_ONLY" "| B123 local continuity | SKIP | SKIPPED | <none> | <none> |"
assert_contains "$OUT_ONLY" "| B124 local drift watch | SKIP | SKIPPED | <none> | <none> |"
assert_contains "$OUT_ONLY" "| B125A platform path-check dry-run batch | 0 | PASS | <none> | $WORK_REL/reports/wave_c_b125_platform_path_checks_${RUN_ONLY}.log |"

RUN_ONLY_SKIP="b125_only_platform_skip_$$"
OUT_ONLY_SKIP_REL="$WORK_REL/reports/bundle_only_skip.md"
OUT_ONLY_SKIP="$ROOT_DIR/$OUT_ONLY_SKIP_REL"
B125A_SKIP_LOG="$WORK_DIR/reports/wave_c_b125_platform_path_checks_${RUN_ONLY_SKIP}.log"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ONLY_SKIP" \
  --reports-dir "$WORK_REL/reports" \
  --output "$OUT_ONLY_SKIP_REL" \
  --only-platform-path-check-dryrun \
  --skip-platform-path-checks-dryrun >/dev/null)

[[ -f "$OUT_ONLY_SKIP" ]] || fail "only+skip mode should generate bundle report"
[[ ! -f "$B125A_SKIP_LOG" ]] || fail "only+skip mode should not generate B125A log"
assert_contains "$OUT_ONLY_SKIP" "| B123 local continuity | SKIP | SKIPPED | <none> | <none> |"
assert_contains "$OUT_ONLY_SKIP" "| B124 local drift watch | SKIP | SKIPPED | <none> | <none> |"
assert_contains "$OUT_ONLY_SKIP" "| B125A platform path-check dry-run batch | SKIP | SKIPPED | <none> | <none> |"

echo "[PASS] wave c b125 only platform path-check mode contract passed"
