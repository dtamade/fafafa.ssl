#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_local_first_guard_bundle.sh"
WORK_REL="tmp/test_wave_c_b125_platform_path_checks_integration"
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
    sed -n '1,160p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c local-first guard bundle platform path-check integration contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_DEFAULT="b125_platform_default_$$"
DEFAULT_OUT_REL="$WORK_REL/reports/bundle_default.md"
DEFAULT_OUT="$ROOT_DIR/$DEFAULT_OUT_REL"
DEFAULT_PLATFORM_LOG="$WORK_DIR/reports/wave_c_b125_platform_path_checks_${RUN_DEFAULT}.log"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_DEFAULT" \
  --reports-dir "$WORK_REL/reports" \
  --output "$DEFAULT_OUT_REL" >/dev/null)

[[ -f "$DEFAULT_OUT" ]] || fail "default run should generate bundle report"
[[ -f "$DEFAULT_PLATFORM_LOG" ]] || fail "default run should generate platform path-check log"
assert_contains "$DEFAULT_OUT" "B125A platform path-check dry-run batch"
assert_contains "$DEFAULT_PLATFORM_LOG" "[PASS] linux-focused multi-platform dry-run batch passed"

RUN_SKIP="b125_platform_skip_$$"
SKIP_OUT_REL="$WORK_REL/reports/bundle_skip.md"
SKIP_OUT="$ROOT_DIR/$SKIP_OUT_REL"
SKIP_PLATFORM_LOG="$WORK_DIR/reports/wave_c_b125_platform_path_checks_${RUN_SKIP}.log"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_SKIP" \
  --reports-dir "$WORK_REL/reports" \
  --output "$SKIP_OUT_REL" \
  --skip-platform-path-checks-dryrun >/dev/null)

[[ -f "$SKIP_OUT" ]] || fail "skip run should generate bundle report"
[[ ! -f "$SKIP_PLATFORM_LOG" ]] || fail "skip run should not generate platform path-check log"
assert_contains "$SKIP_OUT" "| B125A platform path-check dry-run batch | SKIP | SKIPPED | <none> | <none> |"

echo "[PASS] wave c local-first guard bundle platform path-check integration contract passed"
