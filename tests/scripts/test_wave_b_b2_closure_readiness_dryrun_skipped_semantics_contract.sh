#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh"
WORK_REL="tmp/test_wave_b_b2_closure_readiness_dryrun_skipped"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,260p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b b2 closure readiness dryrun/skipped semantics contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

RUN_ID="wb2_closure_semantics_$$"
OUT_MIXED_REL="$WORK_REL/closure_mixed.md"
OUT_PASS_REL="$WORK_REL/closure_pass.md"

cat > "$WORK_DIR/linux_pass.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF

cat > "$WORK_DIR/macos_dryrun.md" <<EOF
# Wave B macOS Gate Summary

- run_id: $RUN_ID
- overall: DRY_RUN
EOF

cat > "$WORK_DIR/windows_skipped.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: SKIPPED
EOF

cat > "$WORK_DIR/macos_pass.md" <<EOF
# Wave B macOS Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

cat > "$WORK_DIR/windows_pass.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

# Case 1: PASS + DRY_RUN + SKIPPED => IN_PROGRESS
(cd /tmp && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_pass.md" \
  --macos-summary "$WORK_REL/macos_dryrun.md" \
  --windows-summary "$WORK_REL/windows_skipped.md" \
  --output "$OUT_MIXED_REL" >/dev/null)

OUT_MIXED="$ROOT_DIR/$OUT_MIXED_REL"
[[ -f "$OUT_MIXED" ]] || fail "mixed output report should exist"

assert_contains "$OUT_MIXED" "- closure_status: **IN_PROGRESS**"
assert_contains "$OUT_MIXED" "| macos | DRY_RUN | summary parsed | $WORK_REL/macos_dryrun.md |"
assert_contains "$OUT_MIXED" "| windows | SKIPPED | summary parsed | $WORK_REL/windows_skipped.md |"
assert_contains "$OUT_MIXED" '- `DRY_RUN`: rehearsal evidence exists but does not count toward closure.'
assert_contains "$OUT_MIXED" '- `SKIPPED`: intentionally skipped evidence; does not count toward closure.'
assert_contains "$OUT_MIXED" "若 Windows 为 DRY_RUN/SKIPPED/PENDING/READY：在 Windows runner 执行 live gate 并回填 summary。"

set +e
(cd /tmp && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_pass.md" \
  --macos-summary "$WORK_REL/macos_dryrun.md" \
  --windows-summary "$WORK_REL/windows_skipped.md" \
  --strict \
  --output "$WORK_REL/closure_mixed_strict.md" >/dev/null 2>&1)
EC_STRICT_MIXED=$?
set -e
[[ $EC_STRICT_MIXED -ne 0 ]] || fail "strict should fail when closure_status is IN_PROGRESS"

# Case 2: all PASS => CLOSED + strict pass
(cd /tmp && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_pass.md" \
  --macos-summary "$WORK_REL/macos_pass.md" \
  --windows-summary "$WORK_REL/windows_pass.md" \
  --strict \
  --output "$OUT_PASS_REL" >/dev/null)

OUT_PASS="$ROOT_DIR/$OUT_PASS_REL"
[[ -f "$OUT_PASS" ]] || fail "pass output report should exist"
assert_contains "$OUT_PASS" "- closure_status: **CLOSED**"
assert_contains "$OUT_PASS" "| macos | PASS | summary parsed | $WORK_REL/macos_pass.md |"
assert_contains "$OUT_PASS" "| windows | PASS | summary parsed | $WORK_REL/windows_pass.md |"

echo "[PASS] wave-b b2 closure readiness dryrun/skipped semantics contract passed"
