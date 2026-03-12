#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh"
WORK_REL="tmp/test_wave_b_b2_handoff_closure_semantics_snapshot"
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
    sed -n '1,320p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b b2 handoff bundle closure semantics snapshot"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

RUN_ID="wb2_handoff_semantics_$$"
OUT_DIR_REL="$WORK_REL/out"
CLOSURE_REL="$OUT_DIR_REL/wave_b_b2_closure_readiness_${RUN_ID}.md"

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | 157/157 |
| run_all_module_tests | **PASS** | 100% |
| verify_examples_compile | **PASS** | 71/75 |
EOF

cat > "$WORK_DIR/examples.json" <<'EOF'
{
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
EOF

cd "$ROOT_DIR"
bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --output-dir "$OUT_DIR_REL" >/dev/null

BUNDLE_FILE="$ROOT_DIR/$OUT_DIR_REL/wave_b_b2_handoff_bundle_${RUN_ID}.md"
[[ -f "$BUNDLE_FILE" ]] || fail "handoff bundle should be generated"

assert_contains "$BUNDLE_FILE" "## Closure Semantics Snapshot"
assert_contains "$BUNDLE_FILE" "- source: $CLOSURE_REL"
assert_contains "$BUNDLE_FILE" "- \`PASS\`: live gate passed and counts toward closure."
assert_contains "$BUNDLE_FILE" "- \`DRY_RUN\`: rehearsal evidence exists but does not count toward closure."
assert_contains "$BUNDLE_FILE" "- \`SKIPPED\`: intentionally skipped evidence; does not count toward closure."

echo "[PASS] wave-b b2 handoff bundle closure semantics snapshot passed"
