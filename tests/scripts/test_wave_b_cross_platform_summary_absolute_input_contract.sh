#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_cross_platform_summary_absolute_input_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

cat > "$WORK_DIR/linux_summary.md" <<'EOF'
# Wave B CI Gate Summary

- run_id: abs_input_contract
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | ok |
| run_all_module_tests | **PASS** | ok |
| verify_examples_compile | **PASS** | ok |
EOF

cat > "$WORK_DIR/macos_summary.md" <<'EOF'
# Wave B macOS Gate Summary

- run_id: abs_input_contract
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | ok |
| modules | **PASS** | ok |
| examples | **PASS** | ok |
EOF

cat > "$WORK_DIR/windows_summary.md" <<'EOF'
# Wave B Windows Gate Summary

- run_id: abs_input_contract
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | ok |
| modules | **PASS** | ok |
| examples | **PASS** | ok |
EOF

cat > "$WORK_DIR/examples.json" <<'EOF'
{
  "summary": {
    "total": 75,
    "passed": 75,
    "failed": 0,
    "skipped": 0,
    "pass_rate": "100%"
  }
}
EOF

ABS_LINUX_SUMMARY="$(cd "$WORK_DIR" && pwd)/linux_summary.md"
ABS_MACOS_SUMMARY="$(cd "$WORK_DIR" && pwd)/macos_summary.md"
ABS_WINDOWS_SUMMARY="$(cd "$WORK_DIR" && pwd)/windows_summary.md"
ABS_EXAMPLES_JSON="$(cd "$WORK_DIR" && pwd)/examples.json"
ABS_OUTPUT_FILE="$(cd "$WORK_DIR" && pwd)/cross_platform_summary.md"

(cd /tmp && bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id abs_input_contract \
  --linux-summary "$ABS_LINUX_SUMMARY" \
  --linux-examples "$ABS_EXAMPLES_JSON" \
  --macos-summary "$ABS_MACOS_SUMMARY" \
  --windows-summary "$ABS_WINDOWS_SUMMARY" \
  --output "$ABS_OUTPUT_FILE" >/dev/null)

if [[ ! -f "$ABS_OUTPUT_FILE" ]]; then
  echo "[FAIL] cross-platform summary should be generated when absolute input paths are provided"
  exit 1
fi

if ! rg -n "\| linux \| PASS \|" "$ABS_OUTPUT_FILE" >/dev/null; then
  echo "[FAIL] linux evidence row should be parsed from absolute linux summary path"
  exit 1
fi

if ! rg -n "\| macos \| PASS \|" "$ABS_OUTPUT_FILE" >/dev/null; then
  echo "[FAIL] macOS evidence row should be parsed from absolute macOS summary path"
  exit 1
fi

if ! rg -n "\| windows \| PASS \|" "$ABS_OUTPUT_FILE" >/dev/null; then
  echo "[FAIL] windows evidence row should be parsed from absolute windows summary path"
  exit 1
fi

if ! rg -n "\| passed \| 75 \|" "$ABS_OUTPUT_FILE" >/dev/null; then
  echo "[FAIL] linux examples metrics should be parsed from absolute JSON path"
  exit 1
fi

echo "[PASS] wave-b cross-platform summary absolute input contract passed"
