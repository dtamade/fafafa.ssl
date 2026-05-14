#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_macos_gate_examples_threshold_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="wave_b_macos_examples_threshold_truth"
OUTPUT_DIR_REL="$WORK_REL/out"
SUMMARY_ABS="$WORK_DIR/$OUTPUT_DIR_REL/wave_b_macos_gate_summary_${RUN_ID}.md"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$WORK_DIR/scripts"
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_b_macos_gate.sh" "$WORK_DIR/scripts/"

cat > "$WORK_DIR/scripts/detect_macos_openssl_enhanced.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
echo '{"status":"ok"}'
EOF

cat > "$WORK_DIR/scripts/run_macos_openssl_path_check_draft.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
exit 0
EOF

cat > "$WORK_DIR/scripts/compile_all_modules.py" <<'EOF'
#!/usr/bin/env python3
print("compile ok")
EOF

cat > "$WORK_DIR/scripts/run_all_module_tests.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
exit 0
EOF

cat > "$WORK_DIR/scripts/verify_examples_compile.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

out=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--output) out="$2"; shift 2 ;;
    *) shift ;;
  esac
done

cat > "$out" <<'JSON'
{
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 2,
    "skipped": 2,
    "pass_rate": 94.7
  }
}
JSON

exit 1
EOF

chmod +x "$WORK_DIR/scripts/"*.sh "$WORK_DIR/scripts/compile_all_modules.py"

set +e
(
  cd "$WORK_DIR"
  OSTYPE=darwin23 bash scripts/run_wave_b_macos_gate.sh \
    --run-id "$RUN_ID" \
    --output-dir "$OUTPUT_DIR_REL" >/dev/null
)
exit_code=$?
set -e

if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "expected macOS gate summary to be generated"
fi

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave b macOS gate should stay green when examples pass_rate meets threshold even if helper reports failed files"
fi

if ! rg -n "^\\| examples \\| 1 \\| PASS \\|" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b macOS summary should keep examples step PASS when pass_rate meets threshold"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b macOS summary should keep overall PASS when examples pass_rate meets threshold"
fi

if ! rg -n "summary: passed=71, failed=2, skipped=2, total=75, pass_rate=94.7" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b macOS summary should still expose helper evidence alongside threshold-based PASS"
fi

echo "[PASS] wave_b_macos_gate examples threshold contract passed"
