#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_macos_invalid_run_id_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
BAD_RUN_ID="bad'quote"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

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
printf 'ran' > probe_ran.marker
echo '{"status":"ok"}'
EOF

cat > "$WORK_DIR/scripts/run_macos_openssl_path_check_draft.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf 'ran' > path_check_ran.marker
exit 0
EOF

cat > "$WORK_DIR/scripts/compile_all_modules.py" <<'EOF'
#!/usr/bin/env python3
from pathlib import Path
Path("compile_ran.marker").write_text("ran", encoding="utf-8")
print("compile ok")
EOF

cat > "$WORK_DIR/scripts/run_all_module_tests.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf 'ran' > modules_ran.marker
exit 0
EOF

cat > "$WORK_DIR/scripts/verify_examples_compile.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf 'ran' > examples_ran.marker
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
    "total": 1,
    "passed": 1,
    "failed": 0,
    "skipped": 0,
    "pass_rate": 100.0
  }
}
JSON
exit 0
EOF

chmod +x "$WORK_DIR/scripts/"*.sh "$WORK_DIR/scripts/compile_all_modules.py"

set +e
(
  cd "$WORK_DIR"
  OSTYPE=darwin23 bash scripts/run_wave_b_macos_gate.sh \
    --run-id "$BAD_RUN_ID" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -eq 0 ]]; then
  fail "wave b macOS gate should reject an invalid run-id before running any gate step"
fi

if ! rg -n "Invalid --run-id|invalid.*run-id|run-id" "$STDERR_LOG" >/dev/null; then
  fail "wave b macOS gate should explain that the run-id is invalid"
fi

for marker in probe_ran.marker path_check_ran.marker compile_ran.marker modules_ran.marker examples_ran.marker; do
  if [[ -e "$WORK_DIR/$marker" ]]; then
    fail "wave b macOS gate should fail before running steps when run-id is invalid"
  fi
done

if [[ -d "$WORK_DIR/test-reports" ]] && find "$WORK_DIR/test-reports" -mindepth 1 -print -quit | grep -q .; then
  fail "wave b macOS gate should not emit any report files for an invalid run-id"
fi

echo "[PASS] wave b macOS gate invalid run-id contract passed"
