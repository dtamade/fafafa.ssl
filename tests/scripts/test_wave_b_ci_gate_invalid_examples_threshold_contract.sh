#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

SANDBOX_ROOT="$(mktemp -d "$PROJECT_ROOT/tmp/test_wave_b_invalid_threshold_XXXXXX")"
trap 'rm -rf "$SANDBOX_ROOT"' EXIT

mkdir -p "$SANDBOX_ROOT/scripts" "$SANDBOX_ROOT/test-reports"
cp "$PROJECT_ROOT/scripts/run_wave_b_ci_gate.sh" "$SANDBOX_ROOT/scripts/"

cat > "$SANDBOX_ROOT/scripts/compile_all_modules.py" <<'EOF_COMPILE'
#!/usr/bin/env python3
from pathlib import Path
Path("compile_ran.marker").write_text("ran", encoding="utf-8")
print("compile ok")
EOF_COMPILE

cat > "$SANDBOX_ROOT/scripts/run_all_module_tests.sh" <<'EOF_MODULES'
#!/usr/bin/env bash
set -euo pipefail
printf 'ran' > modules_ran.marker
exit 0
EOF_MODULES

cat > "$SANDBOX_ROOT/scripts/verify_examples_compile.sh" <<'EOF_EXAMPLES'
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
EOF_EXAMPLES

chmod +x \
  "$SANDBOX_ROOT/scripts/run_wave_b_ci_gate.sh" \
  "$SANDBOX_ROOT/scripts/compile_all_modules.py" \
  "$SANDBOX_ROOT/scripts/run_all_module_tests.sh" \
  "$SANDBOX_ROOT/scripts/verify_examples_compile.sh"

STDOUT_LOG="$SANDBOX_ROOT/stdout.log"
STDERR_LOG="$SANDBOX_ROOT/stderr.log"

set +e
(
  cd "$SANDBOX_ROOT"
  bash scripts/run_wave_b_ci_gate.sh --examples-threshold nope >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
EXIT_CODE=$?
set -e

if [[ "$EXIT_CODE" -eq 0 ]]; then
  echo "[FAIL] wave b linux gate should reject an invalid examples threshold before running any gate step"
  exit 1
fi

if ! rg -n "invalid.*examples-threshold|Invalid --examples-threshold|examples threshold" "$STDERR_LOG" >/dev/null; then
  echo "[FAIL] wave b linux gate should explain that the examples threshold is invalid"
  echo "[TRACE] stderr:"
  sed -n '1,120p' "$STDERR_LOG"
  exit 1
fi

for marker in compile_ran.marker modules_ran.marker examples_ran.marker; do
  if [[ -e "$SANDBOX_ROOT/$marker" ]]; then
    echo "[FAIL] wave b linux gate should fail before running steps when examples threshold is invalid"
    exit 1
  fi
done

if compgen -G "$SANDBOX_ROOT/test-reports/*.md" >/dev/null; then
  echo "[FAIL] wave b linux gate should not emit a summary for an invalid examples threshold"
  exit 1
fi

echo "[PASS] wave b linux gate invalid examples threshold contract passed"
