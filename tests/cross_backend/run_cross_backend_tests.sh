#!/bin/bash
# Cross-Backend Consistency Test Runner
# Compiles and runs all tests in tests/cross_backend/

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BIN_DIR="$PROJECT_ROOT/tmp/cross_backend_bin"

mkdir -p "$BIN_DIR"

echo "=============================================="
echo "Cross-Backend Consistency Test Suite"
echo "=============================================="
echo ""

TOTAL_PASS=0
TOTAL_FAIL=0
TESTS=()

for src in "$PROJECT_ROOT/tests/cross_backend/test_"*.pas; do
  [ -f "$src" ] || continue
  name=$(basename "$src" .pas)
  TESTS+=("$name")

  echo "[BUILD] $name"
  if ! fpc -Fu"$PROJECT_ROOT/src" \
           -Fu"$PROJECT_ROOT/tests/cross_backend" \
           -Fu"$PROJECT_ROOT/tests/framework" \
           -FE"$BIN_DIR" \
           -o"$BIN_DIR/$name" \
           "$src" > "$BIN_DIR/${name}_build.log" 2>&1; then
    echo "  [FAIL] Compilation failed"
    echo "  See: $BIN_DIR/${name}_build.log"
    TOTAL_FAIL=$((TOTAL_FAIL + 1))
    continue
  fi

  echo "[RUN]  $name"
  if "$BIN_DIR/$name" 2>&1; then
    TOTAL_PASS=$((TOTAL_PASS + 1))
  else
    TOTAL_FAIL=$((TOTAL_FAIL + 1))
  fi
  echo ""
done

echo "=============================================="
echo "Suite Summary: ${#TESTS[@]} test programs"
echo "  Passed: $TOTAL_PASS"
echo "  Failed: $TOTAL_FAIL"
echo "=============================================="

if [ "$TOTAL_FAIL" -gt 0 ]; then
  echo "[FAIL] $TOTAL_FAIL test program(s) failed"
  exit 1
else
  echo "[PASS] All cross-backend test programs passed"
  exit 0
fi
