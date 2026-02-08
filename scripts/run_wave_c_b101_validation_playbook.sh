#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
FULL_GATE=false
STRICT=false
DRY_RUN=false
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B101 Validation Playbook

用途：
  在执行 P1 候选接入前后，复用统一命令验证“回归门禁 + benchmark 证据”。

用法：
  scripts/run_wave_c_b101_validation_playbook.sh [options]

选项：
  --run-id ID         指定 run_id（默认时间戳）
  --modules LIST      模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --full-gate         执行 compile_all_modules + run_all_module_tests
  --output FILE       输出报告（默认 test-reports/wave_c_b101_validation_<run_id>.md）
  --strict            任一步骤失败返回非 0
  --dry-run           仅打印步骤，不执行
  --help              显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --modules)
      MODULE_SET="$2"
      shift 2
      ;;
    --full-gate)
      FULL_GATE=true
      shift
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --strict)
      STRICT=true
      shift
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="test-reports/wave_c_b101_validation_${RUN_ID}.md"
fi

COMPILE_LOG="test-reports/wave_c_b101_compile_${RUN_ID}.log"
MODULE_LOG="test-reports/wave_c_b101_modules_${RUN_ID}.log"
BENCH_COMPILE_LOG="test-reports/wave_c_b101_bench_compile_${RUN_ID}.log"
BENCH_RUN_LOG="test-reports/wave_c_b101_bench_run_${RUN_ID}.log"

run_step() {
  local label="$1"
  local cmd="$2"
  local log="$3"

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] $label => $cmd"
    echo 0
    return 0
  fi

  set +e
  ( cd "$PROJECT_ROOT" && eval "$cmd" ) > "$PROJECT_ROOT/$log" 2>&1
  local ec=$?
  set -e
  echo "$ec"
}

compile_exit=0
modules_exit=0
bench_compile_exit=0
bench_run_exit=0

if [[ "$FULL_GATE" == "true" ]]; then
  compile_exit=$(run_step "compile_all_modules" "python3 scripts/compile_all_modules.py" "$COMPILE_LOG")
  modules_exit=$(run_step "run_all_module_tests" "bash scripts/run_all_module_tests.sh --modules $MODULE_SET" "$MODULE_LOG")
fi

bench_compile_exit=$(run_step "compile_benchmark_cert_cache" "mkdir -p tests/benchmarks/bin && fpc -Mobjfpc -Sh -O2 -Fu./src -Fu./src/openssl -Fu./tests/benchmarks -Fu./examples -Fi./src -FE./tests/benchmarks/bin tests/benchmarks/benchmark_cert_verify_cache.pas" "$BENCH_COMPILE_LOG")

if [[ "$bench_compile_exit" == "0" ]]; then
  bench_run_exit=$(run_step "run_benchmark_cert_cache" "./tests/benchmarks/bin/benchmark_cert_verify_cache" "$BENCH_RUN_LOG")
else
  bench_run_exit=127
fi

speedup="n/a"
hit_rate="n/a"
if [[ "$DRY_RUN" == "false" && "$bench_run_exit" == "0" && -f "$PROJECT_ROOT/$BENCH_RUN_LOG" ]]; then
  speedup=$(grep -E 'Speedup Factor:' "$PROJECT_ROOT/$BENCH_RUN_LOG" | head -1 | sed -E 's/.*Speedup Factor:[[:space:]]*//' | sed -E 's/[xX]//g' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)
  hit_rate=$(grep -E 'Hit Rate:' "$PROJECT_ROOT/$BENCH_RUN_LOG" | head -1 | sed -E 's/.*Hit Rate:[[:space:]]*//' | sed -E 's/%//g' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)
fi

overall="PASS"
if [[ "$FULL_GATE" == "true" && ( "$compile_exit" != "0" || "$modules_exit" != "0" ) ]]; then
  overall="FAIL"
fi
if [[ "$bench_compile_exit" != "0" || "$bench_run_exit" != "0" ]]; then
  overall="FAIL"
fi

if [[ "$DRY_RUN" == "false" ]]; then
  {
    echo "# Wave C B101 Validation Playbook Report"
    echo
    echo "- run_id: $RUN_ID"
    echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
    echo "- full_gate: $FULL_GATE"
    echo "- overall: **$overall**"
    echo
    echo "## Step Results"
    echo
    echo "| step | exit | log |"
    echo "|------|------|-----|"
    if [[ "$FULL_GATE" == "true" ]]; then
      echo "| compile_all_modules | $compile_exit | $COMPILE_LOG |"
      echo "| run_all_module_tests | $modules_exit | $MODULE_LOG |"
    fi
    echo "| compile_benchmark_cert_cache | $bench_compile_exit | $BENCH_COMPILE_LOG |"
    echo "| run_benchmark_cert_cache | $bench_run_exit | $BENCH_RUN_LOG |"
    echo
    echo "## Benchmark Snapshot"
    echo
    echo "- hit_rate_percent: $hit_rate"
    echo "- speedup_factor_x: $speedup"
  } > "$PROJECT_ROOT/$OUTPUT_FILE"

  echo "[PASS] B101 validation report generated: $OUTPUT_FILE"
fi

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
