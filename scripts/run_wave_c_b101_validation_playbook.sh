#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
FULL_GATE=false
STRICT=false
DRY_RUN=false
FAST_LOCAL=false
OUTPUT_FILE=""
REPORTS_DIR=""
BENCH_BIN_DIR=""
COMPILE_UNIT_OUTPUT_DIR=""
MODULE_REPORTS_DIR=""
MODULE_BIN_DIR=""
MODULE_UNIT_DIR=""

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
  --fast-local        本地快速模式：日志/报告/benchmark 产物输出到 ./tmp
  --reports-dir DIR   日志与报告根目录（默认: test-reports；fast-local 下默认: tmp/test-reports）
  --bench-bin-dir DIR benchmark 编译产物目录（默认: tests/benchmarks/bin；fast-local 下默认: tmp/wave_c_b101_bench_bin_<run_id>）
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
    --fast-local)
      FAST_LOCAL=true
      shift
      ;;
    --reports-dir)
      REPORTS_DIR="$2"
      shift 2
      ;;
    --bench-bin-dir)
      BENCH_BIN_DIR="$2"
      shift 2
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

if [[ "$FAST_LOCAL" == "false" ]]; then
  if [[ "${FAFAFA_FAST_LOCAL:-}" == "1" || "${FAFAFA_FAST_LOCAL:-}" == "true" ]]; then
    FAST_LOCAL=true
  fi
fi

resolve_under_project_root() {
  local path="$1"
  if [[ "$path" != /* ]]; then
    path="$PROJECT_ROOT/$path"
  fi
  if [[ "$path" != "$PROJECT_ROOT"/* ]]; then
    echo "[FAIL] refusing to write outside project root: $path" >&2
    exit 1
  fi
  echo "$path"
}

if [[ -z "$REPORTS_DIR" ]]; then
  if [[ "$FAST_LOCAL" == "true" ]]; then
    REPORTS_DIR="tmp/test-reports"
  else
    REPORTS_DIR="test-reports"
  fi
fi

if [[ -z "$BENCH_BIN_DIR" ]]; then
  if [[ "$FAST_LOCAL" == "true" ]]; then
    BENCH_BIN_DIR="tmp/wave_c_b101_bench_bin_${RUN_ID}"
  else
    BENCH_BIN_DIR="tests/benchmarks/bin"
  fi
fi

if [[ "$FAST_LOCAL" == "true" ]]; then
  COMPILE_UNIT_OUTPUT_DIR="tmp/wave_c_b101_compile_units_${RUN_ID}"
  MODULE_REPORTS_DIR="tmp/wave_c_b101_module_reports_${RUN_ID}"
  MODULE_BIN_DIR="tmp/wave_c_b101_module_bin_${RUN_ID}"
  MODULE_UNIT_DIR="tmp/wave_c_b101_module_units_${RUN_ID}"
fi

REPORTS_DIR="$(resolve_under_project_root "$REPORTS_DIR")"
BENCH_BIN_DIR="$(resolve_under_project_root "$BENCH_BIN_DIR")"
if [[ -n "$COMPILE_UNIT_OUTPUT_DIR" ]]; then
  COMPILE_UNIT_OUTPUT_DIR="$(resolve_under_project_root "$COMPILE_UNIT_OUTPUT_DIR")"
fi
if [[ -n "$MODULE_REPORTS_DIR" ]]; then
  MODULE_REPORTS_DIR="$(resolve_under_project_root "$MODULE_REPORTS_DIR")"
fi
if [[ -n "$MODULE_BIN_DIR" ]]; then
  MODULE_BIN_DIR="$(resolve_under_project_root "$MODULE_BIN_DIR")"
fi
if [[ -n "$MODULE_UNIT_DIR" ]]; then
  MODULE_UNIT_DIR="$(resolve_under_project_root "$MODULE_UNIT_DIR")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b101_validation_${RUN_ID}.md"
fi

OUTPUT_FILE="$(resolve_under_project_root "$OUTPUT_FILE")"

COMPILE_LOG="$REPORTS_DIR/wave_c_b101_compile_${RUN_ID}.log"
MODULE_LOG="$REPORTS_DIR/wave_c_b101_modules_${RUN_ID}.log"
BENCH_COMPILE_LOG="$REPORTS_DIR/wave_c_b101_bench_compile_${RUN_ID}.log"
BENCH_RUN_LOG="$REPORTS_DIR/wave_c_b101_bench_run_${RUN_ID}.log"

shell_join() {
  local parts=()
  local part
  for part in "$@"; do
    parts+=("$(printf '%q' "$part")")
  done
  local IFS=' '
  echo "${parts[*]}"
}

run_step() {
  local label="$1"
  local log="$2"
  local cmd_desc="$3"
  shift 3

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] $label => $cmd_desc"
    echo 0
    return 0
  fi

  set +e
  ( cd "$PROJECT_ROOT" && "$@" ) > "$log" 2>&1
  local ec=$?
  set -e
  echo "$ec"
}

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[INFO] run_id: $RUN_ID"
  echo "[INFO] reports_dir: $REPORTS_DIR"
  echo "[INFO] output_file: $OUTPUT_FILE"
  echo "[INFO] bench_bin_dir: $BENCH_BIN_DIR"
  if [[ "$FAST_LOCAL" == "true" ]]; then
    echo "[INFO] compile_unit_output_dir: $COMPILE_UNIT_OUTPUT_DIR"
    echo "[INFO] module_reports_dir: $MODULE_REPORTS_DIR"
    echo "[INFO] module_bin_dir: $MODULE_BIN_DIR"
    echo "[INFO] module_unit_dir: $MODULE_UNIT_DIR"
  fi
fi

compile_exit=0
modules_exit=0
bench_compile_exit=0
bench_run_exit=0

if [[ "$DRY_RUN" == "false" ]]; then
  mkdir -p "$REPORTS_DIR"
fi

if [[ "$FULL_GATE" == "true" ]]; then
  compile_cmd_words=(
    python3
    scripts/compile_all_modules.py
  )
  if [[ -n "$COMPILE_UNIT_OUTPUT_DIR" ]]; then
    compile_cmd_words+=(
      --unit-output-dir "$COMPILE_UNIT_OUTPUT_DIR"
    )
  fi
  compile_exit=$(run_step "compile_all_modules" "$COMPILE_LOG" "$(shell_join "${compile_cmd_words[@]}")" "${compile_cmd_words[@]}")

  modules_cmd_words=()
  if [[ "$FAST_LOCAL" == "true" ]]; then
    modules_cmd_words+=(
      env
      "FAFAFA_MODULE_TEST_REPORTS_DIR=$MODULE_REPORTS_DIR"
      "FAFAFA_MODULE_TEST_BIN_DIR=$MODULE_BIN_DIR"
      "FAFAFA_FPC_UNIT_OUTPUT_DIR=$MODULE_UNIT_DIR"
    )
  fi
  modules_cmd_words+=(
    bash
    scripts/run_all_module_tests.sh
    --modules "$MODULE_SET"
  )
  if [[ "$FAST_LOCAL" == "true" ]]; then
    modules_cmd_words+=(--fast-local)
  fi
  modules_exit=$(run_step "run_all_module_tests" "$MODULE_LOG" "$(shell_join "${modules_cmd_words[@]}")" "${modules_cmd_words[@]}")
fi

bench_compile_cmd_words=(
  fpc
  -Mobjfpc
  -Sh
  -O2
  -Fu./src
  -Fu./src/openssl
  -Fu./tests/benchmarks
  -Fu./examples
  -Fi./src
  "-FE$BENCH_BIN_DIR"
  tests/benchmarks/benchmark_cert_verify_cache.pas
)
if [[ "$DRY_RUN" == "false" ]]; then
  mkdir -p "$BENCH_BIN_DIR"
fi
bench_compile_exit=$(run_step "compile_benchmark_cert_cache" "$BENCH_COMPILE_LOG" "$(shell_join "${bench_compile_cmd_words[@]}")" "${bench_compile_cmd_words[@]}")

if [[ "$bench_compile_exit" == "0" ]]; then
  bench_run_cmd_words=(
    env
    "FAFAFA_PROJECT_ROOT=$PROJECT_ROOT"
    "$BENCH_BIN_DIR/benchmark_cert_verify_cache"
  )
  bench_run_exit=$(run_step "run_benchmark_cert_cache" "$BENCH_RUN_LOG" "$(shell_join "${bench_run_cmd_words[@]}")" "${bench_run_cmd_words[@]}")
else
  bench_run_exit=127
fi

speedup="n/a"
hit_rate="n/a"
if [[ "$DRY_RUN" == "false" && "$bench_run_exit" == "0" && -f "$BENCH_RUN_LOG" ]]; then
  speedup=$(grep -E 'Speedup Factor:' "$BENCH_RUN_LOG" | head -1 | sed -E 's/.*Speedup Factor:[[:space:]]*//' | sed -E 's/[xX]//g' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)
  hit_rate=$(grep -E 'Hit Rate:' "$BENCH_RUN_LOG" | head -1 | sed -E 's/.*Hit Rate:[[:space:]]*//' | sed -E 's/%//g' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)
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
    echo "- fast_local: $FAST_LOCAL"
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
  } > "$OUTPUT_FILE"

  echo "[PASS] B101 validation report generated: $OUTPUT_FILE"
fi

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
