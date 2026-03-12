#!/usr/bin/env bash

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DRY_RUN=false
VERBOSE=false
WITH_COMPILE=true
WITH_MODULES=true
WITH_EXAMPLES=true
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
EXAMPLES_THRESHOLD="80.0"
REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"
EXAMPLES_REPORT_REL=""
EXAMPLES_REPORT_EXPLICIT=false
SUMMARY_OUT_REL=""
WITH_TLS13_SIGN_PURITY_CHECK=false
WITH_TLS13_SIGN_BENCH=false
TLS13_SIGN_BENCH_ITERATIONS="3"
TLS13_SIGN_BENCH_WARMUP="1"
TLS13_SIGN_BENCH_SCHEME="rsa_pkcs1_sha256"
TLS13_SIGN_BENCH_KEY="tests/certificate/test_certs/signer_key.pem"
TLS13_SIGN_BENCH_TIMEOUT="120"
TLS13_SIGN_BENCH_JSON_OUT_REL=""
FPC_EXE="${FAFAFA_FPC_EXE:-fpc}"

usage() {
  cat <<'USAGE'
Wave B Linux CI Gate Runner

目标：
  以可复用脚本串联 Linux 最小门禁：
  1) 全模块编译
  2) P2 核心模块回归
  3) 示例编译门禁（按通过率阈值判定）
  4) （可选）TLS13 CertificateVerify signer 纯 Pascal 依赖检查
  5) （可选）TLS13 CertificateVerify signer 基准

用法：
  scripts/run_wave_b_ci_gate.sh [options]

选项：
  --modules LIST                    指定模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --reports-dir PATH                默认 reports 根目录（默认: tmp/wave_b_reports；CLI > env > default）
  --examples-threshold FLOAT        示例通过率阈值，默认 80.0
  --examples-report PATH            示例 JSON 输出路径（相对项目根目录，默认 tmp/wave_b_reports/examples_compile_ci_gate.json）
  --summary-out PATH                Summary markdown 输出路径（相对项目根目录，默认 tmp/wave_b_reports/wave_b_ci_gate_summary_<run_id>.md）
  --skip-compile                    跳过 compile_all_modules 阶段
  --skip-modules                    跳过 run_all_module_tests 阶段
  --skip-examples                   跳过 verify_examples_compile 阶段
  --with-tls13-sign-purity-check    追加运行 TLS13 signer 纯 Pascal 依赖静态检查
  --with-tls13-sign-bench           追加运行 TLS13 signer 基准
  --only-tls13-sign-bench           快速模式：仅运行 TLS13 signer 基准
  --tls13-sign-bench-iterations N   签名基准迭代次数（默认: 3）
  --tls13-sign-bench-warmup N       签名基准预热次数（默认: 1）
  --tls13-sign-bench-scheme NAME    基准算法（默认: rsa_pkcs1_sha256）
  --tls13-sign-bench-key PATH       私钥路径（默认: tests/certificate/test_certs/signer_key.pem）
  --tls13-sign-bench-timeout N      基准超时时间（秒，默认: 120）
  --tls13-sign-bench-json-out PATH  基准 JSON 输出路径（相对项目根目录，可选）
  --verbose                         模块测试启用 verbose
  --dry-run                         仅打印命令，不执行
  --help                            显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --modules)
      MODULE_SET="$2"
      shift 2
      ;;
    --reports-dir)
      REPORTS_DIR="$2"
      shift 2
      ;;
    --examples-threshold)
      EXAMPLES_THRESHOLD="$2"
      shift 2
      ;;
    --examples-report)
      EXAMPLES_REPORT_REL="$2"
      EXAMPLES_REPORT_EXPLICIT=true
      shift 2
      ;;
    --summary-out)
      SUMMARY_OUT_REL="$2"
      shift 2
      ;;
    --skip-compile)
      WITH_COMPILE=false
      shift
      ;;
    --skip-modules)
      WITH_MODULES=false
      shift
      ;;
    --skip-examples)
      WITH_EXAMPLES=false
      shift
      ;;
    --with-tls13-sign-purity-check)
      WITH_TLS13_SIGN_PURITY_CHECK=true
      shift
      ;;
    --with-tls13-sign-bench)
      WITH_TLS13_SIGN_BENCH=true
      shift
      ;;
    --only-tls13-sign-bench)
      WITH_COMPILE=false
      WITH_MODULES=false
      WITH_EXAMPLES=false
      WITH_TLS13_SIGN_BENCH=true
      shift
      ;;
    --tls13-sign-bench-iterations)
      TLS13_SIGN_BENCH_ITERATIONS="$2"
      shift 2
      ;;
    --tls13-sign-bench-warmup)
      TLS13_SIGN_BENCH_WARMUP="$2"
      shift 2
      ;;
    --tls13-sign-bench-scheme)
      TLS13_SIGN_BENCH_SCHEME="$2"
      shift 2
      ;;
    --tls13-sign-bench-key)
      TLS13_SIGN_BENCH_KEY="$2"
      shift 2
      ;;
    --tls13-sign-bench-timeout)
      TLS13_SIGN_BENCH_TIMEOUT="$2"
      shift 2
      ;;
    --tls13-sign-bench-json-out)
      TLS13_SIGN_BENCH_JSON_OUT_REL="$2"
      shift 2
      ;;
    --verbose)
      VERBOSE=true
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

if [[ ! "$TLS13_SIGN_BENCH_ITERATIONS" =~ ^[0-9]+$ ]] || [[ "$TLS13_SIGN_BENCH_ITERATIONS" -le 0 ]]; then
  echo "Invalid --tls13-sign-bench-iterations: $TLS13_SIGN_BENCH_ITERATIONS" >&2
  exit 1
fi

if [[ ! "$TLS13_SIGN_BENCH_WARMUP" =~ ^[0-9]+$ ]] || [[ "$TLS13_SIGN_BENCH_WARMUP" -lt 0 ]]; then
  echo "Invalid --tls13-sign-bench-warmup: $TLS13_SIGN_BENCH_WARMUP" >&2
  exit 1
fi

if [[ ! "$TLS13_SIGN_BENCH_TIMEOUT" =~ ^[0-9]+$ ]] || [[ "$TLS13_SIGN_BENCH_TIMEOUT" -le 0 ]]; then
  echo "Invalid --tls13-sign-bench-timeout: $TLS13_SIGN_BENCH_TIMEOUT" >&2
  exit 1
fi

RUN_ID="${FAFAFA_WAVE_B_CI_GATE_RUN_ID:-$(date +%Y%m%d_%H%M%S)}"
export FAFAFA_WAVE_B_CI_GATE_RUN_ID="$RUN_ID"
COMPILE_UNIT_OUTPUT_DIR="${FAFAFA_WAVE_B_CI_GATE_COMPILE_UNIT_OUTPUT_DIR:-tmp/wave_b_ci_gate_compile_units_${RUN_ID}}"
MODULE_UNIT_OUTPUT_DIR="${FAFAFA_WAVE_B_CI_GATE_MODULE_UNIT_OUTPUT_DIR:-tmp/wave_b_ci_gate_module_units_${RUN_ID}}"
MODULE_BIN_OUTPUT_DIR="${FAFAFA_WAVE_B_CI_GATE_MODULE_BIN_OUTPUT_DIR:-tmp/wave_b_ci_gate_module_bin_${RUN_ID}}"
if [[ -z "$EXAMPLES_REPORT_REL" ]]; then
  EXAMPLES_REPORT_REL="${FAFAFA_WAVE_B_EXAMPLES_REPORT_REL:-$REPORTS_DIR/examples_compile_ci_gate.json}"
fi
if [[ -n "${FAFAFA_WAVE_B_EXAMPLES_REPORT_REL:-}" ]]; then
  EXAMPLES_REPORT_EXPLICIT=true
fi
EXAMPLES_REPORT_RUN_SCOPED_REL="${FAFAFA_WAVE_B_EXAMPLES_REPORT_RUN_SCOPED_REL:-$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json}"
EXAMPLES_ARCHIVE_DIR_REL="${FAFAFA_WAVE_B_EXAMPLES_ARCHIVE_DIR_REL:-$REPORTS_DIR/examples-compile-history}"
EXAMPLES_ARCHIVE_REPORT_REL="${FAFAFA_WAVE_B_EXAMPLES_ARCHIVE_REPORT_REL:-$EXAMPLES_ARCHIVE_DIR_REL/examples_compile_ci_gate_${RUN_ID}.json}"
EXAMPLES_ARCHIVE_ALIAS_REL="${FAFAFA_WAVE_B_EXAMPLES_ARCHIVE_ALIAS_REL:-$EXAMPLES_ARCHIVE_DIR_REL/examples_compile_ci_gate.json}"
EXPECTED_EXAMPLES_ARCHIVE_BASENAME="examples_compile_ci_gate_${RUN_ID}.json"
ARCHIVE_REPORT_DIR_REL="$(dirname "$EXAMPLES_ARCHIVE_REPORT_REL")"
if [[ "$ARCHIVE_REPORT_DIR_REL" == "." || "$ARCHIVE_REPORT_DIR_REL" == "$EXAMPLES_ARCHIVE_REPORT_REL" ]]; then
  ARCHIVE_REPORT_DIR_REL="$EXAMPLES_ARCHIVE_DIR_REL"
fi
if [[ "$(basename "$EXAMPLES_ARCHIVE_REPORT_REL")" != "$EXPECTED_EXAMPLES_ARCHIVE_BASENAME" ]]; then
  EXAMPLES_ARCHIVE_REPORT_REL="$ARCHIVE_REPORT_DIR_REL/$EXPECTED_EXAMPLES_ARCHIVE_BASENAME"
fi
if [[ "$EXAMPLES_ARCHIVE_REPORT_REL" == "$EXAMPLES_REPORT_REL" || "$EXAMPLES_ARCHIVE_REPORT_REL" == "$EXAMPLES_REPORT_RUN_SCOPED_REL" ]]; then
  EXAMPLES_ARCHIVE_REPORT_REL="$EXAMPLES_ARCHIVE_DIR_REL/$EXPECTED_EXAMPLES_ARCHIVE_BASENAME"
fi
if [[ -z "$SUMMARY_OUT_REL" ]]; then
  SUMMARY_OUT_REL="${FAFAFA_WAVE_B_SUMMARY_OUT_REL:-$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md}"
fi
if [[ -z "$TLS13_SIGN_BENCH_JSON_OUT_REL" && "$WITH_TLS13_SIGN_BENCH" == "true" ]]; then
  TLS13_SIGN_BENCH_JSON_OUT_REL="${FAFAFA_WAVE_B_TLS13_SIGN_BENCH_JSON_OUT_REL:-$REPORTS_DIR/wave_b_tls13_signer_${RUN_ID}.json}"
fi

EXAMPLES_REPORT="$PROJECT_ROOT/$EXAMPLES_REPORT_REL"
EXAMPLES_REPORT_RUN_SCOPED="$PROJECT_ROOT/$EXAMPLES_REPORT_RUN_SCOPED_REL"
EXAMPLES_ARCHIVE_REPORT="$PROJECT_ROOT/$EXAMPLES_ARCHIVE_REPORT_REL"
EXAMPLES_ARCHIVE_ALIAS="$PROJECT_ROOT/$EXAMPLES_ARCHIVE_ALIAS_REL"
SUMMARY_OUT="$PROJECT_ROOT/$SUMMARY_OUT_REL"
COMPILE_LOG="$PROJECT_ROOT/$REPORTS_DIR/wave_b_compile_${RUN_ID}.log"
MODULE_LOG="$PROJECT_ROOT/$REPORTS_DIR/wave_b_modules_${RUN_ID}.log"
EXAMPLES_LOG="$PROJECT_ROOT/$REPORTS_DIR/wave_b_examples_${RUN_ID}.log"
PURITY_LOG="$PROJECT_ROOT/$REPORTS_DIR/wave_b_tls13_sign_purity_${RUN_ID}.log"
BENCH_LOG="$PROJECT_ROOT/$REPORTS_DIR/wave_b_tls13_sign_bench_${RUN_ID}.log"

BENCH_JSON_OUT=""
if [[ -n "$TLS13_SIGN_BENCH_JSON_OUT_REL" ]]; then
  BENCH_JSON_OUT="$PROJECT_ROOT/$TLS13_SIGN_BENCH_JSON_OUT_REL"
fi

mkdir -p "$PROJECT_ROOT/$REPORTS_DIR"
mkdir -p "$(dirname "$SUMMARY_OUT")"
mkdir -p "$(dirname "$EXAMPLES_REPORT")"
if [[ -n "$BENCH_JSON_OUT" ]]; then
  mkdir -p "$(dirname "$BENCH_JSON_OUT")"
fi

examples_history_alias_cleanup="absent"
examples_selection="current_alias"
examples_warning="none"
examples_current_alias_rel="$EXAMPLES_REPORT_REL"
examples_run_scoped_rel="$EXAMPLES_REPORT_RUN_SCOPED_REL"
examples_archive_report_rel="$EXAMPLES_ARCHIVE_REPORT_REL"
examples_history_alias_rel="$EXAMPLES_ARCHIVE_ALIAS_REL"

if [[ "$EXAMPLES_REPORT_EXPLICIT" == "true" ]]; then
  examples_selection="explicit_override"
  examples_warning="explicit override in use; verify owner run_id/path manually"
fi

format_command() {
  local formatted=""
  local quoted_arg=""
  local arg

  for arg in "$@"; do
    printf -v quoted_arg '%q' "$arg"
    if [[ -n "$formatted" ]]; then
      formatted+=" "
    fi
    formatted+="$quoted_arg"
  done

  printf '%s' "$formatted"
}

run_step() {
  local step_name="$1"
  local display_cmd="$2"
  local log_file="$3"
  shift 3

  echo "[WAVE-B] [$step_name] $display_cmd" >&2

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[WAVE-B] [$step_name] dry-run skip" > "$log_file"
    echo 0
    return 0
  fi

  local started ended elapsed exit_code
  started=$(date +%s)
  set +e
  (
    cd "$PROJECT_ROOT"
    "$@"
  ) > "$log_file" 2>&1
  exit_code=$?
  ended=$(date +%s)
  elapsed=$((ended - started))

  echo "[WAVE-B] [$step_name] exit=$exit_code elapsed=${elapsed}s log=$log_file" >&2
  echo "$exit_code"
}

compile_cmd_display="python3 scripts/compile_all_modules.py --unit-output-dir '$COMPILE_UNIT_OUTPUT_DIR' --fpc-exe '$FPC_EXE'"
examples_cmd_display="bash scripts/verify_examples_compile.sh -f json -o '$EXAMPLES_REPORT_REL'"
purity_cmd_display="bash scripts/check_tls13_signer_pure_pascal.sh"
bench_cmd_display="FAFAFA_TLS13_SIGN_BENCH_ITERATIONS='$TLS13_SIGN_BENCH_ITERATIONS' FAFAFA_TLS13_SIGN_BENCH_WARMUP='$TLS13_SIGN_BENCH_WARMUP' FAFAFA_TLS13_SIGN_BENCH_SCHEME='$TLS13_SIGN_BENCH_SCHEME' FAFAFA_TLS13_SIGN_BENCH_KEY='$TLS13_SIGN_BENCH_KEY' FAFAFA_TLS13_SIGN_BENCH_TIMEOUT='$TLS13_SIGN_BENCH_TIMEOUT' FAFAFA_TLS13_SIGN_BENCH_JSON_OUT='$BENCH_JSON_OUT' bash scripts/run_freepascal_tls13_servercertverify_bench.sh"
if [[ "$VERBOSE" == "true" ]]; then
  build_module_cmd_display="FAFAFA_FPC_EXE='$FPC_EXE' FAFAFA_FPC_UNIT_OUTPUT_DIR='$MODULE_UNIT_OUTPUT_DIR' FAFAFA_TEST_BIN_DIR='$MODULE_BIN_OUTPUT_DIR' bash scripts/run_all_module_tests.sh --modules $MODULE_SET --verbose"
else
  build_module_cmd_display="FAFAFA_FPC_EXE='$FPC_EXE' FAFAFA_FPC_UNIT_OUTPUT_DIR='$MODULE_UNIT_OUTPUT_DIR' FAFAFA_TEST_BIN_DIR='$MODULE_BIN_OUTPUT_DIR' bash scripts/run_all_module_tests.sh --modules $MODULE_SET"
fi

compile_exit="0"
modules_exit="0"
examples_exit="0"
purity_exit="0"
bench_exit="0"

compile_status="SKIP"
modules_status="SKIP"
examples_status="SKIP"
purity_status="SKIP"
bench_status="SKIP"

if [[ "$WITH_COMPILE" == "true" ]]; then
  compile_exit=$(run_step "compile" "cd '$PROJECT_ROOT' && $compile_cmd_display" "$COMPILE_LOG"     python3 scripts/compile_all_modules.py     --unit-output-dir "$COMPILE_UNIT_OUTPUT_DIR"     --fpc-exe "$FPC_EXE")
  if [[ "$compile_exit" == "0" ]]; then
    compile_status="PASS"
  else
    compile_status="FAIL"
  fi
fi

if [[ "$WITH_MODULES" == "true" ]]; then
  if [[ "$VERBOSE" == "true" ]]; then
    modules_exit=$(run_step "modules" "cd '$PROJECT_ROOT' && $build_module_cmd_display" "$MODULE_LOG"       env       "FAFAFA_FPC_EXE=$FPC_EXE"       "FAFAFA_FPC_UNIT_OUTPUT_DIR=$MODULE_UNIT_OUTPUT_DIR"       "FAFAFA_TEST_BIN_DIR=$MODULE_BIN_OUTPUT_DIR"       bash scripts/run_all_module_tests.sh       --modules "$MODULE_SET"       --verbose)
  else
    modules_exit=$(run_step "modules" "cd '$PROJECT_ROOT' && $build_module_cmd_display" "$MODULE_LOG"       env       "FAFAFA_FPC_EXE=$FPC_EXE"       "FAFAFA_FPC_UNIT_OUTPUT_DIR=$MODULE_UNIT_OUTPUT_DIR"       "FAFAFA_TEST_BIN_DIR=$MODULE_BIN_OUTPUT_DIR"       bash scripts/run_all_module_tests.sh       --modules "$MODULE_SET")
  fi
  if [[ "$modules_exit" == "0" ]]; then
    modules_status="PASS"
  else
    modules_status="FAIL"
  fi
fi

if [[ "$WITH_EXAMPLES" == "true" ]]; then
  examples_exit=$(run_step "examples" "cd '$PROJECT_ROOT' && $examples_cmd_display" "$EXAMPLES_LOG"     bash scripts/verify_examples_compile.sh     -f json     -o "$EXAMPLES_REPORT_REL")
  if [[ -f "$EXAMPLES_REPORT" ]]; then
    mkdir -p "$(dirname "$EXAMPLES_REPORT_RUN_SCOPED")"
    if [[ "$EXAMPLES_REPORT" != "$EXAMPLES_REPORT_RUN_SCOPED" ]]; then
      cp "$EXAMPLES_REPORT" "$EXAMPLES_REPORT_RUN_SCOPED"
    fi

    mkdir -p "$(dirname "$EXAMPLES_ARCHIVE_REPORT")"
    if [[ "$EXAMPLES_REPORT" != "$EXAMPLES_ARCHIVE_REPORT" ]]; then
      cp "$EXAMPLES_REPORT" "$EXAMPLES_ARCHIVE_REPORT"
    fi

    if [[ "$EXAMPLES_ARCHIVE_ALIAS" == "$EXAMPLES_REPORT" || "$EXAMPLES_ARCHIVE_ALIAS" == "$EXAMPLES_REPORT_RUN_SCOPED" || "$EXAMPLES_ARCHIVE_ALIAS" == "$EXAMPLES_ARCHIVE_REPORT" ]]; then
      examples_history_alias_cleanup="path_conflict"
    elif [[ -f "$EXAMPLES_ARCHIVE_ALIAS" ]]; then
      rm -f "$EXAMPLES_ARCHIVE_ALIAS"
      examples_history_alias_cleanup="removed"
    fi
  fi
  if [[ "$examples_exit" == "0" ]]; then
    examples_status="PASS"
  else
    examples_status="FAIL"
  fi
fi

if [[ "$WITH_TLS13_SIGN_PURITY_CHECK" == "true" ]]; then
  purity_exit=$(run_step "tls13_sign_purity" "cd '$PROJECT_ROOT' && $purity_cmd_display" "$PURITY_LOG"     bash scripts/check_tls13_signer_pure_pascal.sh)
  if [[ "$purity_exit" == "0" ]]; then
    purity_status="PASS"
  else
    purity_status="FAIL"
  fi
fi

if [[ "$WITH_TLS13_SIGN_BENCH" == "true" ]]; then
  bench_exit=$(run_step "tls13_sign_bench" "cd '$PROJECT_ROOT' && $bench_cmd_display" "$BENCH_LOG"     env     "FAFAFA_TLS13_SIGN_BENCH_ITERATIONS=$TLS13_SIGN_BENCH_ITERATIONS"     "FAFAFA_TLS13_SIGN_BENCH_WARMUP=$TLS13_SIGN_BENCH_WARMUP"     "FAFAFA_TLS13_SIGN_BENCH_SCHEME=$TLS13_SIGN_BENCH_SCHEME"     "FAFAFA_TLS13_SIGN_BENCH_KEY=$TLS13_SIGN_BENCH_KEY"     "FAFAFA_TLS13_SIGN_BENCH_TIMEOUT=$TLS13_SIGN_BENCH_TIMEOUT"     "FAFAFA_TLS13_SIGN_BENCH_JSON_OUT=$BENCH_JSON_OUT"     bash scripts/run_freepascal_tls13_servercertverify_bench.sh)
  if [[ "$bench_exit" == "0" ]]; then
    bench_status="PASS"
  else
    bench_status="FAIL"
  fi
fi

examples_total="n/a"
examples_passed="n/a"
examples_failed="n/a"
examples_skipped="n/a"
examples_rate="n/a"
examples_json_ok="false"

if [[ "$WITH_EXAMPLES" == "true" && "$DRY_RUN" == "false" && -f "$EXAMPLES_REPORT" ]]; then
  parsed=$(python3 - "$EXAMPLES_REPORT" <<'PY'
import json
import sys
p = sys.argv[1]
with open(p, 'r', encoding='utf-8') as f:
    d = json.load(f)
s = d.get('summary', {})
print(s.get('total', 0))
print(s.get('passed', 0))
print(s.get('failed', 0))
print(s.get('skipped', 0))
print(s.get('pass_rate', 0.0))
PY
)
  if [[ -n "$parsed" ]]; then
    examples_total=$(echo "$parsed" | sed -n '1p')
    examples_passed=$(echo "$parsed" | sed -n '2p')
    examples_failed=$(echo "$parsed" | sed -n '3p')
    examples_skipped=$(echo "$parsed" | sed -n '4p')
    examples_rate=$(echo "$parsed" | sed -n '5p')
    examples_json_ok="true"
  fi
fi

if [[ "$WITH_EXAMPLES" == "true" && "$DRY_RUN" == "false" ]]; then
  examples_status="FAIL"
  if [[ "$examples_json_ok" == "true" ]]; then
    threshold_pass=$(python3 - <<PY
rate = float("$examples_rate")
threshold = float("$EXAMPLES_THRESHOLD")
print("true" if rate >= threshold else "false")
PY
)
    if [[ "$examples_exit" == "0" && "$threshold_pass" == "true" ]]; then
      examples_status="PASS"
    fi
  fi
fi

bench_crt_avg="n/a"
bench_d_avg="n/a"
bench_speedup="n/a"

if [[ "$WITH_TLS13_SIGN_BENCH" == "true" && "$DRY_RUN" == "false" && -f "$BENCH_LOG" ]]; then
  bench_crt_avg=$(grep -E '^CRT_avg_ms=' "$BENCH_LOG" | tail -1 | cut -d '=' -f2 || true)
  bench_d_avg=$(grep -E '^D_avg_ms=' "$BENCH_LOG" | tail -1 | cut -d '=' -f2 || true)
  bench_speedup=$(grep -E '^Speedup_D_over_CRT=' "$BENCH_LOG" | tail -1 | cut -d '=' -f2 || true)
  bench_crt_avg="${bench_crt_avg:-n/a}"
  bench_d_avg="${bench_d_avg:-n/a}"
  bench_speedup="${bench_speedup:-n/a}"
fi

overall_status="PASS"

if [[ "$WITH_COMPILE" == "true" && "$compile_status" != "PASS" ]]; then
  overall_status="FAIL"
fi
if [[ "$WITH_MODULES" == "true" && "$modules_status" != "PASS" ]]; then
  overall_status="FAIL"
fi
if [[ "$WITH_EXAMPLES" == "true" && "$examples_status" != "PASS" ]]; then
  overall_status="FAIL"
fi
if [[ "$WITH_TLS13_SIGN_PURITY_CHECK" == "true" && "$purity_status" != "PASS" ]]; then
  overall_status="FAIL"
fi
if [[ "$WITH_TLS13_SIGN_BENCH" == "true" && "$bench_status" != "PASS" ]]; then
  overall_status="FAIL"
fi

compile_log_rel="-"
modules_log_rel="-"
examples_log_rel="-"
purity_log_rel="-"
bench_log_rel="-"

if [[ "$WITH_COMPILE" == "true" ]]; then
  compile_log_rel="$(realpath --relative-to="$PROJECT_ROOT" "$COMPILE_LOG")"
fi
if [[ "$WITH_MODULES" == "true" ]]; then
  modules_log_rel="$(realpath --relative-to="$PROJECT_ROOT" "$MODULE_LOG")"
fi
if [[ "$WITH_EXAMPLES" == "true" ]]; then
  examples_log_rel="$(realpath --relative-to="$PROJECT_ROOT" "$EXAMPLES_LOG")"
fi
if [[ "$WITH_TLS13_SIGN_PURITY_CHECK" == "true" ]]; then
  purity_log_rel="$(realpath --relative-to="$PROJECT_ROOT" "$PURITY_LOG")"
fi
if [[ "$WITH_TLS13_SIGN_BENCH" == "true" ]]; then
  bench_log_rel="$(realpath --relative-to="$PROJECT_ROOT" "$BENCH_LOG")"
fi

bench_json_line=""
if [[ -n "$BENCH_JSON_OUT" ]]; then
  bench_json_line="- JSON: \`$(realpath --relative-to="$PROJECT_ROOT" "$BENCH_JSON_OUT")\`"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] summary_out=$SUMMARY_OUT_REL"
  echo "[DRY-RUN] examples_report=$EXAMPLES_REPORT_REL"
  echo "[DRY-RUN] examples_current_alias=$examples_current_alias_rel"
  echo "[DRY-RUN] examples_run_scoped=$examples_run_scoped_rel"
  echo "[DRY-RUN] examples_archive=$examples_archive_report_rel"
  echo "[DRY-RUN] examples_selection=$examples_selection"
  echo "[DRY-RUN] examples_warning=$examples_warning"
fi

cat > "$SUMMARY_OUT" <<EOF_SUMMARY
# Wave B Linux CI Gate Summary

- Run ID: \`$RUN_ID\`
- Generated At: \`$(date '+%Y-%m-%d %H:%M:%S %z')\`
- Project Root: \`$PROJECT_ROOT\`
- Overall Status: **$overall_status**

## Gate Steps

| Step | Exit Code | Status | Log |
|------|-----------|--------|-----|
| compile_all_modules | \`$compile_exit\` | **$compile_status** | \`$compile_log_rel\` |
| run_all_module_tests | \`$modules_exit\` | **$modules_status** | \`$modules_log_rel\` |
| verify_examples_compile | \`$examples_exit\` | **$examples_status** | \`$examples_log_rel\` |
| tls13_signer_purity | \`$purity_exit\` | **$purity_status** | \`$purity_log_rel\` |
| tls13_servercertverify_bench | \`$bench_exit\` | **$bench_status** | \`$bench_log_rel\` |

## Examples Gate Metrics

- Report: \`$(realpath --relative-to="$PROJECT_ROOT" "$EXAMPLES_REPORT" 2>/dev/null || echo "$EXAMPLES_REPORT_REL")\`
- Selection: \`$examples_selection\`
- Current Alias: \`$examples_current_alias_rel\`
- Alias Owner Run ID: \`$RUN_ID\`
- Run-Scoped Copy: \`$examples_run_scoped_rel\`
- Archive Copy: \`$examples_archive_report_rel\`
- History Alias Path: \`$examples_history_alias_rel\`
- History Alias Cleanup: \`$examples_history_alias_cleanup\`
- Warning: \`$examples_warning\`
- Threshold: \`$EXAMPLES_THRESHOLD\`
- Summary: \`passed=$examples_passed, failed=$examples_failed, skipped=$examples_skipped, total=$examples_total, pass_rate=$examples_rate\`

## TLS13 Signer Bench Metrics

- Scheme: \`$TLS13_SIGN_BENCH_SCHEME\`
- Iterations: \`$TLS13_SIGN_BENCH_ITERATIONS\`
- Warmup: \`$TLS13_SIGN_BENCH_WARMUP\`
- Timeout: \`$TLS13_SIGN_BENCH_TIMEOUT\`
- Key: \`$TLS13_SIGN_BENCH_KEY\`
- CRT_avg_ms: \`$bench_crt_avg\`
- D_avg_ms: \`$bench_d_avg\`
- Speedup_D_over_CRT: \`$bench_speedup\`
$bench_json_line

## Commands

\`$compile_cmd_display\`

\`$build_module_cmd_display\`

\`$examples_cmd_display\`

\`$purity_cmd_display\`

\`$bench_cmd_display\`
EOF_SUMMARY

echo "[WAVE-B] summary: $SUMMARY_OUT"

auto_exit=1
if [[ "$overall_status" == "PASS" ]]; then
  auto_exit=0
fi

exit "$auto_exit"
