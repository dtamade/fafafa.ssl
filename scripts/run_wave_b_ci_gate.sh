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
EXAMPLES_REPORT_REL=""
SUMMARY_OUT_REL=""
WITH_TLS13_SIGN_PURITY_CHECK=false
WITH_TLS13_SIGN_BENCH=false
FAST_LOCAL=false
REPORTS_DIR_REL=""
TLS13_SIGN_BENCH_ITERATIONS="3"
TLS13_SIGN_BENCH_WARMUP="1"
TLS13_SIGN_BENCH_SCHEME="rsa_pkcs1_sha256"
TLS13_SIGN_BENCH_KEY="tests/certificate/test_certs/signer_key.pem"
TLS13_SIGN_BENCH_TIMEOUT="120"
TLS13_SIGN_BENCH_JSON_OUT_REL=""
RUN_ID=""

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
  --fast-local                     本地快速模式：logs/summary/examples report 默认输出到 ./tmp（避免污染 git 工作区）
  --reports-dir DIR                reports 根目录（相对项目根目录）；默认产物将写入该目录
  --run-id ID                      指定 run_id（影响默认输出路径与 summary 内 Run ID 字段）
  --modules LIST                    指定模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --examples-threshold FLOAT        示例通过率阈值，默认 80.0
  --examples-report PATH            示例 JSON 输出路径（相对项目根目录）
  --summary-out PATH                Summary markdown 输出路径（相对项目根目录）
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
    --fast-local)
      FAST_LOCAL=true
      shift
      ;;
    --reports-dir)
      REPORTS_DIR_REL="$2"
      shift 2
      ;;
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --modules)
      MODULE_SET="$2"
      shift 2
      ;;
    --examples-threshold)
      EXAMPLES_THRESHOLD="$2"
      shift 2
      ;;
    --examples-report)
      EXAMPLES_REPORT_REL="$2"
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

if [[ "$FAST_LOCAL" == "false" ]]; then
  if [[ "${FAFAFA_FAST_LOCAL:-}" == "1" || "${FAFAFA_FAST_LOCAL:-}" == "true" ]]; then
    FAST_LOCAL=true
  fi
fi

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

if ! python3 - "$EXAMPLES_THRESHOLD" <<'PY' >/dev/null 2>&1
import sys
float(sys.argv[1])
PY
then
  echo "Invalid --examples-threshold: $EXAMPLES_THRESHOLD" >&2
  exit 1
fi

if [[ -n "$RUN_ID" && "$RUN_ID" =~ [^A-Za-z0-9._-] ]]; then
  echo "Invalid --run-id (allow: A-Z a-z 0-9 . _ -): $RUN_ID" >&2
  exit 1
fi

if [[ -z "$RUN_ID" ]]; then
  RUN_ID="$(date +%Y%m%d_%H%M%S)"
fi

if [[ -z "$REPORTS_DIR_REL" ]]; then
  if [[ "$FAST_LOCAL" == "true" ]]; then
    REPORTS_DIR_REL="tmp/wave_b_ci_gate_reports_${RUN_ID}"
  else
    REPORTS_DIR_REL="test-reports"
  fi
fi

if [[ "$REPORTS_DIR_REL" = /* ]]; then
  echo "Invalid --reports-dir (must be relative to project root): $REPORTS_DIR_REL" >&2
  exit 1
fi

if [[ -n "$EXAMPLES_REPORT_REL" && "$EXAMPLES_REPORT_REL" = /* ]]; then
  echo "Invalid --examples-report (must be relative to project root): $EXAMPLES_REPORT_REL" >&2
  exit 1
fi

if [[ -n "$SUMMARY_OUT_REL" && "$SUMMARY_OUT_REL" = /* ]]; then
  echo "Invalid --summary-out (must be relative to project root): $SUMMARY_OUT_REL" >&2
  exit 1
fi

resolve_rel_under_root() {
  local rel="$1"
  python3 - "$PROJECT_ROOT" "$rel" <<'PY'
import os, sys
root = os.path.abspath(sys.argv[1])
rel = sys.argv[2]
path = os.path.abspath(os.path.join(root, rel))
if path != root and not path.startswith(root + os.sep):
    raise SystemExit(2)
print(path)
PY
}

parse_examples_summary_json() {
  local report_path="$1"
  python3 - "$report_path" <<'PY'
import json
import sys

report_path = sys.argv[1]
try:
    with open(report_path, 'r', encoding='utf-8') as f:
        data = json.load(f)
    summary = data.get('summary', {})
except Exception:
    raise SystemExit(1)

print(summary.get('total', 0))
print(summary.get('passed', 0))
print(summary.get('failed', 0))
print(summary.get('skipped', 0))
print(summary.get('pass_rate', 0.0))
PY
}

REPORTS_DIR="$(resolve_rel_under_root "$REPORTS_DIR_REL" || true)"
if [[ -z "$REPORTS_DIR" ]]; then
  echo "Invalid --reports-dir (must stay within project root): $REPORTS_DIR_REL" >&2
  exit 1
fi

if [[ -z "$EXAMPLES_REPORT_REL" ]]; then
  EXAMPLES_REPORT_REL="$REPORTS_DIR_REL/examples_compile_ci_gate.json"
fi

if [[ -z "$SUMMARY_OUT_REL" ]]; then
  SUMMARY_OUT_REL="$REPORTS_DIR_REL/wave_b_ci_gate_summary_${RUN_ID}.md"
fi

EXAMPLES_REPORT="$(resolve_rel_under_root "$EXAMPLES_REPORT_REL" || true)"
if [[ -z "$EXAMPLES_REPORT" ]]; then
  echo "Invalid --examples-report (must stay within project root): $EXAMPLES_REPORT_REL" >&2
  exit 1
fi

SUMMARY_OUT="$(resolve_rel_under_root "$SUMMARY_OUT_REL" || true)"
if [[ -z "$SUMMARY_OUT" ]]; then
  echo "Invalid --summary-out (must stay within project root): $SUMMARY_OUT_REL" >&2
  exit 1
fi

COMPILE_LOG="$REPORTS_DIR/wave_b_compile_${RUN_ID}.log"
MODULE_LOG="$REPORTS_DIR/wave_b_modules_${RUN_ID}.log"
EXAMPLES_LOG="$REPORTS_DIR/wave_b_examples_${RUN_ID}.log"
PURITY_LOG="$REPORTS_DIR/wave_b_tls13_sign_purity_${RUN_ID}.log"
BENCH_LOG="$REPORTS_DIR/wave_b_tls13_sign_bench_${RUN_ID}.log"

BENCH_JSON_OUT=""
if [[ -n "$TLS13_SIGN_BENCH_JSON_OUT_REL" ]]; then
  if [[ "$TLS13_SIGN_BENCH_JSON_OUT_REL" = /* ]]; then
    echo "Invalid --tls13-sign-bench-json-out (must be relative to project root): $TLS13_SIGN_BENCH_JSON_OUT_REL" >&2
    exit 1
  fi
  BENCH_JSON_OUT="$(resolve_rel_under_root "$TLS13_SIGN_BENCH_JSON_OUT_REL" || true)"
  if [[ -z "$BENCH_JSON_OUT" ]]; then
    echo "Invalid --tls13-sign-bench-json-out (must stay within project root): $TLS13_SIGN_BENCH_JSON_OUT_REL" >&2
    exit 1
  fi
fi

mkdir -p "$REPORTS_DIR"
mkdir -p "$(dirname "$EXAMPLES_REPORT")"
mkdir -p "$(dirname "$SUMMARY_OUT")"
if [[ -n "$BENCH_JSON_OUT" ]]; then
  mkdir -p "$(dirname "$BENCH_JSON_OUT")"
fi

STEP_SHELL="/bin/bash"
if [[ -x "/usr/bin/zsh" ]]; then
  STEP_SHELL="/usr/bin/zsh"
fi

run_step() {
  local step_name="$1"
  local cmd="$2"
  local log_file="$3"

  echo "[WAVE-B] [$step_name] $cmd" >&2

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[WAVE-B] [$step_name] dry-run skip" > "$log_file"
    echo 0
    return 0
  fi

  local started ended elapsed exit_code
  started=$(date +%s)
  set +e
  "$STEP_SHELL" -lc "$cmd" > "$log_file" 2>&1
  exit_code=$?
  ended=$(date +%s)
  elapsed=$((ended - started))

  echo "[WAVE-B] [$step_name] exit=$exit_code elapsed=${elapsed}s log=$log_file" >&2
  echo "$exit_code"
}

build_module_cmd="cd '$PROJECT_ROOT' && bash scripts/run_all_module_tests.sh --modules $MODULE_SET"
if [[ "$FAST_LOCAL" == "true" ]]; then
  build_module_cmd="$build_module_cmd --fast-local"
fi
if [[ "$VERBOSE" == "true" ]]; then
  build_module_cmd="$build_module_cmd --verbose"
fi

compile_cmd="cd '$PROJECT_ROOT' && python3 scripts/compile_all_modules.py"
examples_cmd="cd '$PROJECT_ROOT' && bash scripts/verify_examples_compile.sh -f json -o '$EXAMPLES_REPORT_REL'"
purity_cmd="cd '$PROJECT_ROOT' && bash scripts/check_tls13_signer_pure_pascal.sh"
bench_cmd="cd '$PROJECT_ROOT' && FAFAFA_TLS13_SIGN_BENCH_ITERATIONS='$TLS13_SIGN_BENCH_ITERATIONS' FAFAFA_TLS13_SIGN_BENCH_WARMUP='$TLS13_SIGN_BENCH_WARMUP' FAFAFA_TLS13_SIGN_BENCH_SCHEME='$TLS13_SIGN_BENCH_SCHEME' FAFAFA_TLS13_SIGN_BENCH_KEY='$TLS13_SIGN_BENCH_KEY' FAFAFA_TLS13_SIGN_BENCH_TIMEOUT='$TLS13_SIGN_BENCH_TIMEOUT' FAFAFA_TLS13_SIGN_BENCH_JSON_OUT='$BENCH_JSON_OUT' bash scripts/run_freepascal_tls13_servercertverify_bench.sh"

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
  compile_exit=$(run_step "compile" "$compile_cmd" "$COMPILE_LOG")
  if [[ "$compile_exit" == "0" ]]; then
    compile_status="PASS"
  else
    compile_status="FAIL"
  fi
fi

if [[ "$WITH_MODULES" == "true" ]]; then
  modules_exit=$(run_step "modules" "$build_module_cmd" "$MODULE_LOG")
  if [[ "$modules_exit" == "0" ]]; then
    modules_status="PASS"
  else
    modules_status="FAIL"
  fi
fi

if [[ "$WITH_EXAMPLES" == "true" ]]; then
  examples_exit=$(run_step "examples" "$examples_cmd" "$EXAMPLES_LOG")
  if [[ "$examples_exit" == "0" ]]; then
    examples_status="PASS"
  else
    examples_status="FAIL"
  fi
fi

if [[ "$WITH_TLS13_SIGN_PURITY_CHECK" == "true" ]]; then
  purity_exit=$(run_step "tls13_sign_purity" "$purity_cmd" "$PURITY_LOG")
  if [[ "$purity_exit" == "0" ]]; then
    purity_status="PASS"
  else
    purity_status="FAIL"
  fi
fi

if [[ "$WITH_TLS13_SIGN_BENCH" == "true" ]]; then
  bench_exit=$(run_step "tls13_sign_bench" "$bench_cmd" "$BENCH_LOG")
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
  if parsed="$(parse_examples_summary_json "$EXAMPLES_REPORT" 2>/dev/null)"; then
    examples_total=$(echo "$parsed" | sed -n '1p')
    examples_passed=$(echo "$parsed" | sed -n '2p')
    examples_failed=$(echo "$parsed" | sed -n '3p')
    examples_skipped=$(echo "$parsed" | sed -n '4p')
    examples_rate=$(echo "$parsed" | sed -n '5p')
    examples_json_ok="true"
  else
    echo "[WAVE-B] [examples] invalid json report=$EXAMPLES_REPORT_REL" >&2
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
    if [[ "$threshold_pass" == "true" ]]; then
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

mode_label="live"
if [[ "$DRY_RUN" == "true" ]]; then
  mode_label="dry-run"
  if [[ "$WITH_COMPILE" == "true" ]]; then
    compile_status="DRY_RUN"
  fi
  if [[ "$WITH_MODULES" == "true" ]]; then
    modules_status="DRY_RUN"
  fi
  if [[ "$WITH_EXAMPLES" == "true" ]]; then
    examples_status="DRY_RUN"
  fi
  if [[ "$WITH_TLS13_SIGN_PURITY_CHECK" == "true" ]]; then
    purity_status="DRY_RUN"
  fi
  if [[ "$WITH_TLS13_SIGN_BENCH" == "true" ]]; then
    bench_status="DRY_RUN"
  fi
  overall_status="DRY_RUN"
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

cat > "$SUMMARY_OUT" <<EOF_SUMMARY
# Wave B Linux CI Gate Summary

- Run ID: \`$RUN_ID\`
- Generated At: \`$(date '+%Y-%m-%d %H:%M:%S %z')\`
- Mode: \`$mode_label\`
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

\`$compile_cmd\`

\`$build_module_cmd\`

\`$examples_cmd\`

\`$purity_cmd\`

\`$bench_cmd\`
EOF_SUMMARY

echo "[WAVE-B] summary: $SUMMARY_OUT"

auto_exit=1
if [[ "$overall_status" == "PASS" || "$overall_status" == "DRY_RUN" ]]; then
  auto_exit=0
fi

exit "$auto_exit"
