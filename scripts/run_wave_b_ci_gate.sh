#!/usr/bin/env bash

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DRY_RUN=false
VERBOSE=false
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
EXAMPLES_THRESHOLD="80.0"
EXAMPLES_REPORT_REL="test-reports/examples_compile_ci_gate.json"
SUMMARY_OUT_REL=""

usage() {
  cat <<'USAGE'
Wave B Linux CI Gate Runner

目标：
  以可复用脚本串联 Linux 最小门禁：
  1) 全模块编译
  2) P2 核心模块回归
  3) 示例编译门禁（按通过率阈值判定）

用法：
  scripts/run_wave_b_ci_gate.sh [options]

选项：
  --modules LIST              指定模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --examples-threshold FLOAT  示例通过率阈值，默认 80.0
  --examples-report PATH      示例 JSON 输出路径（相对项目根目录）
  --summary-out PATH          Summary markdown 输出路径（相对项目根目录）
  --verbose                   模块测试启用 verbose
  --dry-run                   仅打印命令，不执行
  --help                      显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
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

RUN_ID="$(date +%Y%m%d_%H%M%S)"
if [[ -z "$SUMMARY_OUT_REL" ]]; then
  SUMMARY_OUT_REL="test-reports/wave_b_ci_gate_summary_${RUN_ID}.md"
fi

EXAMPLES_REPORT="$PROJECT_ROOT/$EXAMPLES_REPORT_REL"
SUMMARY_OUT="$PROJECT_ROOT/$SUMMARY_OUT_REL"
COMPILE_LOG="$PROJECT_ROOT/test-reports/wave_b_compile_${RUN_ID}.log"
MODULE_LOG="$PROJECT_ROOT/test-reports/wave_b_modules_${RUN_ID}.log"
EXAMPLES_LOG="$PROJECT_ROOT/test-reports/wave_b_examples_${RUN_ID}.log"

mkdir -p "$PROJECT_ROOT/test-reports"

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
if [[ "$VERBOSE" == "true" ]]; then
  build_module_cmd="$build_module_cmd --verbose"
fi

compile_cmd="cd '$PROJECT_ROOT' && python3 scripts/compile_all_modules.py"
examples_cmd="cd '$PROJECT_ROOT' && bash scripts/verify_examples_compile.sh -f json -o '$EXAMPLES_REPORT_REL'"

compile_exit=$(run_step "compile" "$compile_cmd" "$COMPILE_LOG")
modules_exit=$(run_step "modules" "$build_module_cmd" "$MODULE_LOG")
examples_exit=$(run_step "examples" "$examples_cmd" "$EXAMPLES_LOG")

examples_total="0"
examples_passed="0"
examples_failed="0"
examples_skipped="0"
examples_rate="0.0"
examples_json_ok="false"

if [[ "$DRY_RUN" == "false" && -f "$EXAMPLES_REPORT" ]]; then
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

compile_status="FAIL"
modules_status="FAIL"
examples_status="FAIL"

if [[ "$compile_exit" == "0" ]]; then
  compile_status="PASS"
fi

if [[ "$modules_exit" == "0" ]]; then
  modules_status="PASS"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  examples_status="PASS"
else
  threshold_pass=$(python3 - <<PY
rate = float("$examples_rate")
threshold = float("$EXAMPLES_THRESHOLD")
print("true" if rate >= threshold else "false")
PY
)

  if [[ "$examples_json_ok" == "true" && "$threshold_pass" == "true" ]]; then
    examples_status="PASS"
  fi
fi

overall_status="FAIL"
if [[ "$compile_status" == "PASS" && "$modules_status" == "PASS" && "$examples_status" == "PASS" ]]; then
  overall_status="PASS"
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
| compile_all_modules | \`$compile_exit\` | **$compile_status** | \`$(realpath --relative-to="$PROJECT_ROOT" "$COMPILE_LOG")\` |
| run_all_module_tests | \`$modules_exit\` | **$modules_status** | \`$(realpath --relative-to="$PROJECT_ROOT" "$MODULE_LOG")\` |
| verify_examples_compile | \`$examples_exit\` | **$examples_status** | \`$(realpath --relative-to="$PROJECT_ROOT" "$EXAMPLES_LOG")\` |

## Examples Gate Metrics

- Report: \`$(realpath --relative-to="$PROJECT_ROOT" "$EXAMPLES_REPORT")\`
- Threshold: \`$EXAMPLES_THRESHOLD\`
- Summary: \`passed=$examples_passed, failed=$examples_failed, skipped=$examples_skipped, total=$examples_total, pass_rate=$examples_rate\`

## Commands

\`$compile_cmd\`

\`$build_module_cmd\`

\`$examples_cmd\`
EOF_SUMMARY

echo "[WAVE-B] summary: $SUMMARY_OUT"

auto_exit=1
if [[ "$overall_status" == "PASS" ]]; then
  auto_exit=0
fi

exit "$auto_exit"
