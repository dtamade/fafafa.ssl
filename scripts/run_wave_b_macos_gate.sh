#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="${FAFAFA_WAVE_B_MACOS_GATE_RUN_ID:-$(date +%Y%m%d_%H%M%S)}"
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
EXAMPLES_THRESHOLD="80.0"
OUTPUT_DIR_REL="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"
OPENSSL_ROOT=""
VERBOSE=false
DRY_RUN=false
PATH_CHECK_DRY_RUN=true
FPC_EXE="${FAFAFA_FPC_EXE:-fpc}"

usage() {
  cat <<'USAGE'
Wave B macOS Gate Runner

目标：
  在 macOS runner 上执行 Wave B/B2 推荐门禁链路并生成摘要。

用法：
  scripts/run_wave_b_macos_gate.sh [options]

选项：
  --run-id ID                指定 run_id
  --modules LIST             模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --examples-threshold NUM   示例通过率阈值（默认: 80.0）
  --output-dir DIR           输出目录（相对项目根，默认: tmp/wave_b_reports）
  --openssl-root DIR         指定 OpenSSL 根目录（可选）
  --verbose                  模块测试启用 --verbose
  --dry-run                  仅打印命令，不执行
  --path-check-live          path check 不使用 --dry-run
  --help                     显示帮助
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
    --examples-threshold)
      EXAMPLES_THRESHOLD="$2"
      shift 2
      ;;
    --output-dir)
      OUTPUT_DIR_REL="$2"
      shift 2
      ;;
    --openssl-root)
      OPENSSL_ROOT="$2"
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
    --path-check-live)
      PATH_CHECK_DRY_RUN=false
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

COMPILE_UNIT_OUTPUT_DIR="${FAFAFA_WAVE_B_MACOS_GATE_COMPILE_UNIT_OUTPUT_DIR:-tmp/wave_b_macos_gate_compile_units_${RUN_ID}}"
MODULE_UNIT_OUTPUT_DIR="${FAFAFA_WAVE_B_MACOS_GATE_MODULE_UNIT_OUTPUT_DIR:-tmp/wave_b_macos_gate_module_units_${RUN_ID}}"
MODULE_BIN_OUTPUT_DIR="${FAFAFA_WAVE_B_MACOS_GATE_MODULE_BIN_OUTPUT_DIR:-tmp/wave_b_macos_gate_module_bin_${RUN_ID}}"

if [[ "$OSTYPE" != darwin* && "$DRY_RUN" != "true" ]]; then
  echo "[FAIL] this script is intended for macOS (current: $OSTYPE). Use --dry-run for rehearsal." >&2
  exit 1
fi

OUTPUT_DIR="$PROJECT_ROOT/$OUTPUT_DIR_REL"
mkdir -p "$OUTPUT_DIR"

PROBE_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_probe_${RUN_ID}.log"
PROBE_JSON_REL="$OUTPUT_DIR_REL/wave_b_macos_gate_probe_${RUN_ID}.json"
PATH_CHECK_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_path_check_${RUN_ID}.log"
COMPILE_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_compile_${RUN_ID}.log"
MODULES_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_modules_${RUN_ID}.log"
EXAMPLES_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_examples_${RUN_ID}.log"
EXAMPLES_JSON_REL="$OUTPUT_DIR_REL/examples_compile_gate_macos_${RUN_ID}.json"
SUMMARY_REL="$OUTPUT_DIR_REL/wave_b_macos_gate_summary_${RUN_ID}.md"

STEP_SHELL="/bin/bash"
if [[ -x "/usr/bin/zsh" ]]; then
  STEP_SHELL="/usr/bin/zsh"
fi

ENV_PREFIX=""
if [[ -n "$OPENSSL_ROOT" ]]; then
  ENV_PREFIX="OPENSSL_ROOT='$OPENSSL_ROOT' DYLD_LIBRARY_PATH='$OPENSSL_ROOT/lib:${DYLD_LIBRARY_PATH:-}' PKG_CONFIG_PATH='$OPENSSL_ROOT/lib/pkgconfig:${PKG_CONFIG_PATH:-}' PATH='$OPENSSL_ROOT/bin:$PATH'"
fi

run_step() {
  local step_name="$1"
  local cmd="$2"
  local log_rel="$3"
  local log_abs="$PROJECT_ROOT/$log_rel"

  echo "[WAVE-B-MACOS] [$step_name] $cmd" >&2

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] $cmd" > "$log_abs"
    echo 0
    return 0
  fi

  local exit_code=0
  set +e
  "$STEP_SHELL" -lc "$cmd" > "$log_abs" 2>&1
  exit_code=$?
  set -e

  echo "[WAVE-B-MACOS] [$step_name] exit=$exit_code log=$log_rel" >&2
  echo "$exit_code"
}

path_check_flag="--dry-run"
if [[ "$PATH_CHECK_DRY_RUN" == "false" ]]; then
  path_check_flag=""
fi

probe_cmd="cd '$PROJECT_ROOT' && ${ENV_PREFIX} bash scripts/detect_macos_openssl_enhanced.sh --json > '$PROBE_JSON_REL'"
path_check_cmd="cd '$PROJECT_ROOT' && ${ENV_PREFIX} FAFAFA_FPC_EXE='$FPC_EXE' bash scripts/run_macos_openssl_path_check_draft.sh ${path_check_flag}"
compile_cmd="cd '$PROJECT_ROOT' && ${ENV_PREFIX} python3 scripts/compile_all_modules.py --unit-output-dir '$COMPILE_UNIT_OUTPUT_DIR' --fpc-exe '$FPC_EXE'"
modules_cmd="cd '$PROJECT_ROOT' && ${ENV_PREFIX} FAFAFA_FPC_EXE='$FPC_EXE' FAFAFA_FPC_UNIT_OUTPUT_DIR='$MODULE_UNIT_OUTPUT_DIR' FAFAFA_TEST_BIN_DIR='$MODULE_BIN_OUTPUT_DIR' bash scripts/run_all_module_tests.sh --modules $MODULE_SET"
examples_cmd="cd '$PROJECT_ROOT' && ${ENV_PREFIX} bash scripts/verify_examples_compile.sh -f json -o '$EXAMPLES_JSON_REL'"

if [[ "$VERBOSE" == "true" ]]; then
  modules_cmd="$modules_cmd --verbose"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] output_dir=$OUTPUT_DIR_REL"
  echo "[DRY-RUN] summary=$SUMMARY_REL"
  echo "[DRY-RUN] probe_json=$PROBE_JSON_REL"
  echo "[DRY-RUN] examples_json=$EXAMPLES_JSON_REL"
fi

probe_exit=$(run_step "probe" "$probe_cmd" "$PROBE_LOG_REL")
path_check_exit=$(run_step "path-check" "$path_check_cmd" "$PATH_CHECK_LOG_REL")
compile_exit=$(run_step "compile" "$compile_cmd" "$COMPILE_LOG_REL")
modules_exit=$(run_step "modules" "$modules_cmd" "$MODULES_LOG_REL")
examples_exit=$(run_step "examples" "$examples_cmd" "$EXAMPLES_LOG_REL")

examples_total="0"
examples_passed="0"
examples_failed="0"
examples_skipped="0"
examples_rate="0.0"

if [[ "$DRY_RUN" == "false" && -f "$PROJECT_ROOT/$EXAMPLES_JSON_REL" ]]; then
  parsed=$(python3 - "$PROJECT_ROOT/$EXAMPLES_JSON_REL" <<'PY'
import json
import sys
with open(sys.argv[1], 'r', encoding='utf-8') as f:
    d = json.load(f)
s = d.get('summary', {})
print(s.get('total', 0))
print(s.get('passed', 0))
print(s.get('failed', 0))
print(s.get('skipped', 0))
print(s.get('pass_rate', 0.0))
PY
)
  examples_total="$(echo "$parsed" | sed -n '1p')"
  examples_passed="$(echo "$parsed" | sed -n '2p')"
  examples_failed="$(echo "$parsed" | sed -n '3p')"
  examples_skipped="$(echo "$parsed" | sed -n '4p')"
  examples_rate="$(echo "$parsed" | sed -n '5p')"
fi

probe_status="FAIL"
path_check_status="FAIL"
compile_status="FAIL"
modules_status="FAIL"
examples_status="FAIL"
overall_status="FAIL"

if [[ "$DRY_RUN" == "true" ]]; then
  probe_status="DRY_RUN"
  path_check_status="DRY_RUN"
  compile_status="DRY_RUN"
  modules_status="DRY_RUN"
  examples_status="DRY_RUN"
  overall_status="DRY_RUN"
else
  [[ "$probe_exit" == "0" ]] && probe_status="PASS"
  [[ "$path_check_exit" == "0" ]] && path_check_status="PASS"
  [[ "$compile_exit" == "0" ]] && compile_status="PASS"
  [[ "$modules_exit" == "0" ]] && modules_status="PASS"

  threshold_pass=$(python3 - <<PY
rate = float("$examples_rate")
threshold = float("$EXAMPLES_THRESHOLD")
print("true" if rate >= threshold else "false")
PY
)
  if [[ "$examples_exit" == "0" && "$threshold_pass" == "true" ]]; then
    examples_status="PASS"
  fi

  if [[ "$probe_status" == "PASS" && "$path_check_status" == "PASS" && "$compile_status" == "PASS" && "$modules_status" == "PASS" && "$examples_status" == "PASS" ]]; then
    overall_status="PASS"
  fi
fi

mode_label="live"
if [[ "$DRY_RUN" == "true" ]]; then
  mode_label="dry-run"
fi

cat > "$PROJECT_ROOT/$SUMMARY_REL" <<EOF_SUMMARY
# Wave B macOS Gate Summary

- run_id: $RUN_ID
- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')
- mode: $mode_label
- overall: **$overall_status**

## Steps

| step | exit | status | evidence |
|------|------|--------|----------|
| probe | $probe_exit | $probe_status | $PROBE_JSON_REL |
| path-check | $path_check_exit | $path_check_status | $PATH_CHECK_LOG_REL |
| compile | $compile_exit | $compile_status | $COMPILE_LOG_REL |
| modules | $modules_exit | $modules_status | $MODULES_LOG_REL |
| examples | $examples_exit | $examples_status | $EXAMPLES_LOG_REL |

## Examples Metrics

- report: $EXAMPLES_JSON_REL
- threshold: $EXAMPLES_THRESHOLD
- summary: passed=$examples_passed, failed=$examples_failed, skipped=$examples_skipped, total=$examples_total, pass_rate=$examples_rate
EOF_SUMMARY

echo "[WAVE-B-MACOS] summary: $SUMMARY_REL"

if [[ "$overall_status" == "PASS" || "$overall_status" == "DRY_RUN" ]]; then
  exit 0
fi
exit 1
