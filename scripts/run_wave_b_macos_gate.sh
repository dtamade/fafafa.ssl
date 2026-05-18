#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID=""
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
EXAMPLES_THRESHOLD="80.0"
OUTPUT_DIR_REL="test-reports"
OPENSSL_ROOT=""
VERBOSE=false
DRY_RUN=false
PATH_CHECK_DRY_RUN=true

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
  --output-dir DIR           输出目录（相对项目根，默认: test-reports）
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

if [[ "$OUTPUT_DIR_REL" = /* ]]; then
  echo "Invalid --output-dir (must be relative to project root): $OUTPUT_DIR_REL" >&2
  exit 1
fi

if [[ "$OSTYPE" != darwin* && "$DRY_RUN" != "true" ]]; then
  echo "[FAIL] this script is intended for macOS (current: $OSTYPE). Use --dry-run for rehearsal." >&2
  exit 1
fi

resolve_rel_under_root() {
  local rel="$1"
  python3 - "$PROJECT_ROOT" "$rel" <<'PY'
import os
import sys

root = os.path.abspath(sys.argv[1])
rel = sys.argv[2]
path = os.path.abspath(os.path.join(root, rel))
if path != root and not path.startswith(root + os.sep):
    raise SystemExit(2)
print(path)
PY
}

shell_join() {
  local parts=()
  local part
  for part in "$@"; do
    parts+=("$(printf '%q' "$part")")
  done
  local IFS=' '
  echo "${parts[*]}"
}

build_step_command() {
  local words=("$@")
  echo "cd $(printf '%q' "$PROJECT_ROOT") && $(shell_join "${words[@]}")"
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

OUTPUT_DIR="$(resolve_rel_under_root "$OUTPUT_DIR_REL" || true)"
if [[ -z "$OUTPUT_DIR" ]]; then
  echo "Invalid --output-dir (must stay within project root): $OUTPUT_DIR_REL" >&2
  exit 1
fi

mkdir -p "$OUTPUT_DIR"

PROBE_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_probe_${RUN_ID}.log"
PROBE_JSON_REL="$OUTPUT_DIR_REL/wave_b_macos_gate_probe_${RUN_ID}.json"
LOADER_SYMBOL_PROBE_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_loader_symbol_probe_${RUN_ID}.log"
LOADER_SYMBOL_PROBE_JSON_REL="$OUTPUT_DIR_REL/wave_b_macos_loader_symbol_probe_${RUN_ID}.json"
PATH_CHECK_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_path_check_${RUN_ID}.log"
COMPILE_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_compile_${RUN_ID}.log"
MODULES_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_modules_${RUN_ID}.log"
EXAMPLES_LOG_REL="$OUTPUT_DIR_REL/wave_b_macos_examples_${RUN_ID}.log"
EXAMPLES_JSON_REL="$OUTPUT_DIR_REL/examples_compile_gate_macos_${RUN_ID}.json"
SUMMARY_REL="$OUTPUT_DIR_REL/wave_b_macos_gate_summary_${RUN_ID}.md"

step_env_assignments=()
if [[ -n "$OPENSSL_ROOT" ]]; then
  step_env_assignments+=(
    "OPENSSL_ROOT=$OPENSSL_ROOT"
    "DYLD_LIBRARY_PATH=$OPENSSL_ROOT/lib:${DYLD_LIBRARY_PATH:-}"
    "PKG_CONFIG_PATH=$OPENSSL_ROOT/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
    "PATH=$OPENSSL_ROOT/bin:$PATH"
  )
fi

run_step() {
  local step_name="$1"
  local cmd_desc="$2"
  local log_rel="$3"
  local stdout_rel="$4"
  shift 4
  local log_abs="$PROJECT_ROOT/$log_rel"
  local stdout_abs=""

  if [[ -n "$stdout_rel" ]]; then
    stdout_abs="$PROJECT_ROOT/$stdout_rel"
  fi

  echo "[WAVE-B-MACOS] [$step_name] $cmd_desc" >&2

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] $cmd_desc" > "$log_abs"
    echo 0
    return 0
  fi

  local exit_code=0
  set +e
  if [[ -n "$stdout_abs" ]]; then
    (
      cd "$PROJECT_ROOT"
      "$@"
    ) > "$stdout_abs" 2> "$log_abs"
  else
    (
      cd "$PROJECT_ROOT"
      "$@"
    ) > "$log_abs" 2>&1
  fi
  exit_code=$?
  set -e

  echo "[WAVE-B-MACOS] [$step_name] exit=$exit_code log=$log_rel" >&2
  echo "$exit_code"
}

if [[ ${#step_env_assignments[@]} -gt 0 ]]; then
  probe_words=(env "${step_env_assignments[@]}" bash scripts/detect_macos_openssl_enhanced.sh --json)
else
  probe_words=(bash scripts/detect_macos_openssl_enhanced.sh --json)
fi
probe_cmd="$(build_step_command "${probe_words[@]}") > $(printf '%q' "$PROBE_JSON_REL")"

if [[ ${#step_env_assignments[@]} -gt 0 ]]; then
  loader_symbol_probe_words=(
    env
    "${step_env_assignments[@]}"
    bash scripts/run_macos_openssl_loader_symbol_probe.sh
    --run-id "$RUN_ID"
    --output "$LOADER_SYMBOL_PROBE_JSON_REL"
  )
else
  loader_symbol_probe_words=(
    bash scripts/run_macos_openssl_loader_symbol_probe.sh
    --run-id "$RUN_ID"
    --output "$LOADER_SYMBOL_PROBE_JSON_REL"
  )
fi
if [[ -n "$OPENSSL_ROOT" ]]; then
  loader_symbol_probe_words+=(--openssl-root "$OPENSSL_ROOT")
fi
loader_symbol_probe_cmd="$(build_step_command "${loader_symbol_probe_words[@]}")"

if [[ ${#step_env_assignments[@]} -gt 0 ]]; then
  path_check_words=(env "${step_env_assignments[@]}" bash scripts/run_macos_openssl_path_check_draft.sh)
else
  path_check_words=(bash scripts/run_macos_openssl_path_check_draft.sh)
fi
if [[ -n "$OPENSSL_ROOT" ]]; then
  path_check_words+=(--openssl-root "$OPENSSL_ROOT")
fi
path_check_words+=(--modules "$MODULE_SET")
if [[ "$VERBOSE" == "true" ]]; then
  path_check_words+=(--verbose)
fi
if [[ "$PATH_CHECK_DRY_RUN" == "true" ]]; then
  path_check_words+=("--dry-run")
fi
path_check_cmd="$(build_step_command "${path_check_words[@]}")"

if [[ ${#step_env_assignments[@]} -gt 0 ]]; then
  compile_words=(env "${step_env_assignments[@]}" python3 scripts/compile_all_modules.py)
else
  compile_words=(python3 scripts/compile_all_modules.py)
fi
compile_cmd="$(build_step_command "${compile_words[@]}")"

if [[ ${#step_env_assignments[@]} -gt 0 ]]; then
  modules_words=(
    env
    "${step_env_assignments[@]}"
    bash scripts/run_all_module_tests.sh
    --modules "$MODULE_SET"
    --reports-dir "$OUTPUT_DIR_REL/module_tests_${RUN_ID}"
    --bin-dir "tmp/module_tests_bin_${RUN_ID}"
  )
else
  modules_words=(
    bash scripts/run_all_module_tests.sh
    --modules "$MODULE_SET"
    --reports-dir "$OUTPUT_DIR_REL/module_tests_${RUN_ID}"
    --bin-dir "tmp/module_tests_bin_${RUN_ID}"
  )
fi
if [[ "$VERBOSE" == "true" ]]; then
  modules_words+=(--verbose)
fi
modules_cmd="$(build_step_command "${modules_words[@]}")"

if [[ ${#step_env_assignments[@]} -gt 0 ]]; then
  examples_words=(env "${step_env_assignments[@]}" bash scripts/verify_examples_compile.sh -f json -o "$EXAMPLES_JSON_REL")
else
  examples_words=(bash scripts/verify_examples_compile.sh -f json -o "$EXAMPLES_JSON_REL")
fi
examples_cmd="$(build_step_command "${examples_words[@]}")"

probe_exit=$(run_step "probe" "$probe_cmd" "$PROBE_LOG_REL" "$PROBE_JSON_REL" "${probe_words[@]}")
loader_symbol_probe_exit=$(run_step "loader-symbol-probe" "$loader_symbol_probe_cmd" "$LOADER_SYMBOL_PROBE_LOG_REL" "" "${loader_symbol_probe_words[@]}")
path_check_exit=$(run_step "path-check" "$path_check_cmd" "$PATH_CHECK_LOG_REL" "" "${path_check_words[@]}")
compile_exit=$(run_step "compile" "$compile_cmd" "$COMPILE_LOG_REL" "" "${compile_words[@]}")
modules_exit=$(run_step "modules" "$modules_cmd" "$MODULES_LOG_REL" "" "${modules_words[@]}")
examples_exit=$(run_step "examples" "$examples_cmd" "$EXAMPLES_LOG_REL" "" "${examples_words[@]}")

examples_total="n/a"
examples_passed="n/a"
examples_failed="n/a"
examples_skipped="n/a"
examples_rate="n/a"
examples_json_ok="false"

if [[ "$DRY_RUN" == "false" && -f "$PROJECT_ROOT/$EXAMPLES_JSON_REL" ]]; then
  if parsed="$(parse_examples_summary_json "$PROJECT_ROOT/$EXAMPLES_JSON_REL" 2>/dev/null)"; then
    examples_total="$(echo "$parsed" | sed -n '1p')"
    examples_passed="$(echo "$parsed" | sed -n '2p')"
    examples_failed="$(echo "$parsed" | sed -n '3p')"
    examples_skipped="$(echo "$parsed" | sed -n '4p')"
    examples_rate="$(echo "$parsed" | sed -n '5p')"
    examples_json_ok="true"
  else
    echo "[WAVE-B-MACOS] [examples] invalid json report=$EXAMPLES_JSON_REL" >&2
  fi
fi

probe_status="FAIL"
loader_symbol_probe_status="FAIL"
path_check_status="FAIL"
compile_status="FAIL"
modules_status="FAIL"
examples_status="FAIL"
overall_status="FAIL"

if [[ "$DRY_RUN" == "true" ]]; then
  probe_status="DRY_RUN"
  loader_symbol_probe_status="DRY_RUN"
  path_check_status="DRY_RUN"
  compile_status="DRY_RUN"
  modules_status="DRY_RUN"
  examples_status="DRY_RUN"
  overall_status="DRY_RUN"
else
  [[ "$probe_exit" == "0" ]] && probe_status="PASS"
  [[ "$loader_symbol_probe_exit" == "0" ]] && loader_symbol_probe_status="PASS"
  [[ "$path_check_exit" == "0" ]] && path_check_status="PASS"
  [[ "$compile_exit" == "0" ]] && compile_status="PASS"
  [[ "$modules_exit" == "0" ]] && modules_status="PASS"

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

  if [[ "$probe_status" == "PASS" && "$loader_symbol_probe_status" == "PASS" && "$path_check_status" == "PASS" && "$compile_status" == "PASS" && "$modules_status" == "PASS" && "$examples_status" == "PASS" ]]; then
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
| loader-symbol-probe | $loader_symbol_probe_exit | $loader_symbol_probe_status | $LOADER_SYMBOL_PROBE_JSON_REL |
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
