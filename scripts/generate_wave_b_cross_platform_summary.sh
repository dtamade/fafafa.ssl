#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID=""
LINUX_SUMMARY=""
LINUX_EXAMPLES_JSON=""
MACOS_PROBE=""
MACOS_SUMMARY=""
WINDOWS_SUMMARY=""
OUTPUT_FILE=""
DRY_RUN=false

usage() {
  cat <<'USAGE'
Wave B Cross-Platform Summary Generator

用途：
  聚合 Linux/macOS/Windows 的 Wave B 门禁证据，生成统一 markdown 摘要。

用法：
  scripts/generate_wave_b_cross_platform_summary.sh [options]

选项：
  --run-id ID               指定 run_id（默认时间戳）
  --linux-summary FILE      Linux gate summary（默认自动取最新 wave_b_ci_gate_summary_*.md）
  --linux-examples FILE     Linux examples json（默认优先 test-reports/examples_compile_ci_gate_<run_id>.json，fallback 到旧 generic 路径）
  --macos-probe FILE        macOS probe json（可选）
  --macos-summary FILE      macOS gate summary markdown（可选）
  --windows-summary FILE    Windows gate summary markdown（可选）
  --output FILE             输出文件（默认 test-reports/wave_b_cross_platform_summary_<run_id>.md）
  --dry-run                 仅打印参数与判定，不写文件
  --help                    显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --linux-summary)
      LINUX_SUMMARY="$2"
      shift 2
      ;;
    --linux-examples)
      LINUX_EXAMPLES_JSON="$2"
      shift 2
      ;;
    --macos-probe)
      MACOS_PROBE="$2"
      shift 2
      ;;
    --macos-summary)
      MACOS_SUMMARY="$2"
      shift 2
      ;;
    --windows-summary)
      WINDOWS_SUMMARY="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
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

if [[ -z "$RUN_ID" ]]; then
  RUN_ID="$(date +%Y%m%d_%H%M%S)"
fi

resolve_path() {
  local file="$1"
  if [[ "$file" = /* ]]; then
    echo "$file"
  else
    echo "$PROJECT_ROOT/$file"
  fi
}

default_linux_examples_json_path() {
  local run_specific="test-reports/examples_compile_ci_gate_${RUN_ID}.json"
  local generic="test-reports/examples_compile_ci_gate.json"
  if [[ -f "$(resolve_path "$run_specific")" ]]; then
    echo "$run_specific"
  elif [[ -f "$(resolve_path "$generic")" ]]; then
    echo "$generic"
  else
    echo "$run_specific"
  fi
}

if [[ -z "$LINUX_SUMMARY" ]]; then
  LINUX_SUMMARY="$(cd "$PROJECT_ROOT" && ls -1t test-reports/wave_b_ci_gate_summary_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$LINUX_EXAMPLES_JSON" ]]; then
  LINUX_EXAMPLES_JSON="$(default_linux_examples_json_path)"
fi

LINUX_SUMMARY_ABS="$(resolve_path "$LINUX_SUMMARY")"
LINUX_EXAMPLES_JSON_ABS="$(resolve_path "$LINUX_EXAMPLES_JSON")"
MACOS_PROBE_ABS=""
MACOS_SUMMARY_ABS=""
WINDOWS_SUMMARY_ABS=""
if [[ -n "$MACOS_PROBE" ]]; then
  MACOS_PROBE_ABS="$(resolve_path "$MACOS_PROBE")"
fi
if [[ -n "$MACOS_SUMMARY" ]]; then
  MACOS_SUMMARY_ABS="$(resolve_path "$MACOS_SUMMARY")"
fi
if [[ -n "$WINDOWS_SUMMARY" ]]; then
  WINDOWS_SUMMARY_ABS="$(resolve_path "$WINDOWS_SUMMARY")"
fi

if [[ -z "$LINUX_SUMMARY" || ! -f "$LINUX_SUMMARY_ABS" ]]; then
  echo "[ERROR] Linux summary not found. Use --linux-summary to specify." >&2
  exit 1
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="test-reports/wave_b_cross_platform_summary_${RUN_ID}.md"
fi

read_linux_summary_field() {
  local file="$1"
  local key="$2"
  grep -E "^- ${key}:" "$file" | head -1 | sed -E "s/^- ${key}: *//" | tr -d '`*' || true
}

read_platform_summary_overall() {
  local file="$1"
  grep -E "^- overall:" "$file" \
    | head -1 \
    | sed -E 's/^- overall: *//' \
    | tr -d '`*' \
    | tr '[:lower:]' '[:upper:]' \
    | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
}

normalize_platform_state() {
  local overall="$1"
  case "$overall" in
    PASS|FAIL|DRY_RUN)
      echo "$overall"
      ;;
    *)
      echo "READY"
      ;;
  esac
}

read_platform_step_status() {
  local file="$1"
  local step="$2"
  awk -F'|' -v want_step="$step" '
    {
      if (NF >= 4) {
        step_col = $2
        status_col = $3
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", step_col)
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", status_col)
        gsub(/\*\*/, "", status_col)
        if (tolower(step_col) == tolower(want_step)) {
          print toupper(status_col)
          exit
        }
      }
    }
  ' "$file" || true
}

parse_check_state() {
  local status="$1"
  status="$(echo "$status" | tr -d '`*' | tr '[:lower:]' '[:upper:]' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"
  case "$status" in
    PASS|FAIL|DRY_RUN|SKIP|PENDING|PROBE_ONLY|PROBE_OK|READY)
      echo "$status"
      ;;
    *)
      echo ""
      ;;
  esac
}

stable_check_state() {
  local status="$1"
  local parsed
  parsed="$(parse_check_state "$status")"
  if [[ -n "$parsed" ]]; then
    echo "$parsed"
  else
    echo "PENDING"
  fi
}

linux_overall="$(read_linux_summary_field "$LINUX_SUMMARY_ABS" "Overall Status")"
if [[ -z "$linux_overall" ]]; then
  linux_overall="UNKNOWN"
fi

linux_compile_check="$(parse_check_state "$(read_platform_step_status "$LINUX_SUMMARY_ABS" "compile_all_modules")")"
linux_modules_check="$(parse_check_state "$(read_platform_step_status "$LINUX_SUMMARY_ABS" "run_all_module_tests")")"
linux_examples_check="$(parse_check_state "$(read_platform_step_status "$LINUX_SUMMARY_ABS" "verify_examples_compile")")"
if [[ -z "$linux_compile_check" ]]; then
  linux_compile_check="$(stable_check_state "$linux_overall")"
fi
if [[ -z "$linux_modules_check" ]]; then
  linux_modules_check="$(stable_check_state "$linux_overall")"
fi
if [[ -z "$linux_examples_check" ]]; then
  linux_examples_check="$(stable_check_state "$linux_overall")"
fi

linux_examples_total="n/a"
linux_examples_passed="n/a"
linux_examples_failed="n/a"
linux_examples_skipped="n/a"
linux_examples_rate="n/a"

if [[ -f "$LINUX_EXAMPLES_JSON_ABS" ]]; then
  parsed_linux_examples=$(python3 - "$LINUX_EXAMPLES_JSON_ABS" <<'PY'
import json
import sys
p = sys.argv[1]
with open(p, 'r', encoding='utf-8') as f:
    data = json.load(f)
s = data.get('summary', {})
print(s.get('total', 'n/a'))
print(s.get('passed', 'n/a'))
print(s.get('failed', 'n/a'))
print(s.get('skipped', 'n/a'))
print(s.get('pass_rate', 'n/a'))
PY
)
  linux_examples_total="$(echo "$parsed_linux_examples" | sed -n '1p')"
  linux_examples_passed="$(echo "$parsed_linux_examples" | sed -n '2p')"
  linux_examples_failed="$(echo "$parsed_linux_examples" | sed -n '3p')"
  linux_examples_skipped="$(echo "$parsed_linux_examples" | sed -n '4p')"
  linux_examples_rate="$(echo "$parsed_linux_examples" | sed -n '5p')"
fi

macos_state="PENDING"
macos_note="no evidence"
if [[ -n "$MACOS_SUMMARY" && -f "$MACOS_SUMMARY_ABS" ]]; then
  macos_overall="$(read_platform_summary_overall "$MACOS_SUMMARY_ABS")"
  macos_state="$(normalize_platform_state "$macos_overall")"
  if [[ -n "$macos_overall" ]]; then
    macos_note="summary: $MACOS_SUMMARY (overall=$macos_overall)"
  else
    macos_note="summary: $MACOS_SUMMARY"
  fi
elif [[ -n "$MACOS_PROBE" && -f "$MACOS_PROBE_ABS" ]]; then
  probe_status=$(python3 - "$MACOS_PROBE_ABS" <<'PY'
import json
import sys
with open(sys.argv[1], 'r', encoding='utf-8') as f:
    d = json.load(f)
print(d.get('status', 'unknown'))
PY
)
  if [[ "$probe_status" == "error" ]]; then
    macos_state="PROBE_ONLY"
  else
    macos_state="PROBE_OK"
  fi
  macos_note="probe: $MACOS_PROBE (status=$probe_status)"
fi

windows_state="PENDING"
windows_note="no evidence"
if [[ -n "$WINDOWS_SUMMARY" && -f "$WINDOWS_SUMMARY_ABS" ]]; then
  windows_overall="$(read_platform_summary_overall "$WINDOWS_SUMMARY_ABS")"
  windows_state="$(normalize_platform_state "$windows_overall")"
  if [[ -n "$windows_overall" ]]; then
    windows_note="summary: $WINDOWS_SUMMARY (overall=$windows_overall)"
  else
    windows_note="summary: $WINDOWS_SUMMARY"
  fi
fi

macos_overall_check="$(stable_check_state "$macos_state")"
windows_overall_check="$(stable_check_state "$windows_state")"

macos_compile_check="PENDING"
macos_modules_check="PENDING"
macos_examples_check="PENDING"
if [[ -n "$MACOS_SUMMARY" && -f "$MACOS_SUMMARY_ABS" ]]; then
  parsed_macos_compile="$(parse_check_state "$(read_platform_step_status "$MACOS_SUMMARY_ABS" "compile")")"
  parsed_macos_modules="$(parse_check_state "$(read_platform_step_status "$MACOS_SUMMARY_ABS" "modules")")"
  parsed_macos_examples="$(parse_check_state "$(read_platform_step_status "$MACOS_SUMMARY_ABS" "examples")")"
  if [[ -n "$parsed_macos_compile" ]]; then
    macos_compile_check="$parsed_macos_compile"
  fi
  if [[ -n "$parsed_macos_modules" ]]; then
    macos_modules_check="$parsed_macos_modules"
  fi
  if [[ -n "$parsed_macos_examples" ]]; then
    macos_examples_check="$parsed_macos_examples"
  fi
fi

windows_compile_check="PENDING"
windows_modules_check="PENDING"
windows_examples_check="PENDING"
if [[ -n "$WINDOWS_SUMMARY" && -f "$WINDOWS_SUMMARY_ABS" ]]; then
  parsed_windows_compile="$(parse_check_state "$(read_platform_step_status "$WINDOWS_SUMMARY_ABS" "compile")")"
  parsed_windows_modules="$(parse_check_state "$(read_platform_step_status "$WINDOWS_SUMMARY_ABS" "modules")")"
  parsed_windows_examples="$(parse_check_state "$(read_platform_step_status "$WINDOWS_SUMMARY_ABS" "examples")")"
  if [[ -n "$parsed_windows_compile" ]]; then
    windows_compile_check="$parsed_windows_compile"
  fi
  if [[ -n "$parsed_windows_modules" ]]; then
    windows_modules_check="$parsed_windows_modules"
  fi
  if [[ -n "$parsed_windows_examples" ]]; then
    windows_examples_check="$parsed_windows_examples"
  fi
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] linux_summary=$LINUX_SUMMARY"
  echo "[DRY-RUN] linux_overall=$linux_overall"
  echo "[DRY-RUN] linux_examples: passed=$linux_examples_passed failed=$linux_examples_failed total=$linux_examples_total rate=$linux_examples_rate"
  echo "[DRY-RUN] macos_state=$macos_state note=$macos_note"
  echo "[DRY-RUN] windows_state=$windows_state note=$windows_note"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

OUTPUT_ABS="$(resolve_path "$OUTPUT_FILE")"
mkdir -p "$(dirname "$OUTPUT_ABS")"

cat > "$OUTPUT_ABS" <<EOF_SUMMARY
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')
- linux_summary: $LINUX_SUMMARY
- linux_examples_json: $LINUX_EXAMPLES_JSON

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | $linux_overall | $LINUX_SUMMARY |
| macos | $macos_state | $macos_note |
| windows | $windows_state | $windows_note |

## 2) Linux Gate Metrics

| metric | value |
|--------|-------|
| total | $linux_examples_total |
| passed | $linux_examples_passed |
| failed | $linux_examples_failed |
| skipped | $linux_examples_skipped |
| pass_rate | $linux_examples_rate |

## 3) Cross-Platform Checklist

| check | linux | macos | windows |
|-------|-------|-------|---------|
| compile_all_modules | $linux_compile_check | $macos_compile_check | $windows_compile_check |
| p2_modules_gate | $linux_modules_check | $macos_modules_check | $windows_modules_check |
| examples_compile_gate | $linux_examples_check | $macos_examples_check | $windows_examples_check |
| overall | $linux_overall | $macos_overall_check | $windows_overall_check |

## 4) Next Actions

- 在 macOS runner 执行 B2 命令并回填 macos 证据文件。
- 在 Windows runner 执行 WinSSL/OpenSSL 对照回归并回填 windows 证据文件。
- 回填后重新运行本脚本，形成最终三平台对齐摘要。
EOF_SUMMARY

echo "[PASS] wave-b cross-platform summary generated: $OUTPUT_FILE"
