#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID=""
LINUX_SUMMARY=""
REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"
LINUX_EXAMPLES_JSON=""
LINUX_EXAMPLES_SELECTION="run_scoped_missing"
LINUX_EXAMPLES_WARNING="none"
RESOLVED_LINUX_EXAMPLES_JSON=""
MACOS_PROBE=""
MACOS_SUMMARY=""
WINDOWS_SUMMARY=""
ANDROID_SUMMARY=""
OUTPUT_FILE=""
DRY_RUN=false

usage() {
  cat <<'USAGE'
Wave B Cross-Platform Summary Generator

用途：
  聚合 Linux/macOS/Windows/Android 的 Wave B 门禁证据，生成统一 markdown 摘要。

用法：
  scripts/generate_wave_b_cross_platform_summary.sh [options]

选项：
  --run-id ID               指定 run_id（默认时间戳）
  --linux-summary FILE      Linux gate summary（默认自动取最新 wave_b_ci_gate_summary_*.md）
  --linux-examples FILE     Linux examples json（默认 tmp/wave_b_reports/examples_compile_ci_gate.json）
  --macos-probe FILE        macOS probe json（可选）
  --macos-summary FILE      macOS gate summary markdown（可选）
  --windows-summary FILE    Windows gate summary markdown（可选）
  --android-summary FILE    Android gate summary markdown（可选）
  --output FILE             输出文件（默认 tmp/wave_b_reports/wave_b_cross_platform_summary_<run_id>.md）
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
    --android-summary)
      ANDROID_SUMMARY="$2"
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

parse_run_id_json() {
  local file="$1"
  python3 - "$file" <<'PY'
import json
import sys

path = sys.argv[1]
with open(path, 'r', encoding='utf-8') as f:
    data = json.load(f)

run_id = None
if isinstance(data, dict):
    run_id = data.get('run_id')
    if run_id is None:
        meta = data.get('metadata')
        if isinstance(meta, dict):
            run_id = meta.get('run_id')
    if run_id is None:
        summary = data.get('summary')
        if isinstance(summary, dict):
            run_id = summary.get('run_id')

if isinstance(run_id, str):
    print(run_id.strip())
PY
}

resolve_preferred_examples_artifact() {
  local run_scoped_rel="$1"
  local fallback_rel="$2"
  local fallback_abs="$PROJECT_ROOT/$fallback_rel"
  local parsed_run_id=""

  if [[ -f "$PROJECT_ROOT/$run_scoped_rel" ]]; then
    LINUX_EXAMPLES_SELECTION="run_scoped_exact"
    RESOLVED_LINUX_EXAMPLES_JSON="$run_scoped_rel"
    return 0
  fi

  if [[ -f "$fallback_abs" ]]; then
    parsed_run_id="$(parse_run_id_json "$fallback_abs" || true)"
    parsed_run_id="${parsed_run_id%%$'
'*}"
    if [[ -n "$parsed_run_id" && "$parsed_run_id" == "$RUN_ID" ]]; then
      LINUX_EXAMPLES_SELECTION="static_same_run_fallback"
      RESOLVED_LINUX_EXAMPLES_JSON="$fallback_rel"
      return 0
    fi
  fi

  LINUX_EXAMPLES_SELECTION="run_scoped_missing"
  RESOLVED_LINUX_EXAMPLES_JSON="$run_scoped_rel"
}

if [[ -n "$LINUX_EXAMPLES_JSON" ]]; then
  LINUX_EXAMPLES_SELECTION="explicit_override"
elif [[ -n "${FAFAFA_WAVE_B_EXAMPLES_REPORT_REL:-}" ]]; then
  LINUX_EXAMPLES_JSON="$FAFAFA_WAVE_B_EXAMPLES_REPORT_REL"
  LINUX_EXAMPLES_SELECTION="explicit_override"
else
  resolve_preferred_examples_artifact "$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json" "$REPORTS_DIR/examples_compile_ci_gate.json"
  LINUX_EXAMPLES_JSON="$RESOLVED_LINUX_EXAMPLES_JSON"
fi

if [[ "$LINUX_EXAMPLES_SELECTION" == "explicit_override" ]]; then
  LINUX_EXAMPLES_WARNING="explicit override in use; verify owner run_id/path manually"
fi

resolve_run_scoped_artifact() {
  local run_scoped_path="$1"
  local glob_pattern="$2"

  python3 - "$PROJECT_ROOT" "$run_scoped_path" "$glob_pattern" <<'PY'
import glob
import os
import sys

project_root = sys.argv[1]
run_scoped_rel = sys.argv[2]
glob_rel = sys.argv[3]
run_scoped_abs = os.path.join(project_root, run_scoped_rel)
if os.path.isfile(run_scoped_abs):
    print(run_scoped_rel)
else:
    matches = sorted(
        glob.glob(os.path.join(project_root, glob_rel)),
        key=lambda p: (os.path.getmtime(p), p),
        reverse=True,
    )
    if matches:
        print(os.path.relpath(matches[0], project_root))
PY
}

if [[ -z "$LINUX_SUMMARY" ]]; then
  LINUX_SUMMARY="$(resolve_run_scoped_artifact "$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" "$REPORTS_DIR/wave_b_ci_gate_summary_*.md")"
fi

if [[ -z "$LINUX_SUMMARY" || ! -f "$PROJECT_ROOT/$LINUX_SUMMARY" ]]; then
  echo "[ERROR] Linux summary not found. Use --linux-summary to specify." >&2
  exit 1
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"
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
    SKIP)
      echo "SKIP"
      ;;
    SKIPPED)
      echo "SKIPPED"
      ;;
    *)
      echo "UNKNOWN"
      ;;
  esac
}

read_platform_step_status() {
  local file="$1"
  local step="$2"
  awk -F'|' -v want_step="$step" '
    {
      col_count = 0
      for (i = 1; i <= NF; i++) {
        col = $i
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", col)
        if (col == "") {
          continue
        }
        cols[++col_count] = col
      }

      if (col_count >= 2) {
        step_col = cols[1]
        if (step_col ~ /^-+$/) {
          next
        }

        if (col_count >= 4) {
          status_col = cols[3]
        } else {
          status_col = cols[2]
        }

        gsub(/\*\*/, "", status_col)
        if (tolower(step_col) == tolower(want_step)) {
          print toupper(status_col)
          exit
        }
      }
    }
  ' "$file" || true
}

normalize_check_state() {
  local status="$1"
  status="$(echo "$status" | tr -d '`*' | tr '[:lower:]' '[:upper:]' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"
  case "$status" in
    PASS|FAIL|DRY_RUN|PENDING|PROBE_ONLY|PROBE_OK|READY|MISSING|UNKNOWN)
      echo "$status"
      ;;
    SKIP)
      echo "SKIP"
      ;;
    SKIPPED)
      echo "SKIPPED"
      ;;
    *)
      echo "UNKNOWN"
      ;;
  esac
}

combine_pair_state() {
  local left="$1"
  local right="$2"

  if [[ "$left" == "FAIL" || "$right" == "FAIL" ]]; then
    echo "FAIL"
    return 0
  fi

  if [[ "$left" == "PASS" && "$right" == "PASS" ]]; then
    echo "PASS"
    return 0
  fi

  if [[ "$left" == "MISSING" || "$right" == "MISSING" ]]; then
    echo "MISSING"
    return 0
  fi

  if [[ "$left" == "UNKNOWN" || "$right" == "UNKNOWN" ]]; then
    echo "UNKNOWN"
    return 0
  fi

  if [[ "$left" == "DRY_RUN" || "$right" == "DRY_RUN" ]]; then
    echo "DRY_RUN"
    return 0
  fi

  if [[ "$left" == "SKIP" || "$right" == "SKIP" ]]; then
    echo "SKIP"
    return 0
  fi

  if [[ "$left" == "SKIPPED" || "$right" == "SKIPPED" ]]; then
    echo "SKIPPED"
    return 0
  fi

  echo "UNKNOWN"
}

linux_overall="$(read_linux_summary_field "$PROJECT_ROOT/$LINUX_SUMMARY" "Overall Status")"
if [[ -z "$linux_overall" ]]; then
  linux_overall="UNKNOWN"
fi

linux_compile_check="UNKNOWN"
linux_modules_check="UNKNOWN"
linux_examples_check="UNKNOWN"
linux_compile_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$LINUX_SUMMARY" "compile_all_modules")")"
linux_modules_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$LINUX_SUMMARY" "run_all_module_tests")")"
linux_examples_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$LINUX_SUMMARY" "verify_examples_compile")")"
if [[ "$linux_compile_check" == "UNKNOWN" ]]; then
  linux_compile_check="$(normalize_check_state "$linux_overall")"
fi
if [[ "$linux_modules_check" == "UNKNOWN" ]]; then
  linux_modules_check="$(normalize_check_state "$linux_overall")"
fi
if [[ "$linux_examples_check" == "UNKNOWN" ]]; then
  linux_examples_check="$(normalize_check_state "$linux_overall")"
fi

linux_examples_total="n/a"
linux_examples_passed="n/a"
linux_examples_failed="n/a"
linux_examples_skipped="n/a"
linux_examples_rate="n/a"

if [[ -f "$PROJECT_ROOT/$LINUX_EXAMPLES_JSON" ]]; then
  parsed_linux_examples=$(python3 - "$PROJECT_ROOT/$LINUX_EXAMPLES_JSON" <<'PY'
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

macos_state="MISSING"
macos_note="no evidence"
if [[ -n "$MACOS_SUMMARY" && -f "$PROJECT_ROOT/$MACOS_SUMMARY" ]]; then
  macos_overall="$(read_platform_summary_overall "$PROJECT_ROOT/$MACOS_SUMMARY")"
  macos_state="$(normalize_platform_state "$macos_overall")"
  if [[ -n "$macos_overall" ]]; then
    macos_note="summary: $MACOS_SUMMARY (overall=$macos_overall)"
  else
    macos_note="summary: $MACOS_SUMMARY"
  fi
elif [[ -n "$MACOS_PROBE" && -f "$PROJECT_ROOT/$MACOS_PROBE" ]]; then
  probe_status=$(python3 - "$PROJECT_ROOT/$MACOS_PROBE" <<'PY'
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

windows_state="MISSING"
windows_note="no evidence"
if [[ -n "$WINDOWS_SUMMARY" && -f "$PROJECT_ROOT/$WINDOWS_SUMMARY" ]]; then
  windows_overall="$(read_platform_summary_overall "$PROJECT_ROOT/$WINDOWS_SUMMARY")"
  windows_state="$(normalize_platform_state "$windows_overall")"
  if [[ -n "$windows_overall" ]]; then
    windows_note="summary: $WINDOWS_SUMMARY (overall=$windows_overall)"
  else
    windows_note="summary: $WINDOWS_SUMMARY"
  fi
fi

android_state="MISSING"
android_note="no evidence"
if [[ -n "$ANDROID_SUMMARY" && -f "$PROJECT_ROOT/$ANDROID_SUMMARY" ]]; then
  android_overall="$(read_platform_summary_overall "$PROJECT_ROOT/$ANDROID_SUMMARY")"
  android_state="$(normalize_platform_state "$android_overall")"
  if [[ -n "$android_overall" ]]; then
    android_note="summary: $ANDROID_SUMMARY (overall=$android_overall)"
  else
    android_note="summary: $ANDROID_SUMMARY"
  fi
fi

macos_overall_check="MISSING"
case "$macos_state" in
  PASS|FAIL|DRY_RUN|SKIPPED|UNKNOWN|MISSING)
    macos_overall_check="$macos_state"
    ;;
esac

windows_overall_check="MISSING"
case "$windows_state" in
  PASS|FAIL|DRY_RUN|SKIPPED|UNKNOWN|MISSING)
    windows_overall_check="$windows_state"
    ;;
esac

android_overall_check="MISSING"
case "$android_state" in
  PASS|FAIL|DRY_RUN|SKIPPED|UNKNOWN|MISSING)
    android_overall_check="$android_state"
    ;;
esac

macos_compile_check="MISSING"
macos_modules_check="MISSING"
macos_examples_check="MISSING"
if [[ -n "$MACOS_SUMMARY" && -f "$PROJECT_ROOT/$MACOS_SUMMARY" ]]; then
  macos_compile_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$MACOS_SUMMARY" "compile")")"
  macos_modules_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$MACOS_SUMMARY" "modules")")"
  macos_examples_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$MACOS_SUMMARY" "examples")")"
fi

windows_compile_check="MISSING"
windows_modules_check="MISSING"
windows_examples_check="MISSING"
if [[ -n "$WINDOWS_SUMMARY" && -f "$PROJECT_ROOT/$WINDOWS_SUMMARY" ]]; then
  windows_compile_legacy="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$WINDOWS_SUMMARY" "compile")")"
  windows_modules_legacy="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$WINDOWS_SUMMARY" "modules")")"
  windows_examples_legacy="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$WINDOWS_SUMMARY" "examples")")"
  windows_winssl_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$WINDOWS_SUMMARY" "winssl")")"
  windows_openssl_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$WINDOWS_SUMMARY" "openssl")")"
  windows_blocker_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$WINDOWS_SUMMARY" "winssl_blocker_batch")")"

  windows_compile_check="$windows_compile_legacy"
  if [[ "$windows_compile_check" == "UNKNOWN" && "$windows_modules_legacy" != "UNKNOWN" ]]; then
    windows_compile_check="$windows_modules_legacy"
  fi

  windows_modules_check="$windows_modules_legacy"
  if [[ "$windows_winssl_check" != "UNKNOWN" || "$windows_openssl_check" != "UNKNOWN" ]]; then
    windows_modules_check="$(combine_pair_state "$windows_winssl_check" "$windows_openssl_check")"
  fi

  windows_examples_check="$windows_examples_legacy"
  if [[ "$windows_examples_check" == "UNKNOWN" && "$windows_blocker_check" != "UNKNOWN" ]]; then
    windows_examples_check="$windows_blocker_check"
  fi
fi

android_compile_check="MISSING"
android_modules_check="MISSING"
android_examples_check="MISSING"
if [[ -n "$ANDROID_SUMMARY" && -f "$PROJECT_ROOT/$ANDROID_SUMMARY" ]]; then
  android_compile_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$ANDROID_SUMMARY" "compile")")"
  android_modules_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$ANDROID_SUMMARY" "modules")")"
  android_examples_check="$(normalize_check_state "$(read_platform_step_status "$PROJECT_ROOT/$ANDROID_SUMMARY" "examples")")"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] linux_summary=$LINUX_SUMMARY"
  echo "[DRY-RUN] macos_probe=$MACOS_PROBE"
  echo "[DRY-RUN] macos_summary=$MACOS_SUMMARY"
  echo "[DRY-RUN] windows_summary=$WINDOWS_SUMMARY"
  echo "[DRY-RUN] android_summary=$ANDROID_SUMMARY"
  echo "[DRY-RUN] linux_overall=$linux_overall"
  echo "[DRY-RUN] linux_examples_json=$LINUX_EXAMPLES_JSON"
  echo "[DRY-RUN] linux_examples_selection=$LINUX_EXAMPLES_SELECTION"
  echo "[DRY-RUN] linux_examples_warning=$LINUX_EXAMPLES_WARNING"
  echo "[DRY-RUN] linux_examples: passed=$linux_examples_passed failed=$linux_examples_failed total=$linux_examples_total rate=$linux_examples_rate"
  echo "[DRY-RUN] macos_state=$macos_state note=$macos_note"
  echo "[DRY-RUN] windows_state=$windows_state note=$windows_note"
  echo "[DRY-RUN] android_state=$android_state note=$android_note"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

mkdir -p "$(dirname "$PROJECT_ROOT/$OUTPUT_FILE")"

cat > "$PROJECT_ROOT/$OUTPUT_FILE" <<EOF_SUMMARY
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')
- linux_summary: $LINUX_SUMMARY
- linux_examples_json: $LINUX_EXAMPLES_JSON
- linux_examples_selection: $LINUX_EXAMPLES_SELECTION
- linux_examples_warning: $LINUX_EXAMPLES_WARNING

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | $linux_overall | $LINUX_SUMMARY |
| macos | $macos_state | $macos_note |
| windows | $windows_state | $windows_note |
| android | $android_state | $android_note |

## 2) Linux Gate Metrics

| metric | value |
|--------|-------|
| total | $linux_examples_total |
| passed | $linux_examples_passed |
| failed | $linux_examples_failed |
| skipped | $linux_examples_skipped |
| pass_rate | $linux_examples_rate |

## 3) Cross-Platform Checklist

| check | linux | macos | windows | android |
|-------|-------|-------|---------|---------|
| compile_all_modules | $linux_compile_check | $macos_compile_check | $windows_compile_check | $android_compile_check |
| p2_modules_gate | $linux_modules_check | $macos_modules_check | $windows_modules_check | $android_modules_check |
| examples_compile_gate | $linux_examples_check | $macos_examples_check | $windows_examples_check | $android_examples_check |
| overall | $linux_overall | $macos_overall_check | $windows_overall_check | $android_overall_check |

## 4) Next Actions

- 在 macOS runner 执行 B2 命令并回填 macos 证据文件。
- 在 Windows runner 执行 WinSSL/OpenSSL 对照回归并回填 windows 证据文件。
- 在 Android runner 执行门禁回归并回填 android 证据文件。
- 回填后重新运行本脚本，形成最终四平台对齐摘要。
EOF_SUMMARY

echo "[PASS] wave-b cross-platform summary generated: $OUTPUT_FILE"
