#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"
LINUX_SUMMARY=""
MACOS_SUMMARY=""
WINDOWS_SUMMARY=""
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
Wave B / B2 Closure Readiness Checker

用途：
  读取 Linux/macOS/Windows gate summary，判定 B2 是否已满足最终闭环条件。

用法：
  scripts/check_wave_b_b2_closure_readiness.sh [options]

选项：
  --run-id ID               指定 run_id（默认时间戳）
  --reports-dir DIR        报告目录（默认 tmp/wave_b_reports）
  --linux-summary FILE      Linux summary（默认自动选最新 wave_b_ci_gate_summary_*.md）
  --macos-summary FILE      macOS summary（可选）
  --windows-summary FILE    Windows summary（可选）
  --output FILE             输出 markdown（默认 tmp/wave_b_reports/wave_b_b2_closure_readiness_<run_id>.md）
  --strict                  若未闭环则返回非 0
  --dry-run                 仅打印判定，不写文件
  --help                    显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --reports-dir)
      REPORTS_DIR="$2"
      shift 2
      ;;
    --linux-summary)
      LINUX_SUMMARY="$2"
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

resolve_run_scoped_artifact() {
  local run_scoped_path="$1"
  local glob_pattern="$2"

  python3 - "$run_scoped_path" "$glob_pattern" <<'PY'
import glob
import os
import sys

run_scoped_path = sys.argv[1]
glob_pattern = sys.argv[2]
if os.path.isfile(run_scoped_path):
    print(run_scoped_path)
else:
    matches = sorted(glob.glob(glob_pattern), key=lambda p: (os.path.getmtime(p), p), reverse=True)
    if matches:
        print(matches[0])
PY
}

if [[ -z "$LINUX_SUMMARY" ]]; then
  LINUX_SUMMARY="$(resolve_run_scoped_artifact "$PROJECT_ROOT/$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" "$PROJECT_ROOT/$REPORTS_DIR/wave_b_ci_gate_summary_*.md")"
  if [[ -n "$LINUX_SUMMARY" ]]; then
    LINUX_SUMMARY="${LINUX_SUMMARY#"$PROJECT_ROOT/"}"
  fi
fi

if [[ -z "$LINUX_SUMMARY" ]]; then
  echo "[ERROR] linux summary not found" >&2
  exit 1
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md"
fi

resolve_path() {
  local file="$1"
  if [[ -z "$file" ]]; then
    echo ""
    return 0
  fi
  if [[ "$file" = /* ]]; then
    echo "$file"
  else
    echo "$PROJECT_ROOT/$file"
  fi
}

normalize_status() {
  local value="$1"
  local upper="$(echo "$value" | tr '[:lower:]' '[:upper:]')"
  case "$upper" in
    PASS|FAIL|DRY_RUN)
      echo "$upper"
      ;;
    SKIP|SKIPPED)
      echo "SKIPPED"
      ;;
    *)
      echo "UNKNOWN"
      ;;
  esac
}

read_overall_field() {
  local file="$1"
  grep -E "^- (Overall Status|overall):" "$file" \
    | head -1 \
    | sed -E 's/^- (Overall Status|overall): *//' \
    | tr -d '`*' \
    | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
}

evaluate_platform() {
  local label="$1"
  local rel_file="$2"

  if [[ -z "$rel_file" ]]; then
    echo "${label}|PENDING|no evidence|"
    return 0
  fi

  local abs_file
  abs_file="$(resolve_path "$rel_file")"
  if [[ ! -f "$abs_file" ]]; then
    echo "${label}|PENDING|missing file|$rel_file"
    return 0
  fi

  local raw
  raw="$(read_overall_field "$abs_file")"
  local status
  status="$(normalize_status "$raw")"

  if [[ "$status" == "UNKNOWN" ]]; then
    echo "${label}|READY|summary exists but overall unknown|$rel_file"
  else
    echo "${label}|$status|summary parsed|$rel_file"
  fi
}

linux_info="$(evaluate_platform "linux" "$LINUX_SUMMARY")"
macos_info="$(evaluate_platform "macos" "$MACOS_SUMMARY")"
windows_info="$(evaluate_platform "windows" "$WINDOWS_SUMMARY")"

linux_state="$(echo "$linux_info" | cut -d'|' -f2)"
linux_note="$(echo "$linux_info" | cut -d'|' -f3)"
linux_file="$(echo "$linux_info" | cut -d'|' -f4)"

macos_state="$(echo "$macos_info" | cut -d'|' -f2)"
macos_note="$(echo "$macos_info" | cut -d'|' -f3)"
macos_file="$(echo "$macos_info" | cut -d'|' -f4)"

windows_state="$(echo "$windows_info" | cut -d'|' -f2)"
windows_note="$(echo "$windows_info" | cut -d'|' -f3)"
windows_file="$(echo "$windows_info" | cut -d'|' -f4)"

closure_status="IN_PROGRESS"
if [[ "$linux_state" == "PASS" && "$macos_state" == "PASS" && "$windows_state" == "PASS" ]]; then
  closure_status="CLOSED"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] linux_summary=$LINUX_SUMMARY"
  echo "[DRY-RUN] macos_summary=$MACOS_SUMMARY"
  echo "[DRY-RUN] windows_summary=$WINDOWS_SUMMARY"
  echo "[DRY-RUN] linux=$linux_state ($linux_note)"
  echo "[DRY-RUN] macos=$macos_state ($macos_note)"
  echo "[DRY-RUN] windows=$windows_state ($windows_note)"
  echo "[DRY-RUN] closure_status=$closure_status"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  if [[ "$STRICT" == "true" && "$closure_status" != "CLOSED" ]]; then
    exit 1
  fi
  exit 0
fi

mkdir -p "$(dirname "$PROJECT_ROOT/$OUTPUT_FILE")"

cat > "$PROJECT_ROOT/$OUTPUT_FILE" <<EOF_REPORT
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')
- closure_status: **$closure_status**
- strict_mode: $STRICT

## Platform Status

| platform | state | note | summary |
|----------|-------|------|---------|
| linux | $linux_state | $linux_note | $linux_file |
| macos | $macos_state | $macos_note | $macos_file |
| windows | $windows_state | $windows_note | $windows_file |

## Closure Criteria

- linux = PASS
- macos = PASS
- windows = PASS

## Status Semantics

- \`PASS\`: live gate passed and counts toward closure.
- \`DRY_RUN\`: rehearsal evidence exists but does not count toward closure.
- \`SKIPPED\`: intentionally skipped evidence; does not count toward closure.
- \`PENDING\`: expected summary/evidence file is missing.
- \`READY\`: summary exists but overall field is unknown/unmapped.

## Next Actions

- 若 macOS 为 DRY_RUN/SKIPPED/PENDING/READY：在 macOS runner 执行 live gate 并回填 summary。
- 若 Windows 为 DRY_RUN/SKIPPED/PENDING/READY：在 Windows runner 执行 live gate 并回填 summary。
- 三平台 summary 回填后，复跑 'scripts/generate_wave_b_cross_platform_summary.sh'。
EOF_REPORT

echo "[PASS] readiness report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$closure_status" != "CLOSED" ]]; then
  exit 1
fi
exit 0
