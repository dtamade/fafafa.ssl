#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
LINUX_SUMMARY=""
LINUX_EXAMPLES_JSON=""
MACOS_SUMMARY=""
WINDOWS_SUMMARY=""
CROSS_SUMMARY=""
CLOSURE_REPORT=""
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
Wave B / B2 Evidence Consistency Checker

用途：
  校验 B2 关键证据文件是否齐全，并检查 run_id 一致性。

用法：
  scripts/check_wave_b_b2_evidence_consistency.sh [options]

选项：
  --run-id ID                指定 run_id（默认时间戳）
  --linux-summary FILE       Linux summary 路径
  --linux-examples FILE      Linux examples JSON 路径
  --macos-summary FILE       macOS summary 路径
  --windows-summary FILE     Windows summary 路径
  --cross-summary FILE       Cross-platform summary 路径
  --closure-report FILE      Closure readiness 报告路径
  --output FILE              输出 markdown（默认 test-reports/wave_b_b2_evidence_consistency_<run_id>.md）
  --strict                   检测到缺失/不一致时返回非 0
  --dry-run                  仅打印判定结果，不写文件
  --help                     显示帮助
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
    --macos-summary)
      MACOS_SUMMARY="$2"
      shift 2
      ;;
    --windows-summary)
      WINDOWS_SUMMARY="$2"
      shift 2
      ;;
    --cross-summary)
      CROSS_SUMMARY="$2"
      shift 2
      ;;
    --closure-report)
      CLOSURE_REPORT="$2"
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

if [[ -z "$LINUX_SUMMARY" ]]; then
  LINUX_SUMMARY="test-reports/wave_b_ci_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$LINUX_EXAMPLES_JSON" ]]; then
  LINUX_EXAMPLES_JSON="test-reports/examples_compile_ci_gate_${RUN_ID}.json"
fi
if [[ -z "$MACOS_SUMMARY" ]]; then
  MACOS_SUMMARY="test-reports/wave_b_macos_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$WINDOWS_SUMMARY" ]]; then
  WINDOWS_SUMMARY="test-reports/wave_b_windows_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$CROSS_SUMMARY" ]]; then
  CROSS_SUMMARY="test-reports/wave_b_cross_platform_summary_${RUN_ID}.md"
fi
if [[ -z "$CLOSURE_REPORT" ]]; then
  CLOSURE_REPORT="test-reports/wave_b_b2_closure_readiness_${RUN_ID}.md"
fi
if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="test-reports/wave_b_b2_evidence_consistency_${RUN_ID}.md"
fi

resolve_path() {
  local file="$1"
  if [[ "$file" = /* ]]; then
    echo "$file"
  else
    echo "$PROJECT_ROOT/$file"
  fi
}

parse_run_id_md() {
  local file="$1"
  local value=""
  value="$(grep -E "^- (Run ID|run_id):" "$file" | head -1 | sed -E 's/^- (Run ID|run_id): *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)"
  echo "$value"
}

parse_closure_status_md() {
  local file="$1"
  grep -E "^- closure_status:" "$file" | head -1 | sed -E 's/^- closure_status: *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
}

required_missing=0
runid_mismatch=0
rows=()

check_markdown_artifact() {
  local label="$1"
  local rel_path="$2"
  local required="$3"

  local abs_path
  abs_path="$(resolve_path "$rel_path")"

  if [[ ! -f "$abs_path" ]]; then
    local missing_note="missing"
    rows+=("| $label | $rel_path | NO | n/a | NO | $missing_note |")
    if [[ "$required" == "true" ]]; then
      required_missing=$((required_missing + 1))
    fi
    return 0
  fi

  local parsed
  parsed="$(parse_run_id_md "$abs_path")"

  local match="NO"
  local note="run_id mismatch"
  if [[ -n "$parsed" && "$parsed" == "$RUN_ID" ]]; then
    match="YES"
    note="ok"
  elif [[ -z "$parsed" ]]; then
    note="run_id not found"
  fi

  if [[ "$match" == "NO" ]]; then
    runid_mismatch=$((runid_mismatch + 1))
  fi

  rows+=("| $label | $rel_path | YES | ${parsed:-n/a} | $match | $note |")
}

check_json_artifact() {
  local label="$1"
  local rel_path="$2"
  local required="$3"
  local abs_path
  abs_path="$(resolve_path "$rel_path")"

  if [[ ! -f "$abs_path" ]]; then
    rows+=("| $label | $rel_path | NO | n/a | n/a | missing |")
    if [[ "$required" == "true" ]]; then
      required_missing=$((required_missing + 1))
    fi
    return 0
  fi

  local json_ok="NO"
  if python3 - <<PY >/dev/null 2>&1
import json
with open(r'''$abs_path''', 'r', encoding='utf-8') as f:
    json.load(f)
PY
  then
    json_ok="YES"
  fi

  rows+=("| $label | $rel_path | YES | n/a | n/a | json_valid=$json_ok |")
  if [[ "$json_ok" != "YES" ]]; then
    runid_mismatch=$((runid_mismatch + 1))
  fi
}

check_markdown_artifact "linux_summary" "$LINUX_SUMMARY" true
check_json_artifact "linux_examples_json" "$LINUX_EXAMPLES_JSON" true
check_markdown_artifact "macos_summary" "$MACOS_SUMMARY" false
check_markdown_artifact "windows_summary" "$WINDOWS_SUMMARY" false
check_markdown_artifact "cross_summary" "$CROSS_SUMMARY" true
check_markdown_artifact "closure_report" "$CLOSURE_REPORT" true

consistency_status="CONSISTENT"
if [[ "$required_missing" -gt 0 || "$runid_mismatch" -gt 0 ]]; then
  consistency_status="INCONSISTENT"
fi

closure_status_note="n/a"
closure_abs="$(resolve_path "$CLOSURE_REPORT")"
if [[ -f "$closure_abs" ]]; then
  closure_status_note="$(parse_closure_status_md "$closure_abs")"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] required_missing=$required_missing"
  echo "[DRY-RUN] runid_mismatch=$runid_mismatch"
  echo "[DRY-RUN] consistency_status=$consistency_status"
  echo "[DRY-RUN] closure_status_note=$closure_status_note"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  if [[ "$STRICT" == "true" && "$consistency_status" != "CONSISTENT" ]]; then
    exit 1
  fi
  exit 0
fi

mkdir -p "$(dirname "$PROJECT_ROOT/$OUTPUT_FILE")"

{
  echo "# Wave B / B2 Evidence Consistency"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- consistency_status: **$consistency_status**"
  echo "- strict_mode: $STRICT"
  echo "- required_missing: $required_missing"
  echo "- runid_mismatch_or_parse_issue: $runid_mismatch"
  echo "- closure_status_note: $closure_status_note"
  echo
  echo "## Artifact Matrix"
  echo
  echo "| artifact | path | exists | parsed_run_id | run_id_match | note |"
  echo "|----------|------|--------|---------------|--------------|------|"
  for row in "${rows[@]}"; do
    echo "$row"
  done
  echo
  echo "## Gate Rule"
  echo
  echo "- CONSISTENT 条件：required_missing=0 且 runid_mismatch_or_parse_issue=0"
  echo "- strict 模式：若非 CONSISTENT，脚本返回非 0"
} > "$PROJECT_ROOT/$OUTPUT_FILE"

echo "[PASS] evidence consistency report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$consistency_status" != "CONSISTENT" ]]; then
  exit 1
fi
exit 0
