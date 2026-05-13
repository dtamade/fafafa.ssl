#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID=""
RUN_ID_EXPLICIT=false
LINUX_SUMMARY=""
LINUX_SUMMARY_EXPLICIT=false
LINUX_EXAMPLES_JSON=""
LINUX_EXAMPLES_EXPLICIT=false
MACOS_PROBE=""
MACOS_PROBE_EXPLICIT=false
MACOS_SUMMARY=""
MACOS_SUMMARY_EXPLICIT=false
WINDOWS_SUMMARY=""
WINDOWS_SUMMARY_EXPLICIT=false
WINDOWS_QUICK_LOG=""
WINDOWS_QUICK_LOG_EXPLICIT=false
WINDOWS_RUNTIME_TRANSCRIPT=""
WINDOWS_RUNTIME_TRANSCRIPT_EXPLICIT=false
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
  --run-id ID                指定 run_id（默认优先从 Linux summary、active Linux truth 或现有报告推导，否则时间戳）
  --linux-summary FILE       Linux summary 路径
  --linux-examples FILE      Linux examples JSON 路径
  --macos-probe FILE         macOS probe JSON 路径
  --macos-summary FILE       macOS summary 路径
  --windows-summary FILE     Windows summary 路径
  --windows-quick-log FILE   Windows quick smoke 日志路径
  --windows-runtime-transcript FILE
                             Windows broader runtime transcript 路径
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
      RUN_ID_EXPLICIT=true
      shift 2
      ;;
    --linux-summary)
      LINUX_SUMMARY="$2"
      LINUX_SUMMARY_EXPLICIT=true
      shift 2
      ;;
    --linux-examples)
      LINUX_EXAMPLES_JSON="$2"
      LINUX_EXAMPLES_EXPLICIT=true
      shift 2
      ;;
    --macos-probe)
      MACOS_PROBE="$2"
      MACOS_PROBE_EXPLICIT=true
      shift 2
      ;;
    --macos-summary)
      MACOS_SUMMARY="$2"
      MACOS_SUMMARY_EXPLICIT=true
      shift 2
      ;;
    --windows-summary)
      WINDOWS_SUMMARY="$2"
      WINDOWS_SUMMARY_EXPLICIT=true
      shift 2
      ;;
    --windows-quick-log)
      WINDOWS_QUICK_LOG="$2"
      WINDOWS_QUICK_LOG_EXPLICIT=true
      shift 2
      ;;
    --windows-runtime-transcript)
      WINDOWS_RUNTIME_TRANSCRIPT="$2"
      WINDOWS_RUNTIME_TRANSCRIPT_EXPLICIT=true
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

infer_run_id_from_linux_summary() {
  local file="$1"
  if [[ -z "$file" ]]; then
    echo ""
    return 0
  fi

  local abs_file
  abs_file="$(resolve_path "$file")"
  if [[ ! -f "$abs_file" ]]; then
    echo ""
    return 0
  fi

  parse_run_id_md "$abs_file"
}

infer_run_id_from_markdown_artifact() {
  local file="$1"
  if [[ -z "$file" ]]; then
    echo ""
    return 0
  fi

  local abs_file
  abs_file="$(resolve_path "$file")"
  if [[ ! -f "$abs_file" ]]; then
    echo ""
    return 0
  fi

  parse_run_id_md "$abs_file"
}

parse_cross_summary_linux_summary_path() {
  local file="$1"
  grep -E "^- linux_summary:" "$file" | head -1 | sed -E 's/^- linux_summary: *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
}

infer_run_id_from_cross_summary_linux_summary() {
  local file="$1"
  if [[ -z "$file" ]]; then
    echo ""
    return 0
  fi

  local abs_file
  abs_file="$(resolve_path "$file")"
  if [[ ! -f "$abs_file" ]]; then
    echo ""
    return 0
  fi

  local cross_summary_linux_summary
  cross_summary_linux_summary="$(parse_cross_summary_linux_summary_path "$abs_file")"
  infer_run_id_from_linux_summary "$cross_summary_linux_summary"
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

derive_sibling_artifact_path() {
  local anchor_path="$1"
  local filename="$2"
  local anchor_dir
  anchor_dir="$(dirname "$anchor_path")"
  if [[ "$anchor_dir" == "." ]]; then
    echo "$filename"
  else
    echo "$anchor_dir/$filename"
  fi
}

if [[ "$RUN_ID_EXPLICIT" != "true" ]]; then
  RUN_ID="$(infer_run_id_from_linux_summary "$LINUX_SUMMARY")"
  if [[ -z "$RUN_ID" ]]; then
    RUN_ID="$(infer_run_id_from_cross_summary_linux_summary "$CROSS_SUMMARY")"
  fi
  if [[ -z "$RUN_ID" ]]; then
    RUN_ID="$(infer_run_id_from_markdown_artifact "$CROSS_SUMMARY")"
  fi
  if [[ -z "$RUN_ID" ]]; then
    RUN_ID="$(infer_run_id_from_markdown_artifact "$CLOSURE_REPORT")"
  fi
fi
if [[ -z "$RUN_ID" ]]; then
  RUN_ID="$(date +%Y%m%d_%H%M%S)"
fi

if [[ -z "$LINUX_SUMMARY" ]]; then
  LINUX_SUMMARY="test-reports/wave_b_ci_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$LINUX_EXAMPLES_JSON" ]]; then
  LINUX_EXAMPLES_JSON="$(default_linux_examples_json_path)"
fi
if [[ -z "$MACOS_PROBE" ]]; then
  MACOS_PROBE="test-reports/wave_b_macos_gate_probe_${RUN_ID}.json"
fi
if [[ -z "$MACOS_SUMMARY" ]]; then
  MACOS_SUMMARY="test-reports/wave_b_macos_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$WINDOWS_SUMMARY" ]]; then
  WINDOWS_SUMMARY="test-reports/wave_b_windows_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$WINDOWS_QUICK_LOG" ]]; then
  WINDOWS_QUICK_LOG="$(derive_sibling_artifact_path "$WINDOWS_SUMMARY" "winssl_quick_smoke_${RUN_ID}.log")"
fi
if [[ -z "$WINDOWS_RUNTIME_TRANSCRIPT" ]]; then
  WINDOWS_RUNTIME_TRANSCRIPT="$(derive_sibling_artifact_path "$WINDOWS_SUMMARY" "winssl_runtime_suite_${RUN_ID}.log")"
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

parse_closure_status_md() {
  local file="$1"
  grep -E "^- closure_status:" "$file" | head -1 | sed -E 's/^- closure_status: *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
}

parse_cross_summary_linux_examples_path() {
  local file="$1"
  grep -E "^- linux_examples_json:" "$file" | head -1 | sed -E 's/^- linux_examples_json: *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
}

parse_cross_summary_macos_summary_path() {
  local file="$1"
  awk -F'|' '
    {
      if (NF >= 4) {
        platform_col = $2
        evidence_col = $4
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", platform_col)
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", evidence_col)
        if (tolower(platform_col) == "macos" && evidence_col ~ /^summary: /) {
          sub(/^summary: /, "", evidence_col)
          sub(/ \(overall=.*$/, "", evidence_col)
          print evidence_col
          exit
        }
      }
    }
  ' "$file" || true
}

parse_cross_summary_windows_summary_path() {
  local file="$1"
  awk -F'|' '
    {
      if (NF >= 4) {
        platform_col = $2
        evidence_col = $4
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", platform_col)
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", evidence_col)
        if (tolower(platform_col) == "windows" && evidence_col ~ /^summary: /) {
          sub(/^summary: /, "", evidence_col)
          sub(/ \(overall=.*$/, "", evidence_col)
          print evidence_col
          exit
        }
      }
    }
  ' "$file" || true
}

parse_cross_summary_macos_probe_path() {
  local file="$1"
  awk -F'|' '
    {
      if (NF >= 4) {
        platform_col = $2
        state_col = $3
        evidence_col = $4
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", platform_col)
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", state_col)
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", evidence_col)
        if (tolower(platform_col) == "macos" && (state_col == "PROBE_ONLY" || state_col == "PROBE_OK") && evidence_col ~ /^probe: /) {
          sub(/^probe: /, "", evidence_col)
          sub(/ \(status=.*$/, "", evidence_col)
          print evidence_col
          exit
        }
      }
    }
  ' "$file" || true
}

required_missing=0
runid_mismatch=0
rows=()
cross_summary_abs="$(resolve_path "$CROSS_SUMMARY")"
cross_summary_linux_summary=""
cross_summary_linux_examples=""
cross_summary_macos_summary=""
cross_summary_windows_summary=""
cross_summary_macos_probe=""
if [[ -f "$cross_summary_abs" ]]; then
  cross_summary_linux_summary="$(parse_cross_summary_linux_summary_path "$cross_summary_abs")"
  cross_summary_linux_examples="$(parse_cross_summary_linux_examples_path "$cross_summary_abs")"
  cross_summary_macos_summary="$(parse_cross_summary_macos_summary_path "$cross_summary_abs")"
  cross_summary_windows_summary="$(parse_cross_summary_windows_summary_path "$cross_summary_abs")"
  cross_summary_macos_probe="$(parse_cross_summary_macos_probe_path "$cross_summary_abs")"
fi
if [[ "$LINUX_SUMMARY_EXPLICIT" != "true" && -n "$cross_summary_linux_summary" ]]; then
  LINUX_SUMMARY="$cross_summary_linux_summary"
fi
if [[ "$LINUX_EXAMPLES_EXPLICIT" != "true" && -n "$cross_summary_linux_examples" ]]; then
  LINUX_EXAMPLES_JSON="$cross_summary_linux_examples"
fi
if [[ "$MACOS_SUMMARY_EXPLICIT" != "true" && -n "$cross_summary_macos_summary" ]]; then
  MACOS_SUMMARY="$cross_summary_macos_summary"
fi
if [[ "$WINDOWS_SUMMARY_EXPLICIT" != "true" && -n "$cross_summary_windows_summary" ]]; then
  WINDOWS_SUMMARY="$cross_summary_windows_summary"
fi
if [[ "$WINDOWS_QUICK_LOG_EXPLICIT" != "true" ]]; then
  WINDOWS_QUICK_LOG="$(derive_sibling_artifact_path "$WINDOWS_SUMMARY" "winssl_quick_smoke_${RUN_ID}.log")"
fi
if [[ "$WINDOWS_RUNTIME_TRANSCRIPT_EXPLICIT" != "true" ]]; then
  WINDOWS_RUNTIME_TRANSCRIPT="$(derive_sibling_artifact_path "$WINDOWS_SUMMARY" "winssl_runtime_suite_${RUN_ID}.log")"
fi

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

check_presence_artifact() {
  local label="$1"
  local rel_path="$2"
  local required="$3"
  local abs_path
  abs_path="$(resolve_path "$rel_path")"

  if [[ ! -f "$abs_path" ]]; then
    local missing_note="optional missing"
    if [[ "$required" == "true" ]]; then
      missing_note="missing"
      required_missing=$((required_missing + 1))
    fi
    rows+=("| $label | $rel_path | NO | n/a | n/a | $missing_note |")
    return 0
  fi

  rows+=("| $label | $rel_path | YES | n/a | n/a | presence-only evidence |")
}

check_markdown_artifact "linux_summary" "$LINUX_SUMMARY" true
check_json_artifact "linux_examples_json" "$LINUX_EXAMPLES_JSON" true
macos_summary_required=false
if [[ -n "$cross_summary_macos_summary" ]]; then
  macos_summary_required=true
fi
macos_probe_required=false
macos_probe_track=false
if [[ "$MACOS_PROBE_EXPLICIT" == "true" ]]; then
  macos_probe_required=true
  macos_probe_track=true
elif [[ -n "$cross_summary_macos_probe" ]]; then
  MACOS_PROBE="$cross_summary_macos_probe"
  macos_probe_required=true
  macos_probe_track=true
fi
if [[ "$macos_probe_track" == "true" ]]; then
  check_json_artifact "macos_probe" "$MACOS_PROBE" "$macos_probe_required"
fi
check_markdown_artifact "macos_summary" "$MACOS_SUMMARY" "$macos_summary_required"
windows_summary_required=false
if [[ -n "$cross_summary_windows_summary" ]]; then
  windows_summary_required=true
fi
check_markdown_artifact "windows_summary" "$WINDOWS_SUMMARY" "$windows_summary_required"
windows_runtime_required=false
windows_summary_abs="$(resolve_path "$WINDOWS_SUMMARY")"
if [[ -n "$cross_summary_windows_summary" ]]; then
  windows_runtime_required=true
fi
if [[ -f "$windows_summary_abs" ]]; then
  windows_runtime_required=true
fi
check_presence_artifact "windows_quick_log" "$WINDOWS_QUICK_LOG" "$windows_runtime_required"
check_presence_artifact "windows_runtime_transcript" "$WINDOWS_RUNTIME_TRANSCRIPT" "$windows_runtime_required"
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

OUTPUT_ABS="$(resolve_path "$OUTPUT_FILE")"
mkdir -p "$(dirname "$OUTPUT_ABS")"

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
} > "$OUTPUT_ABS"

echo "[PASS] evidence consistency report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$consistency_status" != "CONSISTENT" ]]; then
  exit 1
fi
exit 0
