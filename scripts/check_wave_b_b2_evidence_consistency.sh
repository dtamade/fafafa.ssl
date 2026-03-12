#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"
LINUX_SUMMARY=""
LINUX_EXAMPLES_JSON=""
LINUX_EXAMPLES_SELECTION="run_scoped_missing"
LINUX_EXAMPLES_WARNING="none"
RESOLVED_LINUX_EXAMPLES_JSON=""
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
  --reports-dir DIR         报告目录（默认 tmp/wave_b_reports）
  --linux-summary FILE       Linux summary 路径
  --linux-examples FILE      Linux examples JSON 路径
  --macos-summary FILE       macOS summary 路径
  --windows-summary FILE     Windows summary 路径
  --cross-summary FILE       Cross-platform summary 路径
  --closure-report FILE      Closure readiness 报告路径
  --output FILE              输出 markdown（默认 tmp/wave_b_reports/wave_b_b2_evidence_consistency_<run_id>.md）
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
    --reports-dir)
      REPORTS_DIR="$2"
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
  LINUX_SUMMARY="$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md"
fi

resolve_path() {
  local file="$1"
  if [[ "$file" = /* ]]; then
    echo "$file"
  else
    echo "$PROJECT_ROOT/$file"
  fi
}

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
  local run_scoped_abs
  local fallback_abs
  local parsed_run_id=""

  run_scoped_abs="$(resolve_path "$run_scoped_rel")"
  fallback_abs="$(resolve_path "$fallback_rel")"

  if [[ -f "$run_scoped_abs" ]]; then
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

if [[ -z "$MACOS_SUMMARY" ]]; then
  MACOS_SUMMARY="$REPORTS_DIR/wave_b_macos_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$WINDOWS_SUMMARY" ]]; then
  WINDOWS_SUMMARY="$REPORTS_DIR/wave_b_windows_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$CROSS_SUMMARY" ]]; then
  CROSS_SUMMARY="$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"
fi
if [[ -z "$CLOSURE_REPORT" ]]; then
  CLOSURE_REPORT="$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md"
fi
if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"
fi

parse_run_id_md() {
  local file="$1"
  local value=""
  value="$(grep -E "^- (Run ID|run_id):" "$file" | head -1 | sed -E 's/^- (Run ID|run_id): *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)"
  echo "$value"
}

read_markdown_scalar_field() {
  local file="$1"
  local key="$2"
  grep -E "^- ${key}:" "$file" | head -1 | sed -E "s/^- ${key}: *//" | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
}

parse_closure_status_md() {
  local file="$1"
  grep -E "^- closure_status:" "$file" | head -1 | sed -E 's/^- closure_status: *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
}

normalize_step_status() {
  local value="$1"
  echo "$value" | tr -d '`*' | tr '[:lower:]' '[:upper:]' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g'
}

read_summary_step_field() {
  local file="$1"
  local step="$2"
  local field="$3"

  awk -F'|' -v want_step="$step" -v want_field="$field" '
    {
      delete cols
      col_count = 0
      for (i = 1; i <= NF; i++) {
        col = $i
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", col)
        if (col == "") {
          continue
        }
        cols[++col_count] = col
      }

      if (col_count < 2) {
        next
      }

      step_col = cols[1]
      if (step_col ~ /^-+$/) {
        next
      }
      if (tolower(step_col) != tolower(want_step)) {
        next
      }

      out = ""
      if (want_field == "status") {
        if (col_count >= 4) {
          out = cols[3]
        } else {
          out = cols[2]
        }
      } else if (want_field == "evidence") {
        if (col_count >= 4) {
          out = cols[4]
        } else if (col_count >= 3) {
          out = cols[3]
        }
      }

      gsub(/\*\*/, "", out)
      print out
      exit
    }
  ' "$file" || true
}

required_missing=0
runid_mismatch=0
linked_evidence_mismatch=0
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

  if [[ "$json_ok" != "YES" ]]; then
    rows+=("| $label | $rel_path | YES | n/a | NO | json_valid=NO |")
    runid_mismatch=$((runid_mismatch + 1))
    return 0
  fi

  local parsed=""
  parsed="$(parse_run_id_json "$abs_path" || true)"
  parsed="${parsed%%$'
'*}"

  local match="n/a"
  local note="json_valid=YES"

  if [[ -n "$parsed" ]]; then
    if [[ "$parsed" == "$RUN_ID" ]]; then
      match="YES"
      note="ok"
    else
      match="NO"
      note="run_id mismatch"
      runid_mismatch=$((runid_mismatch + 1))
    fi
  fi

  rows+=("| $label | $rel_path | YES | ${parsed:-n/a} | $match | $note |")
}

check_cross_summary_artifact() {
  local rel_path="$1"
  local required="$2"
  local abs_path
  abs_path="$(resolve_path "$rel_path")"

  if [[ ! -f "$abs_path" ]]; then
    local missing_note="missing"
    rows+=("| cross_summary | $rel_path | NO | n/a | NO | $missing_note |")
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

  if [[ "$match" == "YES" ]]; then
    local embedded_examples_json=""
    local embedded_examples_selection=""
    local mismatch_notes=()
    local extra_note=""

    embedded_examples_json="$(read_markdown_scalar_field "$abs_path" "linux_examples_json")"
    embedded_examples_selection="$(read_markdown_scalar_field "$abs_path" "linux_examples_selection")"

    if [[ -n "$embedded_examples_json" && "$embedded_examples_json" != "$LINUX_EXAMPLES_JSON" ]]; then
      mismatch_notes+=("linked linux_examples_json mismatch")
    fi
    if [[ -n "$embedded_examples_selection" && "$embedded_examples_selection" != "$LINUX_EXAMPLES_SELECTION" ]]; then
      mismatch_notes+=("linked linux_examples_selection mismatch")
    fi

    if [[ ${#mismatch_notes[@]} -gt 0 ]]; then
      linked_evidence_mismatch=$((linked_evidence_mismatch + 1))
      for extra_note in "${mismatch_notes[@]}"; do
        note="$note; $extra_note"
      done
    fi
  fi

  rows+=("| cross_summary | $rel_path | YES | ${parsed:-n/a} | $match | $note |")
}

check_markdown_artifact "linux_summary" "$LINUX_SUMMARY" true
check_json_artifact "linux_examples_json" "$LINUX_EXAMPLES_JSON" true
check_markdown_artifact "macos_summary" "$MACOS_SUMMARY" false
check_markdown_artifact "windows_summary" "$WINDOWS_SUMMARY" false
check_cross_summary_artifact "$CROSS_SUMMARY" true
check_markdown_artifact "closure_report" "$CLOSURE_REPORT" true

windows_summary_abs="$(resolve_path "$WINDOWS_SUMMARY")"
if [[ -n "$WINDOWS_SUMMARY" && -f "$windows_summary_abs" ]]; then
  blocker_status="$(normalize_step_status "$(read_summary_step_field "$windows_summary_abs" "winssl_blocker_batch" "status")")"
  blocker_evidence="$(read_summary_step_field "$windows_summary_abs" "winssl_blocker_batch" "evidence" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"

  if [[ -n "$blocker_status" ]]; then
    if [[ "$blocker_status" == "SKIP" || "$blocker_status" == "SKIPPED" ]]; then
      rows+=("| windows_blocker_batch_report | <none> | NO | n/a | NO | blocker skipped in windows summary |")
    else
      if [[ -z "$blocker_evidence" ]]; then
        blocker_evidence="$REPORTS_DIR/winssl_blocker_batch_${RUN_ID}.md"
      fi

      if [[ "$blocker_evidence" == "<none>" ]]; then
        rows+=("| windows_blocker_batch_report | <none> | NO | n/a | NO | blocker evidence missing in windows summary |")
        required_missing=$((required_missing + 1))
      else
        check_markdown_artifact "windows_blocker_batch_report" "$blocker_evidence" true
      fi
    fi
  else
    rows+=("| windows_blocker_batch_report | n/a | NO | n/a | NO | blocker row not found in windows summary (legacy layout) |")
  fi
fi

consistency_status="CONSISTENT"
if [[ "$required_missing" -gt 0 || "$runid_mismatch" -gt 0 || "$linked_evidence_mismatch" -gt 0 ]]; then
  consistency_status="INCONSISTENT"
fi

closure_status_note="n/a"
closure_abs="$(resolve_path "$CLOSURE_REPORT")"
if [[ -f "$closure_abs" ]]; then
  closure_status_note="$(parse_closure_status_md "$closure_abs")"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] linux_summary=$LINUX_SUMMARY"
  echo "[DRY-RUN] macos_summary=$MACOS_SUMMARY"
  echo "[DRY-RUN] windows_summary=$WINDOWS_SUMMARY"
  echo "[DRY-RUN] cross_summary=$CROSS_SUMMARY"
  echo "[DRY-RUN] closure_report=$CLOSURE_REPORT"
  echo "[DRY-RUN] required_missing=$required_missing"
  echo "[DRY-RUN] runid_mismatch=$runid_mismatch"
  echo "[DRY-RUN] linked_evidence_mismatch=$linked_evidence_mismatch"
  echo "[DRY-RUN] linux_examples_json=$LINUX_EXAMPLES_JSON"
  echo "[DRY-RUN] linux_examples_selection=$LINUX_EXAMPLES_SELECTION"
  echo "[DRY-RUN] linux_examples_warning=$LINUX_EXAMPLES_WARNING"
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
  echo "- linked_evidence_mismatch: $linked_evidence_mismatch"
  echo "- closure_status_note: $closure_status_note"
  echo "- linux_examples_selection: $LINUX_EXAMPLES_SELECTION"
  echo "- linux_examples_warning: $LINUX_EXAMPLES_WARNING"
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
  echo "- CONSISTENT 条件：required_missing=0 且 runid_mismatch_or_parse_issue=0 且 linked_evidence_mismatch=0"
  echo "- strict 模式：若非 CONSISTENT，脚本返回非 0"
} > "$PROJECT_ROOT/$OUTPUT_FILE"

echo "[PASS] evidence consistency report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$consistency_status" != "CONSISTENT" ]]; then
  exit 1
fi
exit 0
