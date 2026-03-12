#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"
LINUX_SUMMARY=""
LINUX_EXAMPLES=""
LINUX_EXAMPLES_EXPLICIT_ARG=false
LINUX_EXAMPLES_SELECTION="run_scoped_missing"
LINUX_EXAMPLES_WARNING="none"
RESOLVED_LINUX_EXAMPLES=""
MACOS_SUMMARY=""
WINDOWS_SUMMARY=""
CROSS_SUMMARY=""
CLOSURE_REPORT=""
CONSISTENCY_REPORT=""
OUTPUT_DIR=""
BUNDLE_REPORT=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
Wave B / B2 Handoff Bundle Preparer

用途：
  生成 B2 交接包：cross summary + closure readiness + evidence consistency + handoff index。

用法：
  scripts/prepare_wave_b_b2_handoff_bundle.sh [options]

选项：
  --run-id ID                指定 run_id（默认时间戳）
  --reports-dir DIR          输入报告目录（默认 tmp/wave_b_reports）
  --linux-summary FILE       Linux summary（默认自动取最新 wave_b_ci_gate_summary_*.md）
  --linux-examples FILE      Linux examples json（默认 tmp/wave_b_reports/examples_compile_ci_gate.json）
  --macos-summary FILE       macOS summary（可选）
  --windows-summary FILE     Windows summary（可选）
  --output-dir DIR           输出目录（默认 tmp/wave_b_reports）
  --strict                   启用 strict 门禁判定（未闭环即非 0）
  --dry-run                  仅打印计划，不写文件
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
      LINUX_EXAMPLES="$2"
      LINUX_EXAMPLES_EXPLICIT_ARG=true
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
    --output-dir)
      OUTPUT_DIR="$2"
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

if [[ -z "$OUTPUT_DIR" ]]; then
  OUTPUT_DIR="$REPORTS_DIR"
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
    RESOLVED_LINUX_EXAMPLES="$run_scoped_rel"
    return 0
  fi

  if [[ -f "$fallback_abs" ]]; then
    parsed_run_id="$(parse_run_id_json "$fallback_abs" || true)"
    parsed_run_id="${parsed_run_id%%$'
'*}"
    if [[ -n "$parsed_run_id" && "$parsed_run_id" == "$RUN_ID" ]]; then
      LINUX_EXAMPLES_SELECTION="static_same_run_fallback"
      RESOLVED_LINUX_EXAMPLES="$fallback_rel"
      return 0
    fi
  fi

  LINUX_EXAMPLES_SELECTION="run_scoped_missing"
  RESOLVED_LINUX_EXAMPLES="$run_scoped_rel"
}

if [[ -n "$LINUX_EXAMPLES" ]]; then
  LINUX_EXAMPLES_SELECTION="explicit_override"
elif [[ -n "${FAFAFA_WAVE_B_EXAMPLES_REPORT_REL:-}" ]]; then
  LINUX_EXAMPLES="$FAFAFA_WAVE_B_EXAMPLES_REPORT_REL"
  LINUX_EXAMPLES_SELECTION="explicit_override"
else
  resolve_preferred_examples_artifact "$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json" "$REPORTS_DIR/examples_compile_ci_gate.json"
  LINUX_EXAMPLES="$RESOLVED_LINUX_EXAMPLES"
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
if [[ -z "$MACOS_SUMMARY" ]]; then
  MACOS_SUMMARY="$REPORTS_DIR/wave_b_macos_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$WINDOWS_SUMMARY" ]]; then
  WINDOWS_SUMMARY="$REPORTS_DIR/wave_b_windows_gate_summary_${RUN_ID}.md"
fi

CROSS_SUMMARY="${OUTPUT_DIR}/wave_b_cross_platform_summary_${RUN_ID}.md"
CLOSURE_REPORT="${OUTPUT_DIR}/wave_b_b2_closure_readiness_${RUN_ID}.md"
CONSISTENCY_REPORT="${OUTPUT_DIR}/wave_b_b2_evidence_consistency_${RUN_ID}.md"
BUNDLE_REPORT="${OUTPUT_DIR}/wave_b_b2_handoff_bundle_${RUN_ID}.md"

resolve_path() {
  local file="$1"
  if [[ "$file" = /* ]]; then
    echo "$file"
  else
    echo "$PROJECT_ROOT/$file"
  fi
}

read_consistency_artifact_field() {
  local file="$1"
  local artifact="$2"
  local field="$3"

  awk -F'|' -v want_artifact="$artifact" -v want_field="$field" '
    {
      if (NF < 4) {
        next
      }
      a = $2
      p = $3
      e = $4
      r = $5
      m = $6
      n = $7
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", a)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", p)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", e)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", r)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", m)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", n)
      if (a == "artifact" || a ~ /^-+$/) {
        next
      }
      if (a != want_artifact) {
        next
      }

      if (want_field == "path") {
        print p
      } else if (want_field == "exists") {
        print e
      } else if (want_field == "parsed_run_id") {
        print r
      } else if (want_field == "run_id_match") {
        print m
      } else if (want_field == "note") {
        print n
      }
      exit
    }
  ' "$file" || true
}

read_consistency_metric_field() {
  local file="$1"
  local metric="$2"
  grep -E "^- ${metric}:" "$file" | head -1 | sed -E "s/^- ${metric}: *//" | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
}

render_consistency_snapshot_row() {
  local artifact="$1"
  local report_file="$2"

  local path
  local exists
  local run_id_match
  local note

  path="$(read_consistency_artifact_field "$report_file" "$artifact" "path" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"
  exists="$(read_consistency_artifact_field "$report_file" "$artifact" "exists" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' | tr '[:lower:]' '[:upper:]')"
  run_id_match="$(read_consistency_artifact_field "$report_file" "$artifact" "run_id_match" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' | tr '[:lower:]' '[:upper:]')"
  note="$(read_consistency_artifact_field "$report_file" "$artifact" "note" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"

  if [[ -z "$path" ]]; then
    path="<none>"
  fi
  if [[ -z "$exists" ]]; then
    exists="NO"
  fi
  if [[ -z "$run_id_match" ]]; then
    run_id_match="NO"
  fi
  if [[ -z "$note" ]]; then
    note="artifact row not found in consistency report"
  fi

  echo "| $artifact | $path | $exists | $run_id_match | $note |"
}

should_emit_consistency_alert_row() {
  local artifact="$1"
  local note="$2"

  local note_lc
  note_lc="$(echo "$note" | tr '[:upper:]' '[:lower:]' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"

  if [[ -z "$note_lc" || "$note_lc" == "ok" || "$note_lc" == "n/a" ]]; then
    return 1
  fi
  if [[ "$note_lc" == *"skipped in windows summary"* ]]; then
    return 1
  fi
  if [[ "$artifact" == "windows_summary" && "$note_lc" == "missing" ]]; then
    return 1
  fi
  return 0
}

extract_markdown_section_body() {
  local file="$1"
  local heading="$2"

  awk -v want_heading="$heading" '
    $0 == want_heading {
      in_section = 1
      next
    }
    in_section && /^## / {
      exit
    }
    in_section {
      print
    }
  ' "$file" || true
}

if [[ -z "$LINUX_SUMMARY" || ! -f "$(resolve_path "$LINUX_SUMMARY")" ]]; then
  echo "[ERROR] linux summary not found: $LINUX_SUMMARY" >&2
  exit 1
fi

LINUX_EXAMPLES_ARGS=()
if [[ "$LINUX_EXAMPLES_EXPLICIT_ARG" == "true" ]]; then
  LINUX_EXAMPLES_ARGS=(--linux-examples "$LINUX_EXAMPLES")
fi

MACOS_ARGS=()
WINDOWS_ARGS=()
if [[ -f "$(resolve_path "$MACOS_SUMMARY")" ]]; then
  MACOS_ARGS=(--macos-summary "$MACOS_SUMMARY")
fi
if [[ -f "$(resolve_path "$WINDOWS_SUMMARY")" ]]; then
  WINDOWS_ARGS=(--windows-summary "$WINDOWS_SUMMARY")
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] linux_summary=$LINUX_SUMMARY"
  echo "[DRY-RUN] macos_summary=$MACOS_SUMMARY"
  echo "[DRY-RUN] windows_summary=$WINDOWS_SUMMARY"
  echo "[DRY-RUN] linux_examples_json=$LINUX_EXAMPLES"
  echo "[DRY-RUN] linux_examples_selection=$LINUX_EXAMPLES_SELECTION"
  echo "[DRY-RUN] linux_examples_warning=$LINUX_EXAMPLES_WARNING"
  echo "[DRY-RUN] cross_summary=$CROSS_SUMMARY"
  echo "[DRY-RUN] closure_report=$CLOSURE_REPORT"
  echo "[DRY-RUN] consistency_report=$CONSISTENCY_REPORT"
  echo "[DRY-RUN] bundle_report=$BUNDLE_REPORT"
  echo "[DRY-RUN] macos_args=${MACOS_ARGS[*]:-<none>}"
  echo "[DRY-RUN] windows_args=${WINDOWS_ARGS[*]:-<none>}"
  echo "[DRY-RUN] output_dir=$OUTPUT_DIR"
  echo "[DRY-RUN] strict=$STRICT"
  exit 0
fi

mkdir -p "$(resolve_path "$OUTPUT_DIR")"

FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_DIR" bash "$PROJECT_ROOT/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  "${LINUX_EXAMPLES_ARGS[@]}" \
  "${MACOS_ARGS[@]}" \
  "${WINDOWS_ARGS[@]}" \
  --output "$CROSS_SUMMARY"

FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_DIR" bash "$PROJECT_ROOT/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  "${MACOS_ARGS[@]}" \
  "${WINDOWS_ARGS[@]}" \
  --output "$CLOSURE_REPORT"

FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_DIR" bash "$PROJECT_ROOT/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  "${LINUX_EXAMPLES_ARGS[@]}" \
  "${MACOS_ARGS[@]}" \
  "${WINDOWS_ARGS[@]}" \
  --cross-summary "$CROSS_SUMMARY" \
  --closure-report "$CLOSURE_REPORT" \
  --output "$CONSISTENCY_REPORT"

closure_status="$(grep -E '^- closure_status:' "$(resolve_path "$CLOSURE_REPORT")" | head -1 | sed -E 's/^- closure_status: *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)"
consistency_status="$(grep -E '^- consistency_status:' "$(resolve_path "$CONSISTENCY_REPORT")" | head -1 | sed -E 's/^- consistency_status: *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)"
windows_blocker_report_path="$(read_consistency_artifact_field "$(resolve_path "$CONSISTENCY_REPORT")" "windows_blocker_batch_report" "path" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"
windows_blocker_report_exists="$(read_consistency_artifact_field "$(resolve_path "$CONSISTENCY_REPORT")" "windows_blocker_batch_report" "exists" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"
windows_blocker_report_exists="$(echo "$windows_blocker_report_exists" | tr '[:lower:]' '[:upper:]')"
closure_semantics_snapshot="$(extract_markdown_section_body "$(resolve_path "$CLOSURE_REPORT")" "## Status Semantics")"
if [[ -z "$(echo "$closure_semantics_snapshot" | tr -d '[:space:]')" ]]; then
  closure_semantics_snapshot="- Status semantics unavailable in closure report."
fi
closure_next_actions_snapshot="$(extract_markdown_section_body "$(resolve_path "$CLOSURE_REPORT")" "## Next Actions")"
if [[ -z "$(echo "$closure_next_actions_snapshot" | tr -d '[:space:]')" ]]; then
  closure_next_actions_snapshot="- Next actions unavailable in closure report."
fi

consistency_required_missing="$(read_consistency_metric_field "$(resolve_path "$CONSISTENCY_REPORT")" "required_missing")"
consistency_runid_mismatch="$(read_consistency_metric_field "$(resolve_path "$CONSISTENCY_REPORT")" "runid_mismatch_or_parse_issue")"
consistency_linked_evidence_mismatch="$(read_consistency_metric_field "$(resolve_path "$CONSISTENCY_REPORT")" "linked_evidence_mismatch")"
if [[ ! "$consistency_required_missing" =~ ^[0-9]+$ ]]; then
  consistency_required_missing="0"
fi
if [[ ! "$consistency_runid_mismatch" =~ ^[0-9]+$ ]]; then
  consistency_runid_mismatch="0"
fi
if [[ ! "$consistency_linked_evidence_mismatch" =~ ^[0-9]+$ ]]; then
  consistency_linked_evidence_mismatch="0"
fi

consistency_alert_state="CLEAR"
if (( consistency_required_missing > 0 || consistency_runid_mismatch > 0 || consistency_linked_evidence_mismatch > 0 )); then
  consistency_alert_state="WARN"
fi

consistency_alert_rows=()
for artifact in linux_summary cross_summary closure_report windows_summary windows_blocker_batch_report; do
  alert_exists="$(read_consistency_artifact_field "$(resolve_path "$CONSISTENCY_REPORT")" "$artifact" "exists" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' | tr '[:lower:]' '[:upper:]')"
  alert_run_id_match="$(read_consistency_artifact_field "$(resolve_path "$CONSISTENCY_REPORT")" "$artifact" "run_id_match" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' | tr '[:lower:]' '[:upper:]')"
  alert_note="$(read_consistency_artifact_field "$(resolve_path "$CONSISTENCY_REPORT")" "$artifact" "note" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"

  if [[ -z "$alert_exists" ]]; then
    alert_exists="NO"
  fi
  if [[ -z "$alert_run_id_match" ]]; then
    alert_run_id_match="NO"
  fi
  if [[ -z "$alert_note" ]]; then
    alert_note="artifact row not found in consistency report"
  fi

  if should_emit_consistency_alert_row "$artifact" "$alert_note"; then
    consistency_alert_rows+=("| $artifact | $alert_exists | $alert_run_id_match | $alert_note |")
  fi
done

handoff_state="READY_FOR_RUNNER"
if [[ "$consistency_status" == "INCONSISTENT" ]]; then
  handoff_state="NEEDS_EVIDENCE_SYNC"
fi
if [[ "$closure_status" == "CLOSED" && "$consistency_status" == "CONSISTENT" ]]; then
  handoff_state="CLOSED"
fi

{
  echo "# Wave B / B2 Handoff Bundle"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- handoff_state: **$handoff_state**"
  echo "- closure_status: $closure_status"
  echo "- consistency_status: $consistency_status"
  echo "- strict_mode: $STRICT"
  echo "- linux_examples_selection: $LINUX_EXAMPLES_SELECTION"
  echo "- linux_examples_warning: $LINUX_EXAMPLES_WARNING"
  echo
  echo "## Consistency Alert Summary"
  echo
  echo "- source: $CONSISTENCY_REPORT"
  echo "- required_missing: $consistency_required_missing"
  echo "- runid_mismatch_or_parse_issue: $consistency_runid_mismatch"
  echo "- linked_evidence_mismatch: $consistency_linked_evidence_mismatch"
  echo "- alert_state: **$consistency_alert_state**"
  echo
  echo "| artifact | exists | run_id_match | note |"
  echo "|----------|--------|--------------|------|"
  if [[ "${#consistency_alert_rows[@]}" -eq 0 ]]; then
    echo "| <none> | n/a | n/a | no blocking inconsistency in key artifacts |"
  else
    for row in "${consistency_alert_rows[@]}"; do
      echo "$row"
    done
  fi
  echo
  echo "## Artifacts"
  echo
  echo "| artifact | path | exists |"
  echo "|----------|------|--------|"
  for p in "$LINUX_SUMMARY" "$LINUX_EXAMPLES" "$MACOS_SUMMARY" "$WINDOWS_SUMMARY" "$CROSS_SUMMARY" "$CLOSURE_REPORT" "$CONSISTENCY_REPORT"; do
    if [[ -f "$(resolve_path "$p")" ]]; then
      echo "| $(basename "$p") | $p | YES |"
    else
      echo "| $(basename "$p") | $p | NO |"
    fi
  done
  if [[ -n "$windows_blocker_report_path" ]]; then
    if [[ "$windows_blocker_report_exists" != "YES" ]]; then
      windows_blocker_report_exists="NO"
    fi
    echo "| windows_blocker_batch_report | $windows_blocker_report_path | $windows_blocker_report_exists |"
  fi
  echo
  echo "## Consistency Artifact Snapshot"
  echo
  echo "- source: $CONSISTENCY_REPORT"
  echo
  echo "| artifact | path | exists | run_id_match | note |"
  echo "|----------|------|--------|--------------|------|"
  render_consistency_snapshot_row "linux_summary" "$(resolve_path "$CONSISTENCY_REPORT")"
  render_consistency_snapshot_row "cross_summary" "$(resolve_path "$CONSISTENCY_REPORT")"
  render_consistency_snapshot_row "closure_report" "$(resolve_path "$CONSISTENCY_REPORT")"
  render_consistency_snapshot_row "windows_summary" "$(resolve_path "$CONSISTENCY_REPORT")"
  render_consistency_snapshot_row "windows_blocker_batch_report" "$(resolve_path "$CONSISTENCY_REPORT")"
  echo
  echo "## Closure Semantics Snapshot"
  echo
  echo "- source: $CLOSURE_REPORT"
  echo
  printf '%s\n' "$closure_semantics_snapshot"
  echo
  echo "## Closure Next Actions Snapshot"
  echo
  echo "- source: $CLOSURE_REPORT"
  echo
  printf '%s\n' "$closure_next_actions_snapshot"
  echo
  echo "## Next Actions"
  echo
  echo "1. 在 macOS runner 执行 live gate 并回填 macOS summary。"
  echo "2. 在 Windows runner 执行 live gate 并回填 Windows summary。"
  echo "3. 回填后重新执行 'scripts/prepare_wave_b_b2_handoff_bundle.sh --run-id $RUN_ID --strict'。"
} > "$(resolve_path "$BUNDLE_REPORT")"

echo "[PASS] handoff bundle generated: $BUNDLE_REPORT"

if [[ "$STRICT" == "true" ]]; then
  FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_DIR" bash "$PROJECT_ROOT/scripts/check_wave_b_b2_evidence_consistency.sh" \
    --run-id "$RUN_ID" \
    --linux-summary "$LINUX_SUMMARY" \
    "${LINUX_EXAMPLES_ARGS[@]}" \
    "${MACOS_ARGS[@]}" \
    "${WINDOWS_ARGS[@]}" \
    --cross-summary "$CROSS_SUMMARY" \
    --closure-report "$CLOSURE_REPORT" \
    --strict \
    --dry-run

  FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_DIR" bash "$PROJECT_ROOT/scripts/check_wave_b_b2_closure_readiness.sh" \
    --run-id "$RUN_ID" \
    --linux-summary "$LINUX_SUMMARY" \
    "${MACOS_ARGS[@]}" \
    "${WINDOWS_ARGS[@]}" \
    --strict \
    --dry-run
fi

exit 0
