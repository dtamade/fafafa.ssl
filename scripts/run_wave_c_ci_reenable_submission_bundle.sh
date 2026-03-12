#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMMON_LIB="$SCRIPT_DIR/wave_c_audit_note_common.sh"
if [[ ! -f "$COMMON_LIB" ]]; then
  echo "[ERROR] common lib not found: $COMMON_LIB" >&2
  exit 1
fi
# shellcheck source=/dev/null
source "$COMMON_LIB"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
OUTPUT_FILE=""
DEFAULT_REPORTS_DIR="tmp/wave_c_ci_reenable_reports"
REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"
WITH_LOCAL_GUARD_BATCH=true
WITH_DOCS_GOVERNANCE=true
ONLY_PLATFORM_PATH_CHECK_DRYRUN=false
OVERRIDE_B147_PROJECTED_AUDIT_NOTE=""

usage() {
  cat <<'USAGE'
Wave C B149 CI Re-enable Submission Bundle

用途：
  一次执行 B144C/B146/B147/B148 并输出恢复 CI 审批提交打包报告。

用法：
  scripts/run_wave_c_ci_reenable_submission_bundle.sh [options]

选项：
  --run-id ID      指定 run_id
  --output FILE    输出报告路径（默认 tmp/wave_c_ci_reenable_reports/wave_c_b149_ci_reenable_submission_bundle_<run_id>.md）
  --reports-dir DIR  子报告目录（默认 tmp/wave_c_ci_reenable_reports）
  --skip-local-guard-batch  跳过 B144C local guard skip-matrix batch
  --only-platform-path-check-dryrun  将 B144C local guard batch 切换为平台路径检查 only 模式
  --skip-docs-governance     跳过 B149D docs 治理 strict batch
  --override-b147-projected-audit-note VALUE  覆盖 B149 聚合阶段读取到的 b147 projected audit note（合同测试用）
  --strict         任一步骤失败返回非 0
  --help           显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --reports-dir)
      REPORTS_DIR="$2"
      shift 2
      ;;
    --skip-local-guard-batch)
      WITH_LOCAL_GUARD_BATCH=false
      shift
      ;;
    --only-platform-path-check-dryrun)
      ONLY_PLATFORM_PATH_CHECK_DRYRUN=true
      shift
      ;;
    --skip-docs-governance)
      WITH_DOCS_GOVERNANCE=false
      shift
      ;;
    --override-b147-projected-audit-note)
      OVERRIDE_B147_PROJECTED_AUDIT_NOTE="$2"
      shift 2
      ;;
    --strict)
      STRICT=true
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

if [[ -n "$OVERRIDE_B147_PROJECTED_AUDIT_NOTE" ]]; then
  if ! wave_c_is_allowed_audit_note "$OVERRIDE_B147_PROJECTED_AUDIT_NOTE"; then
    echo "[ERROR] invalid value for --override-b147-projected-audit-note: $OVERRIDE_B147_PROJECTED_AUDIT_NOTE" >&2
    echo "[ERROR] allowed values: $(wave_c_allowed_audit_notes_csv)" >&2
    exit 1
  fi
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b149_ci_reenable_submission_bundle_${RUN_ID}.md"
fi

mkdir -p "$REPORTS_DIR" "$(dirname "$OUTPUT_FILE")"

b146_report="$REPORTS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_ID}.md"
b147_report="$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_ID}.md"
b148_report="$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.md"

run_step() {
  local cmd="$1"
  local log="$2"

  set +e
  eval "$cmd" > "$log" 2>&1
  local ec=$?
  set -e

  echo "$ec"
}

extract_marked_state() {
  local file="$1"
  local key="$2"
  if [[ ! -f "$file" ]]; then
    echo "MISSING"
    return 0
  fi

  local value
  value="$(rg -o "${key}:[[:space:]]*\*\*[A-Z0-9_]+\*\*" "$file" | head -1 | sed -E 's/.*\*\*([A-Z0-9_]+)\*\*/\1/' || true)"
  echo "${value:-UNKNOWN}"
}

b146_log="$REPORTS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_ID}.b149.log"
b147_log="$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_ID}.b149.log"
b148_log="$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.b149.log"
b144c_local_guard_batch_log="$REPORTS_DIR/wave_c_b144_local_guard_batch_${RUN_ID}.b149.log"
b149d_docs_governance_log="$REPORTS_DIR/wave_c_docs_governance_batch_${RUN_ID}.b149.log"

b144c_local_guard_batch_exit="SKIP"
b144c_local_guard_batch_log_display="<none>"
if [[ "$WITH_LOCAL_GUARD_BATCH" == "true" ]]; then
  b144c_local_guard_batch_cmd="bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh"
  if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
    b144c_local_guard_batch_cmd="$b144c_local_guard_batch_cmd --only-platform-path-check-dryrun"
  fi
  b144c_local_guard_batch_exit=$(run_step "$b144c_local_guard_batch_cmd" "$b144c_local_guard_batch_log")
  b144c_local_guard_batch_log_display="$b144c_local_guard_batch_log"
fi

b149d_docs_governance_exit="SKIP"
b149d_docs_governance_log_display="<none>"
if [[ "$WITH_DOCS_GOVERNANCE" == "true" ]]; then
  b149d_docs_governance_exit=$(run_step "bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh" "$b149d_docs_governance_log")
  b149d_docs_governance_log_display="$b149d_docs_governance_log"
fi

b146_exit=$(run_step "bash scripts/prepare_wave_c_ci_reenable_submission_pack.sh --run-id ${RUN_ID} --strict --output ${b146_report}" "$b146_log")
b147_exit=$(run_step "bash scripts/check_wave_c_ci_reenable_submission_pack.sh --run-id ${RUN_ID} --strict --input ${b146_report} --output ${b147_report}" "$b147_log")
b148_exit=$(run_step "bash scripts/generate_wave_c_ci_reenable_approval_brief.sh --run-id ${RUN_ID} --input ${b146_report} --check ${b147_report} --output ${b148_report}" "$b148_log")

overall="PASS"
if [[ "$b144c_local_guard_batch_exit" != "0" && "$b144c_local_guard_batch_exit" != "SKIP" ]]; then
  overall="FAIL"
fi
if [[ "$b149d_docs_governance_exit" != "0" && "$b149d_docs_governance_exit" != "SKIP" ]]; then
  overall="FAIL"
fi
if [[ "$b146_exit" != "0" || "$b147_exit" != "0" || "$b148_exit" != "0" ]]; then
  overall="FAIL"
fi

submission_state="$(extract_marked_state "$b146_report" "submission_state")"
check_state="$(extract_marked_state "$b147_report" "check_state")"
brief_submission_state="$(extract_marked_state "$b148_report" "submission_state")"
b148_alert_state="$(extract_marked_state "$b148_report" "alert_state")"
b149_audit_alert_note="$(wave_c_map_alert_state_to_audit_note "$b148_alert_state")"

b147_projected_b149_audit_alert_note="$(extract_marked_state "$b147_report" "projected_b149_audit_alert_note")"
b147_projected_audit_note_source="B147_REPORT"
b147_projected_audit_note_override_value="NONE"
if [[ -n "$OVERRIDE_B147_PROJECTED_AUDIT_NOTE" ]]; then
  b147_projected_b149_audit_alert_note="$OVERRIDE_B147_PROJECTED_AUDIT_NOTE"
  b147_projected_audit_note_source="OVERRIDE"
  b147_projected_audit_note_override_value="$OVERRIDE_B147_PROJECTED_AUDIT_NOTE"
fi
b149_audit_alert_note_sync_state="$(wave_c_compute_audit_note_sync_state "$b147_projected_b149_audit_alert_note" "$b149_audit_alert_note")"

b148_b149_audit_alert_note_preview="$(extract_marked_state "$b148_report" "b149_audit_alert_note_preview")"
b148_b149_audit_alert_note_sync_state="$(extract_marked_state "$b148_report" "b149_audit_alert_note_sync_state")"
b147_b148_b149_audit_note_consistency="$(wave_c_compute_audit_note_chain_consistency "$b147_projected_b149_audit_alert_note" "$b148_b149_audit_alert_note_preview" "$b149_audit_alert_note")"

b144c_local_guard_bundle_mode="FULL"
b144c_local_guard_option_resolution="RUN_FULL"
if [[ "$WITH_LOCAL_GUARD_BATCH" == "false" ]]; then
  b144c_local_guard_bundle_mode="SKIPPED"
  b144c_local_guard_option_resolution="SKIP_LOCAL_GUARD_BATCH"
  if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
    b144c_local_guard_option_resolution="SKIP_LOCAL_GUARD_BATCH_ONLY_FLAG_IGNORED"
  fi
else
  if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
    b144c_local_guard_bundle_mode="PLATFORM_ONLY"
    b144c_local_guard_option_resolution="RUN_PLATFORM_ONLY"
  fi
fi

{
  echo "# Wave C B149 CI Re-enable Submission Bundle"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- overall: **$overall**"
  echo "- submission_state: **$submission_state**"
  echo "- check_state: **$check_state**"
  echo "- audit_alert_note: **$b149_audit_alert_note**"
  echo "- projected_audit_note_source: **$b147_projected_audit_note_source**"
  echo "- projected_audit_note_override_value: **$b147_projected_audit_note_override_value**"
  echo
  echo "## Step Matrix"
  echo
  echo "| step | exit | output | log |"
  echo "|------|------|--------|-----|"
  echo "| B144C local guard skip-matrix batch | $b144c_local_guard_batch_exit | <none> | $b144c_local_guard_batch_log_display |"
  echo "| B149D docs governance strict batch | $b149d_docs_governance_exit | <none> | $b149d_docs_governance_log_display |"
  echo "| B146 submission pack | $b146_exit | $b146_report | $b146_log |"
  echo "| B147 pack check | $b147_exit | $b147_report | $b147_log |"
  echo "| B148 approval brief | $b148_exit | $b148_report | $b148_log |"
  echo
  echo "## Summary"
  echo
  echo "- b146_submission_state: $submission_state"
  echo "- b147_check_state: $check_state"
  echo "- b147_projected_audit_note_source: $b147_projected_audit_note_source"
  echo "- b147_projected_audit_note_override_value: $b147_projected_audit_note_override_value"
  echo "- b147_projected_b149_audit_alert_note: $b147_projected_b149_audit_alert_note"
  echo "- b148_submission_state: $brief_submission_state"
  echo "- b148_alert_state: $b148_alert_state"
  echo "- b148_b149_audit_alert_note_sync_state: $b148_b149_audit_alert_note_sync_state"
  echo "- b149_audit_alert_note: $b149_audit_alert_note"
  echo "- b149_audit_alert_note_sync_state: $b149_audit_alert_note_sync_state"
  echo "- b147_b148_b149_audit_note_consistency: $b147_b148_b149_audit_note_consistency"
  echo "- b144c_local_guard_bundle_mode: $b144c_local_guard_bundle_mode"
  echo "- b144c_local_guard_option_resolution: $b144c_local_guard_option_resolution"
  echo "- boundary: 保持 workflow disabled，待审批后再执行 enable。"
} > "$OUTPUT_FILE"

echo "[INFO] overall=$overall"
echo "[PASS] submission bundle report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
