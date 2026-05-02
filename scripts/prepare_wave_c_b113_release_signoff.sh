#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
REPORTS_DIR="tmp/test-reports"
THRESHOLD_REPORT=""
READINESS_REPORT=""
ROLLOUT_REPORT=""
ROLLBACK_REPORT=""
BUNDLE_REPORT=""
OUTPUT_FILE=""
RELEASE_SCOPE="Wave C cert verify cache rollout governance"
APPROVAL_STATE=""

usage() {
  cat <<'USAGE'
Wave C B113 Release Signoff Builder

用途：
  基于 B107/B108/B109/B110 + quick sprint bundle 证据，生成结构化 signoff record。

用法：
  scripts/prepare_wave_c_b113_release_signoff.sh [options]

选项：
  --run-id ID             指定 run_id
  --reports-dir DIR       证据报告目录（默认: tmp/test-reports）
  --threshold-report FILE 指定 B107 报告
  --readiness-report FILE 指定 B108 报告
  --rollout-report FILE   指定 B109 报告
  --rollback-report FILE  指定 B110 报告
  --bundle-report FILE    指定 quick sprint bundle 报告
  --output FILE           输出记录路径（默认: docs/test_reports/WAVE_C_B113_RELEASE_SIGNOFF_RECORD_<run_id>.md）
  --release-scope TEXT    release_scope 文案
  --approval-state STATE  显式指定审批状态（默认: READY_FOR_APPROVAL/BLOCKED）
  --help                  显示帮助
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
    --threshold-report)
      THRESHOLD_REPORT="$2"
      shift 2
      ;;
    --readiness-report)
      READINESS_REPORT="$2"
      shift 2
      ;;
    --rollout-report)
      ROLLOUT_REPORT="$2"
      shift 2
      ;;
    --rollback-report)
      ROLLBACK_REPORT="$2"
      shift 2
      ;;
    --bundle-report)
      BUNDLE_REPORT="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --release-scope)
      RELEASE_SCOPE="$2"
      shift 2
      ;;
    --approval-state)
      APPROVAL_STATE="$2"
      shift 2
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

if [[ -z "$THRESHOLD_REPORT" ]]; then
  THRESHOLD_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b107_threshold_eval_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$READINESS_REPORT" ]]; then
  READINESS_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b108_default_on_readiness_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$ROLLOUT_REPORT" ]]; then
  ROLLOUT_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b109_canary_rollout_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$ROLLBACK_REPORT" ]]; then
  ROLLBACK_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b110_rollback_drill_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$BUNDLE_REPORT" ]]; then
  BUNDLE_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_quick_sprint_bundle_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="docs/test_reports/WAVE_C_B113_RELEASE_SIGNOFF_RECORD_${RUN_ID}.md"
fi

extract_marked_value() {
  local file="$1"
  local key="$2"
  grep -E "${key}:[[:space:]]*\*\*[A-Z_]+\*\*" "$file" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*.*/\1/'
}

THRESHOLD_STATE="$(extract_marked_value "$THRESHOLD_REPORT" "overall")"
READINESS_STATE="$(extract_marked_value "$READINESS_REPORT" "readiness")"
ROLLOUT_STATE="$(extract_marked_value "$ROLLOUT_REPORT" "rollout_state")"
ROLLBACK_STATE="$(extract_marked_value "$ROLLBACK_REPORT" "drill_status")"
BUNDLE_STATE="$(extract_marked_value "$BUNDLE_REPORT" "overall")"

SIGNOFF_STATE="READY_FOR_APPROVAL"
ALLOW_CANARY_EXECUTION="YES"
if [[ "$THRESHOLD_STATE" != "PASS" || "$READINESS_STATE" != "READY" || "$ROLLOUT_STATE" != "CANARY_READY" || "$ROLLBACK_STATE" != "PASS" || "$BUNDLE_STATE" != "PASS" ]]; then
  SIGNOFF_STATE="BLOCKED"
  ALLOW_CANARY_EXECUTION="NO"
fi

if [[ -n "$APPROVAL_STATE" ]]; then
  SIGNOFF_STATE="$APPROVAL_STATE"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

{
  echo "# Wave C B113 Release Signoff Record"
  echo
  echo "## Signoff Metadata"
  echo
  echo "- signoff_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- release_scope: $RELEASE_SCOPE"
  echo "- default_policy: DEFAULT_OFF"
  echo "- signoff_state: $SIGNOFF_STATE"
  echo
  echo "## Required Evidence"
  echo
  echo "| gate | required | evidence | status |"
  echo "|------|----------|----------|--------|"
  echo "| B107 threshold | PASS | \`$THRESHOLD_REPORT\` | $THRESHOLD_STATE |"
  echo "| B108 readiness | READY | \`$READINESS_REPORT\` | $READINESS_STATE |"
  echo "| B109 canary plan | CANARY_READY | \`$ROLLOUT_REPORT\` | $ROLLOUT_STATE |"
  echo "| B110 rollback drill | PASS | \`$ROLLBACK_REPORT\` | $ROLLBACK_STATE |"
  echo "| Quick sprint bundle | PASS | \`$BUNDLE_REPORT\` | $BUNDLE_STATE |"
  echo
  echo "## Risk Decision"
  echo
  echo "- allow_canary_execution: $ALLOW_CANARY_EXECUTION"
  echo "- allow_default_on_switch: NO"
  echo "- rollback_owner: release-manager (TBD)"
  echo "- incident_contact: oncall-secops (TBD)"
  echo
  echo "## Approval"
  echo
  echo "- approver_name: pending-human-approval"
  echo "- approver_role: pending"
  echo "- approval_time: pending"
  echo "- comments: Pending explicit human approval. Technical evidence chain is ready if signoff_state=READY_FOR_APPROVAL."
} > "$OUTPUT_FILE"

echo "[INFO] signoff_state=$SIGNOFF_STATE"
echo "[PASS] signoff record generated: $OUTPUT_FILE"
