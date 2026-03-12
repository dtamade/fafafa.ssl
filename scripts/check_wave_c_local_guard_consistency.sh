#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
OUTPUT_FILE=""
STRICT=false
REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"

usage() {
  cat <<'USAGE'
Wave C B140 Local Guard Consistency Check

用途：
  检查 local-first 守护核心脚本/文档/索引一致性。

用法：
  scripts/check_wave_c_local_guard_consistency.sh [options]

选项：
  --run-id ID      指定 run_id
  --output FILE    输出报告路径（默认 tmp/wave_c_local_guard_reports/wave_c_b140_local_guard_consistency_<run_id>.md）
  --strict         非 CONSISTENT 返回非 0
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

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b140_local_guard_consistency_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

required_scripts=(
  "scripts/check_wave_c_local_first_continuity.sh"
  "scripts/check_wave_c_local_drift_watch.sh"
  "scripts/run_wave_c_local_first_guard_bundle.sh"
  "scripts/summarize_wave_c_local_guard_history.sh"
  "scripts/run_wave_c_local_guard_oncall_check.sh"
  "scripts/generate_wave_c_local_first_status_snapshot.sh"
  "scripts/prepare_wave_c_b137_pre_ci_reenable_packet.sh"
  "scripts/run_wave_c_pre_ci_reenable_full_gate.sh"
)

required_docs=(
  "docs/test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md"
  "docs/test_reports/WAVE_C_B130_ONCALL_RHYTHM_TEMPLATE_2026-02-09.md"
  "docs/test_reports/WAVE_C_B136_DELIVERABLES_OVERVIEW_2026-02-09.md"
  "docs/test_reports/WAVE_C_B137_PRE_CI_REENABLE_PACKET_RESULT_2026-02-09.md"
  "docs/test_reports/WAVE_C_B138_PRE_CI_REENABLE_FULL_GATE_RESULT_2026-02-09.md"
)

missing=0
script_rows=""
for f in "${required_scripts[@]}"; do
  if [[ -f "$f" ]]; then
    script_rows+="| $f | PASS |\n"
  else
    script_rows+="| $f | FAIL |\n"
    missing=1
  fi
done

doc_rows=""
for f in "${required_docs[@]}"; do
  if [[ -f "$f" ]]; then
    doc_rows+="| $f | PASS |\n"
  else
    doc_rows+="| $f | FAIL |\n"
    missing=1
  fi
done

index_check="PASS"
for token in "B137" "B138" "run_wave_c_pre_ci_reenable_full_gate"; do
  if ! rg -q "$token" docs/DOCUMENTATION_INDEX.md; then
    index_check="FAIL"
    missing=1
  fi
done

workflow_state="UNKNOWN"
if [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml.disabled" && ! -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="DISABLED"
else
  workflow_state="NOT_DISABLED"
  missing=1
fi

consistency_state="CONSISTENT"
if [[ "$missing" -ne 0 ]]; then
  consistency_state="DRIFT"
fi

{
  echo "# Wave C B140 Local Guard Consistency"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- consistency_state: **$consistency_state**"
  echo
  echo "## Script Checks"
  echo
  echo "| script | result |"
  echo "|--------|--------|"
  printf "%b" "$script_rows"
  echo
  echo "## Document Checks"
  echo
  echo "| document | result |"
  echo "|----------|--------|"
  printf "%b" "$doc_rows"
  echo
  echo "## Global Checks"
  echo
  echo "| check | value | result |"
  echo "|------|-------|--------|"
  echo "| documentation_index_tokens | B137/B138/script | $index_check |"
  echo "| workflow_state | $workflow_state | $([[ "$workflow_state" == "DISABLED" ]] && echo PASS || echo FAIL) |"
} > "$OUTPUT_FILE"

echo "[INFO] consistency_state=$consistency_state"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$consistency_state" != "CONSISTENT" ]]; then
  exit 1
fi

exit 0
