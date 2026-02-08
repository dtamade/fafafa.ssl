#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
OUTPUT_JSON=""
STRICT=false

usage() {
  cat <<'USAGE'
Wave C B142 Local Guard Status Export

用途：
  导出 local-first 守护当前状态为 JSON（供告警/看板消费）。

用法：
  scripts/export_wave_c_local_guard_status_json.sh [options]

选项：
  --run-id ID       指定 run_id
  --output FILE     输出 JSON 路径
  --strict          overall_state 非 HEALTHY 返回非 0
  --help            显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --output)
      OUTPUT_JSON="$2"
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

if [[ -z "$OUTPUT_JSON" ]]; then
  OUTPUT_JSON="test-reports/wave_c_b142_local_guard_status_${RUN_ID}.json"
fi

mkdir -p "$(dirname "$OUTPUT_JSON")"

workflow_state="UNKNOWN"
if [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml.disabled" && ! -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="DISABLED"
elif [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="ENABLED"
fi

latest_oncall="$(ls -1t test-reports/wave_c_b129_oncall_check_*.md 2>/dev/null | head -1 || true)"
latest_snapshot="$(ls -1t test-reports/wave_c_b132_local_first_status_snapshot_*.md 2>/dev/null | head -1 || true)"
latest_fullgate="$(ls -1t test-reports/wave_c_b138_pre_ci_reenable_full_gate_*.md 2>/dev/null | head -1 || true)"
latest_consistency="$(ls -1t test-reports/wave_c_b140_local_guard_consistency_*.md 2>/dev/null | head -1 || true)"

extract_marked_state() {
  local file="$1"
  local key="$2"
  if [[ -z "$file" || ! -f "$file" ]]; then
    echo "MISSING"
    return 0
  fi
  local value
  value="$(rg -o "${key}:[[:space:]]*\*\*[A-Z_]+\*\*" "$file" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  echo "${value:-UNKNOWN}"
}

oncall_state="$(extract_marked_state "$latest_oncall" "overall")"
snapshot_state="$(extract_marked_state "$latest_snapshot" "snapshot_state")"
fullgate_state="$(extract_marked_state "$latest_fullgate" "overall")"
consistency_state="$(extract_marked_state "$latest_consistency" "consistency_state")"

overall_state="HEALTHY"
if [[ "$workflow_state" != "DISABLED" || "$oncall_state" != "PASS" || "$snapshot_state" != "GREEN" || "$fullgate_state" != "PASS" || "$consistency_state" != "CONSISTENT" ]]; then
  overall_state="ATTENTION"
fi

cat > "$OUTPUT_JSON" <<JSON
{
  "run_id": "$RUN_ID",
  "generated_at": "$(date '+%Y-%m-%d %H:%M:%S %z')",
  "overall_state": "$overall_state",
  "workflow_state": "$workflow_state",
  "oncall_state": "$oncall_state",
  "snapshot_state": "$snapshot_state",
  "full_gate_state": "$fullgate_state",
  "consistency_state": "$consistency_state",
  "evidence": {
    "oncall_report": "${latest_oncall:-}",
    "snapshot_report": "${latest_snapshot:-}",
    "full_gate_report": "${latest_fullgate:-}",
    "consistency_report": "${latest_consistency:-}"
  }
}
JSON

echo "[INFO] overall_state=$overall_state"
echo "[PASS] status json generated: $OUTPUT_JSON"

echo "WAVE_C_STATUS overall=$overall_state workflow=$workflow_state oncall=$oncall_state snapshot=$snapshot_state fullgate=$fullgate_state consistency=$consistency_state"

if [[ "$STRICT" == "true" && "$overall_state" != "HEALTHY" ]]; then
  exit 1
fi

exit 0
