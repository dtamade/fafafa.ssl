#!/usr/bin/env bash

set -euo pipefail

RUN_ID=""
STRICT=false
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C Post-Trigger Observability Check

用途：
  在 workflow 触发后，核对关键报告/日志产物是否齐全。

用法：
  scripts/check_wave_c_post_trigger_observability.sh --run-id <RUN_ID> [options]

选项：
  --run-id ID      必填，触发 run_id
  --output FILE    输出报告路径
  --strict         非 READY 返回非 0
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

if [[ -z "$RUN_ID" ]]; then
  usage
  exit 1
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="tmp/test-reports/wave_c_b120_post_trigger_observability_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

resolve_wave_c_artifact() {
  local name="$1"
  local candidate=""
  for root in tmp/test-reports test-reports docs/test_reports; do
    candidate="$root/$name"
    if [[ -f "$candidate" ]]; then
      echo "$candidate"
      return 0
    fi
  done
  echo "tmp/test-reports/$name"
}

required=(
  "$(resolve_wave_c_artifact "wave_c_b107_threshold_eval_${RUN_ID}.md")"
  "$(resolve_wave_c_artifact "wave_c_b108_default_on_readiness_${RUN_ID}.md")"
  "$(resolve_wave_c_artifact "wave_c_b109_canary_rollout_${RUN_ID}.md")"
  "$(resolve_wave_c_artifact "wave_c_b110_rollback_drill_${RUN_ID}.md")"
  "$(resolve_wave_c_artifact "wave_c_quick_sprint_bundle_${RUN_ID}.md")"
)

missing=0
rows=""
for f in "${required[@]}"; do
  if [[ -f "$f" ]]; then
    rows+="| $f | PASS |\n"
  else
    rows+="| $f | FAIL |\n"
    missing=1
  fi
done

state="READY"
if [[ "$missing" -ne 0 ]]; then
  state="HOLD"
fi

{
  echo "# Wave C B120 Post-Trigger Observability"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- state: **$state**"
  echo
  echo "## Artifact Checks"
  echo
  echo "| artifact | result |"
  echo "|----------|--------|"
  printf "%b" "$rows"
  echo
  echo "## 15-Minute Ops Checklist"
  echo
  echo "- Confirm workflow job status in GitHub Actions is green."
  echo "- Verify no unexpected WARN/ERROR spikes in generated logs."
  echo "- Confirm rollback drill artifact exists and status PASS."
  echo "- Keep default-off policy unless separate approval is issued."
} > "$OUTPUT_FILE"

echo "[INFO] state=$state"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$state" != "READY" ]]; then
  exit 1
fi

exit 0
