#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
OUTPUT_FILE=""
LIMIT=20

usage() {
  cat <<'USAGE'
Wave C B126 Local Guard History Summary

用途：
  汇总最近若干次 B125 本地守护结果，输出趋势摘要与异常提示。

用法：
  scripts/summarize_wave_c_local_guard_history.sh [options]

选项：
  --run-id ID       指定 run_id
  --limit N         最多扫描最近 N 份 bundle 报告（默认 20）
  --output FILE     输出报告路径
  --strict          存在 FAIL 记录时返回非 0
  --help            显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --limit)
      LIMIT="$2"
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
  OUTPUT_FILE="test-reports/wave_c_b126_local_guard_history_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

mapfile -t reports < <(ls -1t test-reports/wave_c_b125_local_guard_bundle_*.md 2>/dev/null | head -n "$LIMIT" || true)

total=0
pass_count=0
fail_count=0
latest_state="NONE"
rows=""

for report in "${reports[@]}"; do
  total=$((total + 1))

  overall="UNKNOWN"
  if rg -q "overall:[[:space:]]*\*\*PASS\*\*" "$report"; then
    overall="PASS"
    pass_count=$((pass_count + 1))
  elif rg -q "overall:[[:space:]]*\*\*FAIL\*\*" "$report"; then
    overall="FAIL"
    fail_count=$((fail_count + 1))
  fi

  run_id_val="$(rg -o "run_id:[[:space:]]*[0-9_]+" "$report" | head -1 | sed -E 's/.*run_id:[[:space:]]*//' || true)"
  run_id_val="${run_id_val:-unknown}"

  if [[ $total -eq 1 ]]; then
    latest_state="$overall"
  fi

  rows+="| $run_id_val | $overall | $report |\n"
done

trend_state="NO_DATA"
if [[ $total -gt 0 ]]; then
  if [[ $fail_count -eq 0 ]]; then
    trend_state="STABLE"
  else
    trend_state="DEGRADED"
  fi
fi

{
  echo "# Wave C B126 Local Guard History Summary"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- trend_state: **$trend_state**"
  echo
  echo "## Aggregate"
  echo
  echo "- scanned_reports: $total"
  echo "- pass_count: $pass_count"
  echo "- fail_count: $fail_count"
  echo "- latest_state: $latest_state"
  echo
  echo "## Recent Bundles"
  echo
  echo "| run_id | overall | report |"
  echo "|--------|---------|--------|"
  if [[ $total -gt 0 ]]; then
    printf "%b" "$rows"
  else
    echo "| <none> | NO_DATA | <none> |"
  fi
  echo
  echo "## Recommendation"
  echo
  if [[ "$trend_state" == "STABLE" ]]; then
    echo "- local-first guard trend is stable; keep daily B125 strict checks."
  elif [[ "$trend_state" == "DEGRADED" ]]; then
    echo "- local-first guard trend degraded; inspect latest FAIL bundle logs immediately."
  else
    echo "- no history yet; run B125 first to bootstrap trend data."
  fi
} > "$OUTPUT_FILE"

echo "[INFO] trend_state=$trend_state"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$fail_count" -gt 0 ]]; then
  exit 1
fi

exit 0
