#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
LINUX_SUMMARY=""
LINUX_EXAMPLES="test-reports/examples_compile_ci_gate.json"
MACOS_SUMMARY=""
WINDOWS_SUMMARY=""
CROSS_SUMMARY=""
CLOSURE_REPORT=""
CONSISTENCY_REPORT=""
OUTPUT_DIR="test-reports"
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
  --linux-summary FILE       Linux summary（默认自动取最新 wave_b_ci_gate_summary_*.md）
  --linux-examples FILE      Linux examples json（默认 test-reports/examples_compile_ci_gate.json）
  --macos-summary FILE       macOS summary（可选）
  --windows-summary FILE     Windows summary（可选）
  --output-dir DIR           输出目录（默认 test-reports）
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
    --linux-summary)
      LINUX_SUMMARY="$2"
      shift 2
      ;;
    --linux-examples)
      LINUX_EXAMPLES="$2"
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

if [[ -z "$LINUX_SUMMARY" ]]; then
  LINUX_SUMMARY="$(cd "$PROJECT_ROOT" && ls -1t test-reports/wave_b_ci_gate_summary_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$MACOS_SUMMARY" ]]; then
  MACOS_SUMMARY="test-reports/wave_b_macos_gate_summary_${RUN_ID}.md"
fi
if [[ -z "$WINDOWS_SUMMARY" ]]; then
  WINDOWS_SUMMARY="test-reports/wave_b_windows_gate_summary_${RUN_ID}.md"
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

if [[ -z "$LINUX_SUMMARY" || ! -f "$(resolve_path "$LINUX_SUMMARY")" ]]; then
  echo "[ERROR] linux summary not found: $LINUX_SUMMARY" >&2
  exit 1
fi

MACOS_ARGS=()
WINDOWS_SUMMARY_ARGS=()
WINDOWS_EVIDENCE_ARGS=()
if [[ -f "$(resolve_path "$MACOS_SUMMARY")" ]]; then
  MACOS_ARGS=(--macos-summary "$MACOS_SUMMARY")
fi
if [[ -f "$(resolve_path "$WINDOWS_SUMMARY")" ]]; then
  WINDOWS_SUMMARY_ARGS=(--windows-summary "$WINDOWS_SUMMARY")
  WINDOWS_EVIDENCE_ARGS=(
    --windows-quick-log "$(derive_sibling_artifact_path "$WINDOWS_SUMMARY" "winssl_quick_smoke_${RUN_ID}.log")"
    --windows-runtime-transcript "$(derive_sibling_artifact_path "$WINDOWS_SUMMARY" "winssl_runtime_suite_${RUN_ID}.log")"
  )
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] linux_summary=$LINUX_SUMMARY"
  echo "[DRY-RUN] linux_examples=$LINUX_EXAMPLES"
  echo "[DRY-RUN] macos_args=${MACOS_ARGS[*]:-<none>}"
  echo "[DRY-RUN] windows_summary_args=${WINDOWS_SUMMARY_ARGS[*]:-<none>}"
  echo "[DRY-RUN] windows_evidence_args=${WINDOWS_EVIDENCE_ARGS[*]:-<none>}"
  echo "[DRY-RUN] output_dir=$OUTPUT_DIR"
  echo "[DRY-RUN] strict=$STRICT"
  exit 0
fi

mkdir -p "$(resolve_path "$OUTPUT_DIR")"

bash "$PROJECT_ROOT/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --linux-examples "$LINUX_EXAMPLES" \
  "${MACOS_ARGS[@]}" \
  "${WINDOWS_SUMMARY_ARGS[@]}" \
  --output "$CROSS_SUMMARY"

bash "$PROJECT_ROOT/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  "${MACOS_ARGS[@]}" \
  "${WINDOWS_SUMMARY_ARGS[@]}" \
  --output "$CLOSURE_REPORT"

bash "$PROJECT_ROOT/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --linux-examples "$LINUX_EXAMPLES" \
  "${MACOS_ARGS[@]}" \
  "${WINDOWS_SUMMARY_ARGS[@]}" \
  "${WINDOWS_EVIDENCE_ARGS[@]}" \
  --cross-summary "$CROSS_SUMMARY" \
  --closure-report "$CLOSURE_REPORT" \
  --output "$CONSISTENCY_REPORT"

closure_status="$(grep -E '^- closure_status:' "$(resolve_path "$CLOSURE_REPORT")" | head -1 | sed -E 's/^- closure_status: *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)"
consistency_status="$(grep -E '^- consistency_status:' "$(resolve_path "$CONSISTENCY_REPORT")" | head -1 | sed -E 's/^- consistency_status: *//' | tr -d '`*' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true)"

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
  echo
  echo "## Next Actions"
  echo
  echo "1. 在 macOS runner 执行 live gate 并回填 macOS summary。"
  echo "2. 在 Windows runner 执行 live gate 并回填 Windows summary。"
  echo "3. 回填后重新执行 'scripts/prepare_wave_b_b2_handoff_bundle.sh --run-id $RUN_ID --strict'。"
} > "$(resolve_path "$BUNDLE_REPORT")"

echo "[PASS] handoff bundle generated: $BUNDLE_REPORT"

if [[ "$STRICT" == "true" ]]; then
  bash "$PROJECT_ROOT/scripts/check_wave_b_b2_evidence_consistency.sh" \
    --run-id "$RUN_ID" \
    --linux-summary "$LINUX_SUMMARY" \
    --linux-examples "$LINUX_EXAMPLES" \
    "${MACOS_ARGS[@]}" \
    "${WINDOWS_SUMMARY_ARGS[@]}" \
    "${WINDOWS_EVIDENCE_ARGS[@]}" \
    --cross-summary "$CROSS_SUMMARY" \
    --closure-report "$CLOSURE_REPORT" \
    --strict \
    --dry-run

  bash "$PROJECT_ROOT/scripts/check_wave_b_b2_closure_readiness.sh" \
    --run-id "$RUN_ID" \
    --linux-summary "$LINUX_SUMMARY" \
    "${MACOS_ARGS[@]}" \
    "${WINDOWS_SUMMARY_ARGS[@]}" \
    --strict \
    --dry-run
fi

exit 0
