#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID=""
RUN_ID_EXPLICIT=false
LINUX_SUMMARY=""
LINUX_EXAMPLES=""
MACOS_PROBE=""
MACOS_PROBE_EXPLICIT=false
MACOS_SUMMARY=""
MACOS_SUMMARY_EXPLICIT=false
WINDOWS_SUMMARY=""
WINDOWS_SUMMARY_EXPLICIT=false
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
  --run-id ID                指定 run_id（默认优先从 Linux summary 推导，否则时间戳）
  --linux-summary FILE       Linux summary（默认自动取最新 wave_b_ci_gate_summary_*.md）
  --linux-examples FILE      Linux examples json（默认优先 test-reports/examples_compile_ci_gate_<run_id>.json，fallback 到旧 generic 路径）
  --macos-probe FILE         macOS probe json（可选；默认 test-reports/wave_b_macos_gate_probe_<run_id>.json）
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
      RUN_ID_EXPLICIT=true
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
  grep -E "^- (Run ID|run_id):" "$file" \
    | head -1 \
    | sed -E 's/^- (Run ID|run_id): *//' \
    | tr -d '`*' \
    | sed -E 's/^[[:space:]]+|[[:space:]]+$//g' || true
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

build_shell_command() {
  local parts=()
  local part
  for part in "$@"; do
    parts+=("$(printf '%q' "$part")")
  done
  local IFS=' '
  echo "${parts[*]}"
}

if [[ -z "$LINUX_SUMMARY" ]]; then
  LINUX_SUMMARY="$(cd "$PROJECT_ROOT" && ls -1t test-reports/wave_b_ci_gate_summary_*.md 2>/dev/null | head -1 || true)"
fi
if [[ "$RUN_ID_EXPLICIT" != "true" ]]; then
  RUN_ID="$(infer_run_id_from_linux_summary "$LINUX_SUMMARY")"
fi
if [[ -z "$RUN_ID" ]]; then
  RUN_ID="$(date +%Y%m%d_%H%M%S)"
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

CROSS_SUMMARY="${OUTPUT_DIR}/wave_b_cross_platform_summary_${RUN_ID}.md"
CLOSURE_REPORT="${OUTPUT_DIR}/wave_b_b2_closure_readiness_${RUN_ID}.md"
CONSISTENCY_REPORT="${OUTPUT_DIR}/wave_b_b2_evidence_consistency_${RUN_ID}.md"
BUNDLE_REPORT="${OUTPUT_DIR}/wave_b_b2_handoff_bundle_${RUN_ID}.md"

if [[ -z "$LINUX_SUMMARY" || ! -f "$(resolve_path "$LINUX_SUMMARY")" ]]; then
  echo "[ERROR] linux summary not found: $LINUX_SUMMARY" >&2
  exit 1
fi
if [[ -z "$LINUX_EXAMPLES" ]]; then
  LINUX_EXAMPLES="$(default_linux_examples_json_path)"
fi

MACOS_CROSS_ARGS=()
MACOS_SUMMARY_ARGS=()
MACOS_CONSISTENCY_ARGS=()
WINDOWS_SUMMARY_ARGS=()
WINDOWS_EVIDENCE_ARGS=()
macos_summary_exists=false
if [[ -f "$(resolve_path "$MACOS_SUMMARY")" ]]; then
  macos_summary_exists=true
fi
macos_probe_exists=false
if [[ -f "$(resolve_path "$MACOS_PROBE")" ]]; then
  macos_probe_exists=true
fi
windows_summary_exists=false
if [[ -f "$(resolve_path "$WINDOWS_SUMMARY")" ]]; then
  windows_summary_exists=true
fi

if [[ "$MACOS_SUMMARY_EXPLICIT" == "true" ]]; then
  MACOS_CROSS_ARGS=(--macos-summary "$MACOS_SUMMARY")
  MACOS_SUMMARY_ARGS=(--macos-summary "$MACOS_SUMMARY")
elif [[ "$macos_summary_exists" == "true" ]]; then
  MACOS_CROSS_ARGS=(--macos-summary "$MACOS_SUMMARY")
  MACOS_SUMMARY_ARGS=(--macos-summary "$MACOS_SUMMARY")
elif [[ "$MACOS_PROBE_EXPLICIT" == "true" ]]; then
  MACOS_CROSS_ARGS=(--macos-probe "$MACOS_PROBE")
elif [[ "$macos_probe_exists" == "true" ]]; then
  MACOS_CROSS_ARGS=(--macos-probe "$MACOS_PROBE")
fi

if [[ "$MACOS_PROBE_EXPLICIT" == "true" ]]; then
  MACOS_CONSISTENCY_ARGS=(--macos-probe "$MACOS_PROBE")
elif [[ "$MACOS_SUMMARY_EXPLICIT" != "true" && "$macos_summary_exists" != "true" && "$macos_probe_exists" == "true" ]]; then
  MACOS_CONSISTENCY_ARGS=(--macos-probe "$MACOS_PROBE")
fi

if [[ "$WINDOWS_SUMMARY_EXPLICIT" == "true" || "$windows_summary_exists" == "true" ]]; then
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
  echo "[DRY-RUN] macos_probe=$MACOS_PROBE"
  echo "[DRY-RUN] macos_cross_args=${MACOS_CROSS_ARGS[*]:-<none>}"
  echo "[DRY-RUN] macos_summary_args=${MACOS_SUMMARY_ARGS[*]:-<none>}"
  echo "[DRY-RUN] macos_consistency_args=${MACOS_CONSISTENCY_ARGS[*]:-<none>}"
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
  "${MACOS_CROSS_ARGS[@]}" \
  "${WINDOWS_SUMMARY_ARGS[@]}" \
  --output "$CROSS_SUMMARY"

bash "$PROJECT_ROOT/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  "${MACOS_SUMMARY_ARGS[@]}" \
  "${WINDOWS_SUMMARY_ARGS[@]}" \
  --output "$CLOSURE_REPORT"

bash "$PROJECT_ROOT/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --linux-examples "$LINUX_EXAMPLES" \
  "${MACOS_SUMMARY_ARGS[@]}" \
  "${MACOS_CONSISTENCY_ARGS[@]}" \
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

REPLAY_ARGS=(
  --run-id "$RUN_ID"
  --linux-summary "$LINUX_SUMMARY"
  --linux-examples "$LINUX_EXAMPLES"
)
if [[ "$MACOS_SUMMARY_EXPLICIT" == "true" || "$macos_summary_exists" == "true" ]]; then
  REPLAY_ARGS+=(--macos-summary "$MACOS_SUMMARY")
elif [[ "$MACOS_PROBE_EXPLICIT" == "true" || "$macos_probe_exists" == "true" ]]; then
  REPLAY_ARGS+=(--macos-probe "$MACOS_PROBE")
fi
if [[ "$WINDOWS_SUMMARY_EXPLICIT" == "true" || "$windows_summary_exists" == "true" ]]; then
  REPLAY_ARGS+=(--windows-summary "$WINDOWS_SUMMARY")
fi
REPLAY_ARGS+=(--output-dir "$OUTPUT_DIR" --strict)
REPLAY_COMMAND="$(build_shell_command scripts/prepare_wave_b_b2_handoff_bundle.sh "${REPLAY_ARGS[@]}")"

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
  BUNDLE_ARTIFACTS=(
    "$LINUX_SUMMARY"
    "$LINUX_EXAMPLES"
    "$MACOS_PROBE"
    "$MACOS_SUMMARY"
    "$WINDOWS_SUMMARY"
  )
  if [[ ${#WINDOWS_EVIDENCE_ARGS[@]} -gt 0 ]]; then
    BUNDLE_ARTIFACTS+=(
      "${WINDOWS_EVIDENCE_ARGS[1]}"
      "${WINDOWS_EVIDENCE_ARGS[3]}"
    )
  fi
  BUNDLE_ARTIFACTS+=(
    "$CROSS_SUMMARY"
    "$CLOSURE_REPORT"
    "$CONSISTENCY_REPORT"
  )
  for p in "${BUNDLE_ARTIFACTS[@]}"; do
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
  echo "3. 回填后重新执行 '$REPLAY_COMMAND'。"
} > "$(resolve_path "$BUNDLE_REPORT")"

echo "[PASS] handoff bundle generated: $BUNDLE_REPORT"

if [[ "$STRICT" == "true" ]]; then
  bash "$PROJECT_ROOT/scripts/check_wave_b_b2_evidence_consistency.sh" \
    --run-id "$RUN_ID" \
    --linux-summary "$LINUX_SUMMARY" \
    --linux-examples "$LINUX_EXAMPLES" \
    "${MACOS_SUMMARY_ARGS[@]}" \
    "${MACOS_CONSISTENCY_ARGS[@]}" \
    "${WINDOWS_SUMMARY_ARGS[@]}" \
    "${WINDOWS_EVIDENCE_ARGS[@]}" \
    --cross-summary "$CROSS_SUMMARY" \
    --closure-report "$CLOSURE_REPORT" \
    --strict \
    --dry-run

  bash "$PROJECT_ROOT/scripts/check_wave_b_b2_closure_readiness.sh" \
    --run-id "$RUN_ID" \
    --linux-summary "$LINUX_SUMMARY" \
    "${MACOS_SUMMARY_ARGS[@]}" \
    "${WINDOWS_SUMMARY_ARGS[@]}" \
    --strict \
    --dry-run
fi

exit 0
