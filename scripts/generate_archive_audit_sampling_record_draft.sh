#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

SAMPLE_ID=""
ARTIFACT_ROOT="$PROJECT_ROOT/artifacts/ci"
PROFILE_FILTER="all"
SAMPLING_METHOD="oldest-first"
SAMPLE_SIZE="3"
RUN_IDS_CSV=""
OPERATOR="codex"
OUTPUT_FILE=""
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计抽样记录生成脚本（Draft）

用途：
  从 CI 归档目录生成可审阅的抽样记录，支持自动抽样与手工指定 run_id。

用法：
  scripts/generate_archive_audit_sampling_record_draft.sh [options]

选项：
  --sample-id ID         抽样记录 ID（默认: yyyyMMdd_HHmmss）
  --artifact-root DIR    归档根目录（默认: artifacts/ci）
  --profile NAME         筛选 profile（all|pr|nightly|release，默认: all）
  --method NAME          抽样方法（oldest-first|newest-first，默认: oldest-first）
  --sample-size N        样本数量（默认: 3）
  --run-ids CSV          手工指定 run_id 列表（逗号分隔，设置后 method 记为 manual）
  --operator NAME        操作人/作业名（默认: codex）
  --output FILE          输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_<id>.md）
  --dry-run              仅打印计划，不写文件
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --sample-id)
      SAMPLE_ID="$2"
      shift 2
      ;;
    --artifact-root)
      ARTIFACT_ROOT="$2"
      shift 2
      ;;
    --profile)
      PROFILE_FILTER="$2"
      shift 2
      ;;
    --method)
      SAMPLING_METHOD="$2"
      shift 2
      ;;
    --sample-size)
      SAMPLE_SIZE="$2"
      shift 2
      ;;
    --run-ids)
      RUN_IDS_CSV="$2"
      shift 2
      ;;
    --operator)
      OPERATOR="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
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

case "$PROFILE_FILTER" in
  all|pr|nightly|release) ;;
  *)
    echo "[FAIL] unsupported profile filter: $PROFILE_FILTER" >&2
    exit 1
    ;;
esac

case "$SAMPLING_METHOD" in
  oldest-first|newest-first) ;;
  *)
    echo "[FAIL] unsupported sampling method: $SAMPLING_METHOD" >&2
    exit 1
    ;;
esac

if ! [[ "$SAMPLE_SIZE" =~ ^[0-9]+$ ]]; then
  echo "[FAIL] --sample-size must be a non-negative integer" >&2
  exit 1
fi

if [[ "$SAMPLE_SIZE" -eq 0 ]]; then
  echo "[FAIL] --sample-size must be >= 1" >&2
  exit 1
fi

if [[ -z "$SAMPLE_ID" ]]; then
  SAMPLE_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_${SAMPLE_ID}.md"
fi

if [[ -n "$RUN_IDS_CSV" ]]; then
  SAMPLING_METHOD="manual"
fi

trim_space() {
  echo "$1" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//'
}

get_mtime_epoch() {
  local target="$1"
  stat -c %Y "$target" 2>/dev/null || stat -f %m "$target" 2>/dev/null || echo 0
}

is_hold_marked() {
  local run_dir="$1"
  if [[ -f "$run_dir/.hold" ]]; then
    echo "yes"
    return
  fi

  if [[ -f "$run_dir/manifest.md" ]] && grep -qiE 'hold\s*[:=]\s*true' "$run_dir/manifest.md"; then
    echo "yes"
    return
  fi

  echo "no"
}

extract_profile() {
  local run_dir="$1"
  local manifest_file="$run_dir/manifest.md"
  local profile="unknown"

  if [[ -f "$manifest_file" ]]; then
    profile="$(grep -Ei '^\| (workflow_profile|profile) \|' "$manifest_file" | head -1 | sed -E 's/^\|[^|]*\|[[:space:]]*//; s/[[:space:]]*\|[[:space:]]*$//' || true)"

    if [[ -z "$profile" ]]; then
      profile="$(grep -Ei '^(workflow_profile|profile)\s*[:=]' "$manifest_file" | head -1 | sed -E 's/^[^:=]+[:=][[:space:]]*//' || true)"
    fi
  fi

  if [[ -z "$profile" ]]; then
    profile="unknown"
  fi

  echo "$profile"
}

build_entry() {
  local run_dir="$1"
  local run_id profile hold_flag manifest_flag age_days mtime_epoch

  run_id="$(basename "$run_dir")"
  profile="$(extract_profile "$run_dir")"
  hold_flag="$(is_hold_marked "$run_dir")"

  if [[ -f "$run_dir/manifest.md" ]]; then
    manifest_flag="yes"
  else
    manifest_flag="no"
  fi

  mtime_epoch="$(get_mtime_epoch "$run_dir")"
  age_days=$(( ($(date +%s) - mtime_epoch) / 86400 ))
  if (( age_days < 0 )); then
    age_days=0
  fi

  echo "$run_id|$profile|$age_days|$hold_flag|$manifest_flag|$run_dir"
}

sampling_command_text="bash scripts/generate_archive_audit_sampling_record_draft.sh --profile $PROFILE_FILTER --method ${SAMPLING_METHOD} --sample-size $SAMPLE_SIZE"
if [[ -n "$RUN_IDS_CSV" ]]; then
  sampling_command_text="$sampling_command_text --run-ids \"$RUN_IDS_CSV\""
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] sample_id=$SAMPLE_ID"
  echo "[DRY-RUN] artifact_root=$ARTIFACT_ROOT"
  echo "[DRY-RUN] profile_filter=$PROFILE_FILTER method=$SAMPLING_METHOD sample_size=$SAMPLE_SIZE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  if [[ -n "$RUN_IDS_CSV" ]]; then
    echo "[DRY-RUN] manual run_ids=$RUN_IDS_CSV"
  fi
  exit 0
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

population_file="$(mktemp)"
selected_file="$(mktemp)"
trap 'rm -f "$population_file" "$selected_file"' EXIT

if [[ -n "$RUN_IDS_CSV" ]]; then
  IFS=',' read -r -a run_ids <<< "$RUN_IDS_CSV"
  for raw_run_id in "${run_ids[@]}"; do
    run_id="$(trim_space "$raw_run_id")"
    [[ -z "$run_id" ]] && continue

    run_dir="$ARTIFACT_ROOT/$run_id"
    if [[ -d "$run_dir" ]]; then
      build_entry "$run_dir" | tee -a "$population_file" >> "$selected_file"
    else
      echo "$run_id|unknown|n/a|unknown|no|$run_dir" | tee -a "$population_file" >> "$selected_file"
    fi
  done
else
  if [[ -d "$ARTIFACT_ROOT" ]]; then
    while IFS= read -r run_dir; do
      [[ -z "$run_dir" ]] && continue
      entry="$(build_entry "$run_dir")"
      profile="$(echo "$entry" | awk -F'|' '{print $2}')"

      if [[ "$PROFILE_FILTER" != "all" && "$profile" != "$PROFILE_FILTER" ]]; then
        continue
      fi

      echo "$entry" >> "$population_file"
    done < <(find "$ARTIFACT_ROOT" -mindepth 1 -maxdepth 1 -type d | sort)
  fi

  if [[ -s "$population_file" ]]; then
    if [[ "$SAMPLING_METHOD" == "oldest-first" ]]; then
      sort -t'|' -k3,3nr "$population_file" | head -n "$SAMPLE_SIZE" > "$selected_file"
    else
      sort -t'|' -k3,3n "$population_file" | head -n "$SAMPLE_SIZE" > "$selected_file"
    fi
  fi
fi

population_size=0
selected_count=0
if [[ -s "$population_file" ]]; then
  population_size="$(wc -l < "$population_file" | tr -d ' ')"
fi
if [[ -s "$selected_file" ]]; then
  selected_count="$(wc -l < "$selected_file" | tr -d ' ')"
fi

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Sampling Record（Draft）

## 1) Metadata

| field | value |
|------|-------|
| sample_id | $SAMPLE_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| artifact_root | $ARTIFACT_ROOT |
| profile_filter | $PROFILE_FILTER |
| sampling_method | $SAMPLING_METHOD |
| sample_size | $SAMPLE_SIZE |
| population_size | $population_size |
| selected_count | $selected_count |
| operator | $OPERATOR |

## 2) Sampling Command

~~~bash
$sampling_command_text
~~~

## 3) Population Snapshot

| profile | run_count | hold_count |
|---------|-----------|------------|
EOF_REPORT

if [[ -s "$population_file" ]]; then
  awk -F'|' '{run[$2]++; if ($4=="yes") hold[$2]++} END {for (p in run) printf("| %s | %d | %d |\n", p, run[p], hold[p]+0)}' "$population_file" | sort >> "$OUTPUT_FILE"
else
  echo "| n/a | 0 | 0 |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 4) Sampled Runs

| run_id | profile | age_days | hold | manifest | source_path |
|--------|---------|----------|------|----------|-------------|
EOF_APPEND

if [[ -s "$selected_file" ]]; then
  while IFS='|' read -r run_id profile age_days hold_flag manifest_flag source_path; do
    echo "| $run_id | $profile | $age_days | $hold_flag | $manifest_flag | $source_path |" >> "$OUTPUT_FILE"
  done < "$selected_file"
else
  echo "| n/a | n/a | n/a | n/a | n/a | no matched runs |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Audit Checklist

- [ ] 抽样来源与筛选条件可复现。
- [ ] hold 样本已记录原因与到期复核日期。
- [ ] 样本归档符合保留策略（B17/B19）。
- [ ] 样本证据可关联 Gate 汇总与清理执行记录。

## 6) Findings & Actions

- findings:
  - <observation_1>
- actions:
  - <followup_action_1>

## 7) Attachments

- <cross_platform_gate_summary_path>
- <cleanup_execution_record_path>
- <audit_log_path>
EOF_APPEND

echo "[PASS] archive audit sampling record generated: $OUTPUT_FILE"
