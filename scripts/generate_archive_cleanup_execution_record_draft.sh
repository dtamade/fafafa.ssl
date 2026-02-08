#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RECORD_ID=""
PROFILE="pr"
MODE="dry-run"
ARTIFACT_ROOT="$PROJECT_ROOT/artifacts/ci"
OPERATOR="codex"
COMMAND_TEXT=""
CANDIDATES="0"
SKIPPED_HOLD="0"
DELETED="0"
OUTPUT_FILE=""
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档清理执行记录生成脚本（Draft）

用途：
  生成标准化清理执行记录，便于审阅与审计。

用法：
  scripts/generate_archive_cleanup_execution_record_draft.sh [options]

选项：
  --record-id ID         记录 ID（默认: yyyyMMdd_HHmmss）
  --profile NAME         策略配置（pr|nightly|release，默认: pr）
  --mode NAME            执行模式（dry-run|apply，默认: dry-run）
  --artifact-root DIR    归档根目录（默认: artifacts/ci）
  --operator NAME        操作人/作业名（默认: codex）
  --command TEXT         清理命令文本
  --candidates N         候选数（默认: 0）
  --skipped-hold N       hold 跳过数（默认: 0）
  --deleted N            删除数（默认: 0）
  --output FILE          输出文件（默认: docs/test_reports/ARCHIVE_CLEANUP_EXECUTION_RECORD_<id>.md）
  --dry-run              仅打印将生成的信息，不写文件
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --record-id)
      RECORD_ID="$2"
      shift 2
      ;;
    --profile)
      PROFILE="$2"
      shift 2
      ;;
    --mode)
      MODE="$2"
      shift 2
      ;;
    --artifact-root)
      ARTIFACT_ROOT="$2"
      shift 2
      ;;
    --operator)
      OPERATOR="$2"
      shift 2
      ;;
    --command)
      COMMAND_TEXT="$2"
      shift 2
      ;;
    --candidates)
      CANDIDATES="$2"
      shift 2
      ;;
    --skipped-hold)
      SKIPPED_HOLD="$2"
      shift 2
      ;;
    --deleted)
      DELETED="$2"
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

case "$PROFILE" in
  pr|nightly|release) ;;
  *)
    echo "[FAIL] unsupported profile: $PROFILE" >&2
    exit 1
    ;;
esac

case "$MODE" in
  dry-run|apply) ;;
  *)
    echo "[FAIL] unsupported mode: $MODE" >&2
    exit 1
    ;;
esac

for n in "$CANDIDATES" "$SKIPPED_HOLD" "$DELETED"; do
  if ! [[ "$n" =~ ^[0-9]+$ ]]; then
    echo "[FAIL] numeric fields must be non-negative integers" >&2
    exit 1
  fi
done

if [[ -z "$RECORD_ID" ]]; then
  RECORD_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_CLEANUP_EXECUTION_RECORD_${RECORD_ID}.md"
fi

if [[ -z "$COMMAND_TEXT" ]]; then
  COMMAND_TEXT="bash scripts/cleanup_ci_artifacts_draft.sh --profile $PROFILE --mode $MODE"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] record_id=$RECORD_ID"
  echo "[DRY-RUN] profile=$PROFILE mode=$MODE operator=$OPERATOR"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  echo "[DRY-RUN] candidates=$CANDIDATES skipped_hold=$SKIPPED_HOLD deleted=$DELETED"
  exit 0
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

status="pass"
if [[ "$MODE" == "apply" && "$DELETED" -gt "$CANDIDATES" ]]; then
  status="fail"
fi

cat > "$OUTPUT_FILE" <<EOF_RECORD
# Archive Cleanup Execution Record（Draft）

## 1) Metadata

| field | value |
|------|-------|
| record_id | $RECORD_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| profile | $PROFILE |
| mode | $MODE |
| artifact_root | $ARTIFACT_ROOT |
| operator | $OPERATOR |

## 2) Command

~~~bash
$COMMAND_TEXT
~~~

## 3) Result Summary

| metric | value |
|--------|-------|
| candidates | $CANDIDATES |
| skipped_hold | $SKIPPED_HOLD |
| deleted | $DELETED |
| status | $status |

## 4) Candidate Details

| run_id | age_days | hold | action |
|--------|----------|------|--------|
| <run_id> | <n> | <yes/no> | <keep/delete/skip-hold> |

## 5) Risk Check

- [ ] 不在冻结窗口
- [ ] hold 豁免已核对
- [ ] 关键发布归档未误删

## 6) Attachments

- <cleanup_log_path>
- <manifest_or_backup_path>
EOF_RECORD

echo "[PASS] cleanup execution record generated: $OUTPUT_FILE"
