#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

ARTIFACT_ROOT="$PROJECT_ROOT/artifacts/ci"
RUN_ID=""
RUN_DIR=""
REASON=""
OWNER=""
EXPIRES_ON=""
CLEAR_HOLD=false
DRY_RUN=true

usage() {
  cat <<'USAGE'
CI 归档 hold 标记脚本（Draft）

安全默认：
  - 默认 dry-run
  - 仅 --apply 时写入/删除 hold 标记

用法：
  scripts/mark_ci_artifact_hold_draft.sh [options]

选项：
  --root DIR            归档根目录（默认: artifacts/ci）
  --run-id ID           目标 run_id（与 --run-dir 二选一）
  --run-dir DIR         目标目录（与 --run-id 二选一）
  --reason TEXT         hold 原因（设置 hold 时建议提供）
  --owner NAME          责任人/审批人
  --expires-on DATE     复核日期（YYYY-MM-DD）
  --clear               清除 hold 标记
  --apply               执行写入/删除（默认 dry-run）
  --dry-run             强制 dry-run
  --help                显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --root)
      ARTIFACT_ROOT="$2"
      shift 2
      ;;
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --run-dir)
      RUN_DIR="$2"
      shift 2
      ;;
    --reason)
      REASON="$2"
      shift 2
      ;;
    --owner)
      OWNER="$2"
      shift 2
      ;;
    --expires-on)
      EXPIRES_ON="$2"
      shift 2
      ;;
    --clear)
      CLEAR_HOLD=true
      shift
      ;;
    --apply)
      DRY_RUN=false
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

resolve_input_dir() {
  local path="$1"

  if [[ "$path" == /* ]]; then
    echo "$path"
    return
  fi

  if [[ -d "$path" ]]; then
    echo "$path"
    return
  fi

  if [[ -d "$PROJECT_ROOT/$path" ]]; then
    echo "$PROJECT_ROOT/$path"
    return
  fi

  echo "$path"
}

ARTIFACT_ROOT="$(resolve_input_dir "$ARTIFACT_ROOT")"
if [[ -n "$RUN_DIR" ]]; then
  RUN_DIR="$(resolve_input_dir "$RUN_DIR")"
fi

if [[ -n "$RUN_ID" && -n "$RUN_DIR" ]]; then
  echo "[FAIL] use either --run-id or --run-dir, not both" >&2
  exit 1
fi

if [[ -z "$RUN_ID" && -z "$RUN_DIR" ]]; then
  echo "[FAIL] one of --run-id or --run-dir is required" >&2
  exit 1
fi

if [[ -z "$RUN_DIR" ]]; then
  RUN_DIR="$ARTIFACT_ROOT/$RUN_ID"
else
  RUN_ID="$(basename "$RUN_DIR")"
fi

if [[ ! -d "$RUN_DIR" ]]; then
  echo "[FAIL] target run directory not found: $RUN_DIR" >&2
  exit 1
fi

HOLD_FILE="$RUN_DIR/.hold"
META_FILE="$RUN_DIR/.hold.meta"

if [[ "$CLEAR_HOLD" == "false" ]]; then
  if [[ -z "$REASON" ]]; then
    REASON="manual_hold"
  fi
  if [[ -z "$OWNER" ]]; then
    OWNER="unknown"
  fi
fi

echo "========================================"
echo "fafafa.ssl Artifact Hold Marker (Draft)"
echo "========================================"
echo "run_id: $RUN_ID"
echo "run_dir: $RUN_DIR"
echo "mode: $([[ "$CLEAR_HOLD" == "true" ]] && echo clear || echo set)"
echo "dry-run: $DRY_RUN"

if [[ "$CLEAR_HOLD" == "true" ]]; then
  echo "[PLAN] remove: $HOLD_FILE"
  echo "[PLAN] remove: $META_FILE"

  if [[ "$DRY_RUN" == "false" ]]; then
    rm -f "$HOLD_FILE" "$META_FILE"
    echo "[APPLY] hold cleared"
  fi

  echo "[PASS] hold clear flow finished"
  exit 0
fi

cat <<EOF_META
[PLAN] set hold with metadata:
  reason: $REASON
  owner: $OWNER
  expires_on: ${EXPIRES_ON:-n/a}
EOF_META

if [[ "$DRY_RUN" == "false" ]]; then
  date_now="$(date '+%Y-%m-%d %H:%M:%S %z')"

  cat > "$HOLD_FILE" <<EOF_HOLD
hold=true
run_id=$RUN_ID
set_at=$date_now
EOF_HOLD

  cat > "$META_FILE" <<EOF_META_FILE
run_id: $RUN_ID
reason: $REASON
owner: $OWNER
expires_on: ${EXPIRES_ON:-n/a}
set_at: $date_now
EOF_META_FILE

  echo "[APPLY] hold marker written: $HOLD_FILE"
  echo "[APPLY] hold metadata written: $META_FILE"
fi

echo "[PASS] hold set flow finished"
