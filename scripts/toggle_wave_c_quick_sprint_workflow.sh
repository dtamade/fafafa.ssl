#!/usr/bin/env bash

set -euo pipefail

WORKFLOW_ENABLED=".github/workflows/wave-c-quick-sprint-manual.yml"
WORKFLOW_DISABLED=".github/workflows/wave-c-quick-sprint-manual.yml.disabled"

usage() {
  cat <<'USAGE'
Toggle Wave C Quick Sprint Workflow

用法：
  scripts/toggle_wave_c_quick_sprint_workflow.sh <enable|disable|status>

说明：
  enable  - 启用 workflow_dispatch 工作流
  disable - 禁用工作流（回退为 .disabled）
  status  - 显示当前状态
USAGE
}

if [[ $# -ne 1 ]]; then
  usage
  exit 1
fi

action="$1"

case "$action" in
  enable)
    if [[ -f "$WORKFLOW_ENABLED" ]]; then
      echo "[INFO] already enabled: $WORKFLOW_ENABLED"
      exit 0
    fi
    if [[ ! -f "$WORKFLOW_DISABLED" ]]; then
      echo "[ERROR] disabled workflow not found: $WORKFLOW_DISABLED" >&2
      exit 1
    fi
    mv "$WORKFLOW_DISABLED" "$WORKFLOW_ENABLED"
    echo "[PASS] enabled: $WORKFLOW_ENABLED"
    ;;

  disable)
    if [[ -f "$WORKFLOW_DISABLED" ]]; then
      echo "[INFO] already disabled: $WORKFLOW_DISABLED"
      exit 0
    fi
    if [[ ! -f "$WORKFLOW_ENABLED" ]]; then
      echo "[ERROR] enabled workflow not found: $WORKFLOW_ENABLED" >&2
      exit 1
    fi
    mv "$WORKFLOW_ENABLED" "$WORKFLOW_DISABLED"
    echo "[PASS] disabled: $WORKFLOW_DISABLED"
    ;;

  status)
    if [[ -f "$WORKFLOW_ENABLED" ]]; then
      echo "status=ENABLED"
      echo "path=$WORKFLOW_ENABLED"
      exit 0
    fi
    if [[ -f "$WORKFLOW_DISABLED" ]]; then
      echo "status=DISABLED"
      echo "path=$WORKFLOW_DISABLED"
      exit 0
    fi
    echo "status=MISSING"
    echo "path=none"
    exit 1
    ;;

  *)
    usage
    exit 1
    ;;
esac
