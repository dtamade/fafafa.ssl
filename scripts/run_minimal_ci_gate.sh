#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DRY_RUN=false
VERBOSE=false
WITH_PHASE2_DRYRUN=true
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"

usage() {
  cat <<'USAGE'
最小 CI 门禁脚本（Draft）

目标：
  以最低成本覆盖“可编译 + P2 核心回归 + Phase2 基准入口可用性”。

用法：
  scripts/run_minimal_ci_gate.sh [options]

选项：
  --modules LIST          指定模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --skip-phase2-dryrun    跳过 Phase2 baseline 脚本 dry-run 检查
  --verbose               模块测试启用 verbose
  --dry-run               仅打印命令，不执行
  --help                  显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --modules)
      MODULE_SET="$2"
      shift 2
      ;;
    --skip-phase2-dryrun)
      WITH_PHASE2_DRYRUN=false
      shift
      ;;
    --verbose)
      VERBOSE=true
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

run_cmd() {
  local cmd="$1"
  echo "[GATE] $cmd"
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi
  eval "$cmd"
}

echo "========================================"
echo "fafafa.ssl Minimal CI Gate (Draft)"
echo "========================================"

run_cmd "cd '$PROJECT_ROOT' && python3 scripts/compile_all_modules.py"

module_cmd="cd '$PROJECT_ROOT' && bash scripts/run_all_module_tests.sh --modules $MODULE_SET"
if [[ "$VERBOSE" == "true" ]]; then
  module_cmd="$module_cmd --verbose"
fi
run_cmd "$module_cmd"

if [[ "$WITH_PHASE2_DRYRUN" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && bash scripts/run_phase2_performance_baseline.sh --dry-run --iterations 200 --tls-iterations 50"
fi

echo "[PASS] minimal CI gate finished"
