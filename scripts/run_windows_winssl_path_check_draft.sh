#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

MSYS2_ROOT="C:/msys64"
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
WITH_MODULE_TESTS=true
WITH_PHASE2_DRYRUN=true
VERBOSE=false
DRY_RUN=false
FPC_EXE="${FAFAFA_FPC_EXE:-fpc}"
TIMESTAMP="$(date +"%Y%m%d_%H%M%S")"
RUN_ID="${FAFAFA_WINDOWS_PATH_CHECK_RUN_ID:-${TIMESTAMP}_$$}"
DEFAULT_MODULE_UNIT_OUTPUT_DIR="tmp/windows_path_check_module_units_${RUN_ID}"
MODULE_UNIT_OUTPUT_DIR="${FAFAFA_WINDOWS_PATH_CHECK_MODULE_UNIT_OUTPUT_DIR:-$DEFAULT_MODULE_UNIT_OUTPUT_DIR}"
DEFAULT_MODULE_BIN_OUTPUT_DIR="tmp/windows_path_check_module_bin_${RUN_ID}"
MODULE_BIN_OUTPUT_DIR="${FAFAFA_WINDOWS_PATH_CHECK_MODULE_BIN_OUTPUT_DIR:-$DEFAULT_MODULE_BIN_OUTPUT_DIR}"

usage() {
  cat <<'USAGE'
Windows WinSSL 路径校验脚本（Draft）

目标：
  为 Windows 提供 WinSSL/OpenSSL 最小命令链校验入口，
  支持在非 Windows 环境做 dry-run 验证。

用法：
  scripts/run_windows_winssl_path_check_draft.sh [options]

选项：
  --msys2-root DIR         指定 MSYS2 根目录（默认: C:/msys64）
  --modules LIST           模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --skip-module-tests      跳过模块测试步骤
  --skip-phase2-dryrun     跳过 Phase2 baseline dry-run
  --verbose                传递 --verbose 给模块测试
  --dry-run                仅打印命令，不执行
  --help                   显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --msys2-root)
      MSYS2_ROOT="$2"
      shift 2
      ;;
    --modules)
      MODULE_SET="$2"
      shift 2
      ;;
    --skip-module-tests)
      WITH_MODULE_TESTS=false
      shift
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
    --help|-h)
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

if [[ "$OSTYPE" != msys* && "$OSTYPE" != cygwin* && "$OSTYPE" != win32* ]]; then
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[WARN] non-Windows environment detected ($OSTYPE), dry-run only"
  else
    echo "[FAIL] this script is intended for Windows/MSYS2 (current: $OSTYPE)" >&2
    exit 1
  fi
fi

POWERSHELL_EXE="pwsh"
if ! command -v "$POWERSHELL_EXE" >/dev/null 2>&1; then
  if command -v powershell >/dev/null 2>&1; then
    POWERSHELL_EXE="powershell"
  elif [[ "$DRY_RUN" == "true" ]]; then
    echo "[WARN] pwsh/powershell not found; dry-run command preview will use '$POWERSHELL_EXE' placeholder"
  else
    echo "[FAIL] missing PowerShell host: pwsh/powershell" >&2
    exit 1
  fi
fi

if [[ "$WITH_MODULE_TESTS" == "true" ]]; then
  if [[ "$FPC_EXE" == */* ]]; then
    if [[ "$FPC_EXE" != /* ]]; then
      FPC_EXE="$PROJECT_ROOT/$FPC_EXE"
    fi
    if [[ ! -x "$FPC_EXE" ]]; then
      if [[ "$DRY_RUN" == "true" ]]; then
        echo "[WARN] configured FPC executable is not executable: $FPC_EXE (dry-run continues)"
      else
        echo "[FAIL] configured FPC executable is not executable: $FPC_EXE" >&2
        exit 1
      fi
    fi
  else
    if ! command -v "$FPC_EXE" >/dev/null 2>&1; then
      if [[ "$DRY_RUN" == "true" ]]; then
        echo "[WARN] FPC executable not found in PATH: $FPC_EXE (dry-run continues)"
      else
        echo "[FAIL] FPC executable not found in PATH: $FPC_EXE" >&2
        exit 1
      fi
    fi
  fi
fi

run_cmd() {
  local cmd="$1"
  echo "[WINDOWS-CHECK] $cmd"
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi
  eval "$cmd"
}

ENV_PREFIX="MSYS2_ROOT='$MSYS2_ROOT'"

echo "========================================"
echo "fafafa.ssl Windows WinSSL Path Check (Draft)"
echo "========================================"
echo "msys2 root: $MSYS2_ROOT"
echo "modules: $MODULE_SET"
echo "module tests: $WITH_MODULE_TESTS"
echo "phase2 dry-run: $WITH_PHASE2_DRYRUN"
echo "dry-run: $DRY_RUN"

if [[ "$DRY_RUN" == "false" ]]; then
  if [[ ! -d "$MSYS2_ROOT" ]]; then
    echo "[FAIL] missing MSYS2 root: $MSYS2_ROOT" >&2
    exit 1
  fi
fi

run_cmd "cd '$PROJECT_ROOT' && $ENV_PREFIX $POWERSHELL_EXE -NoProfile -Command '\$PSVersionTable.PSVersion'"
run_cmd "cd '$PROJECT_ROOT' && $ENV_PREFIX $POWERSHELL_EXE -NoProfile -File scripts/run_tests_windows.ps1"
run_cmd "cd '$PROJECT_ROOT' && $ENV_PREFIX $POWERSHELL_EXE -NoProfile -File scripts/build_examples_windows.ps1"

if [[ "$WITH_MODULE_TESTS" == "true" ]]; then
  run_cmd "$FPC_EXE -iV"
  module_cmd="cd '$PROJECT_ROOT' && $ENV_PREFIX FAFAFA_FPC_EXE='$FPC_EXE' FAFAFA_FPC_UNIT_OUTPUT_DIR='$MODULE_UNIT_OUTPUT_DIR' FAFAFA_TEST_BIN_DIR='$MODULE_BIN_OUTPUT_DIR' bash scripts/run_all_module_tests.sh --modules $MODULE_SET"
  if [[ "$VERBOSE" == "true" ]]; then
    module_cmd="$module_cmd --verbose"
  fi
  run_cmd "$module_cmd"
fi

if [[ "$WITH_PHASE2_DRYRUN" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && $ENV_PREFIX bash scripts/run_phase2_performance_baseline.sh --dry-run --iterations 200 --tls-iterations 50"
fi

echo "[PASS] windows winssl path check draft finished"
