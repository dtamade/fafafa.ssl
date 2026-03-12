#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="${FAFAFA_MACOS_PATH_CHECK_RUN_ID:-$(date +%Y%m%d_%H%M%S)_$$}"
MODULE_UNIT_OUTPUT_DIR="${FAFAFA_MACOS_PATH_CHECK_MODULE_UNIT_OUTPUT_DIR:-tmp/macos_path_check_module_units_${RUN_ID}}"
MODULE_BIN_OUTPUT_DIR="${FAFAFA_MACOS_PATH_CHECK_MODULE_BIN_OUTPUT_DIR:-tmp/macos_path_check_module_bin_${RUN_ID}}"

OPENSSL_ROOT=""
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
WITH_MODULE_TESTS=true
WITH_PHASE2_DRYRUN=true
VERBOSE=false
DRY_RUN=false
FPC_EXE="${FAFAFA_FPC_EXE:-fpc}"

OPENSSL_CANDIDATES=(
  "/opt/homebrew/opt/openssl@3"
  "/usr/local/opt/openssl@3"
  "/opt/local/libexec/openssl3"
)

usage() {
  cat <<'USAGE'
macOS OpenSSL 路径校验脚本（Draft）

目标：
  为 macOS 提供可复用的 OpenSSL 路径校验与最小命令链，
  用于验证 brew OpenSSL 路径、环境变量注入、模块回归入口可执行性。

用法：
  scripts/run_macos_openssl_path_check_draft.sh [options]

选项：
  --openssl-root DIR      指定 OpenSSL 根目录（例如 /opt/homebrew/opt/openssl@3）
  --modules LIST          模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --skip-module-tests     跳过模块测试步骤
  --skip-phase2-dryrun    跳过 Phase2 baseline dry-run
  --verbose               传递 --verbose 给模块测试
  --dry-run               仅打印命令，不执行
  --help                  显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --openssl-root)
      OPENSSL_ROOT="$2"
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

detect_openssl_root() {
  local candidate
  for candidate in "${OPENSSL_CANDIDATES[@]}"; do
    if [[ -f "$candidate/lib/libcrypto.dylib" && -f "$candidate/include/openssl/ssl.h" ]]; then
      echo "$candidate"
      return 0
    fi
  done
  return 1
}

if [[ -z "$OPENSSL_ROOT" ]]; then
  OPENSSL_ROOT="$(detect_openssl_root || true)"
fi

if [[ -z "$OPENSSL_ROOT" ]]; then
  if [[ "$DRY_RUN" == "true" ]]; then
    OPENSSL_ROOT="/opt/homebrew/opt/openssl@3"
    echo "[WARN] OpenSSL root not auto-detected; dry-run fallback: $OPENSSL_ROOT"
  else
    echo "[FAIL] OpenSSL root not detected. Use --openssl-root DIR" >&2
    exit 1
  fi
fi

if [[ "$OSTYPE" != darwin* ]]; then
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[WARN] non-macOS environment detected ($OSTYPE), dry-run only"
  else
    echo "[FAIL] this script is intended for macOS (current: $OSTYPE)" >&2
    exit 1
  fi
fi

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

if [[ "$DRY_RUN" == "false" ]]; then
  if [[ ! -f "$OPENSSL_ROOT/lib/libcrypto.dylib" ]]; then
    echo "[FAIL] missing libcrypto.dylib: $OPENSSL_ROOT/lib/libcrypto.dylib" >&2
    exit 1
  fi

  if [[ ! -f "$OPENSSL_ROOT/lib/libssl.dylib" ]]; then
    echo "[FAIL] missing libssl.dylib: $OPENSSL_ROOT/lib/libssl.dylib" >&2
    exit 1
  fi

  if [[ ! -f "$OPENSSL_ROOT/include/openssl/ssl.h" ]]; then
    echo "[FAIL] missing ssl.h: $OPENSSL_ROOT/include/openssl/ssl.h" >&2
    exit 1
  fi
fi

ENV_PREFIX="OPENSSL_ROOT='$OPENSSL_ROOT' DYLD_LIBRARY_PATH='$OPENSSL_ROOT/lib:${DYLD_LIBRARY_PATH:-}' PKG_CONFIG_PATH='$OPENSSL_ROOT/lib/pkgconfig:${PKG_CONFIG_PATH:-}' PATH='$OPENSSL_ROOT/bin:$PATH'"

run_cmd() {
  local cmd="$1"
  echo "[MACOS-CHECK] $cmd"
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi
  eval "$cmd"
}

echo "========================================"
echo "fafafa.ssl macOS OpenSSL Path Check (Draft)"
echo "========================================"
echo "run_id: $RUN_ID"
echo "module unit output dir: $MODULE_UNIT_OUTPUT_DIR"
echo "module bin output dir: $MODULE_BIN_OUTPUT_DIR"
echo "openssl root: $OPENSSL_ROOT"
echo "modules: $MODULE_SET"
echo "module tests: $WITH_MODULE_TESTS"
echo "phase2 dry-run: $WITH_PHASE2_DRYRUN"
echo "dry-run: $DRY_RUN"

run_cmd "$FPC_EXE -iV"
run_cmd "cd '$PROJECT_ROOT' && $ENV_PREFIX openssl version"
run_cmd "cd '$PROJECT_ROOT' && test -f '$OPENSSL_ROOT/lib/libcrypto.dylib'"
run_cmd "cd '$PROJECT_ROOT' && test -f '$OPENSSL_ROOT/lib/libssl.dylib'"
run_cmd "cd '$PROJECT_ROOT' && test -f '$OPENSSL_ROOT/include/openssl/ssl.h'"

if [[ "$WITH_MODULE_TESTS" == "true" ]]; then
  module_cmd="cd '$PROJECT_ROOT' && $ENV_PREFIX FAFAFA_FPC_EXE='$FPC_EXE' FAFAFA_FPC_UNIT_OUTPUT_DIR='$MODULE_UNIT_OUTPUT_DIR' FAFAFA_TEST_BIN_DIR='$MODULE_BIN_OUTPUT_DIR' bash scripts/run_all_module_tests.sh --modules $MODULE_SET"
  if [[ "$VERBOSE" == "true" ]]; then
    module_cmd="$module_cmd --verbose"
  fi
  run_cmd "$module_cmd"
fi

if [[ "$WITH_PHASE2_DRYRUN" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && $ENV_PREFIX bash scripts/run_phase2_performance_baseline.sh --dry-run --iterations 200 --tls-iterations 50"
fi

echo "[PASS] macOS openssl path check draft finished"
