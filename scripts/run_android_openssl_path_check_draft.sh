#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

NDK_ROOT=""
OPENSSL_ROOT=""
ANDROID_ABI="arm64-v8a"
ANDROID_API_LEVEL="24"
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
WITH_MODULE_TESTS=true
WITH_PHASE2_DRYRUN=true
VERBOSE=false
DRY_RUN=false
FPC_EXE="${FAFAFA_FPC_EXE:-fpc}"
TIMESTAMP="$(date +"%Y%m%d_%H%M%S")"
RUN_ID="${FAFAFA_ANDROID_PATH_CHECK_RUN_ID:-${TIMESTAMP}_$$}"
DEFAULT_MODULE_UNIT_OUTPUT_DIR="tmp/android_path_check_module_units_${RUN_ID}"
MODULE_UNIT_OUTPUT_DIR="${FAFAFA_ANDROID_PATH_CHECK_MODULE_UNIT_OUTPUT_DIR:-$DEFAULT_MODULE_UNIT_OUTPUT_DIR}"
DEFAULT_MODULE_BIN_OUTPUT_DIR="tmp/android_path_check_module_bin_${RUN_ID}"
MODULE_BIN_OUTPUT_DIR="${FAFAFA_ANDROID_PATH_CHECK_MODULE_BIN_OUTPUT_DIR:-$DEFAULT_MODULE_BIN_OUTPUT_DIR}"

NDK_CANDIDATES=(
  "/opt/android-ndk"
  "/opt/android-sdk/ndk-bundle"
  "/usr/local/android-ndk"
)

OPENSSL_CANDIDATES=(
  "/opt/android-openssl"
  "/usr/local/android-openssl"
)

usage() {
  cat <<'USAGE'
Android OpenSSL 路径校验脚本（Draft）

目标：
  为 Android 交叉编译场景提供 OpenSSL/NDK 路径校验与最小命令链。

用法：
  scripts/run_android_openssl_path_check_draft.sh [options]

选项：
  --ndk-root DIR          指定 Android NDK 根目录
  --openssl-root DIR      指定 Android OpenSSL 根目录
  --abi NAME              Android ABI（默认: arm64-v8a）
  --api-level N           Android API level（默认: 24）
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
    --ndk-root)
      NDK_ROOT="$2"
      shift 2
      ;;
    --openssl-root)
      OPENSSL_ROOT="$2"
      shift 2
      ;;
    --abi)
      ANDROID_ABI="$2"
      shift 2
      ;;
    --api-level)
      ANDROID_API_LEVEL="$2"
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

detect_ndk_root() {
  local candidate
  for candidate in "${NDK_CANDIDATES[@]}"; do
    if [[ -d "$candidate" ]]; then
      echo "$candidate"
      return 0
    fi
  done
  return 1
}

detect_openssl_root() {
  local candidate
  for candidate in "${OPENSSL_CANDIDATES[@]}"; do
    if [[ -f "$candidate/include/openssl/ssl.h" ]]; then
      echo "$candidate"
      return 0
    fi
  done
  return 1
}

detect_ndk_toolchain_bin() {
  local ndk_root="$1"
  local host
  for host in "linux-x86_64" "darwin-arm64" "darwin-x86_64"; do
    if [[ -d "$ndk_root/toolchains/llvm/prebuilt/$host/bin" ]]; then
      echo "$ndk_root/toolchains/llvm/prebuilt/$host/bin"
      return 0
    fi
  done
  return 1
}

if [[ -z "$NDK_ROOT" ]]; then
  NDK_ROOT="$(detect_ndk_root || true)"
fi

if [[ -z "$OPENSSL_ROOT" ]]; then
  OPENSSL_ROOT="$(detect_openssl_root || true)"
fi

if [[ -z "$NDK_ROOT" ]]; then
  if [[ "$DRY_RUN" == "true" ]]; then
    NDK_ROOT="/opt/android-ndk"
    echo "[WARN] Android NDK root not auto-detected; dry-run fallback: $NDK_ROOT"
  else
    echo "[FAIL] Android NDK root not detected. Use --ndk-root DIR" >&2
    exit 1
  fi
fi

if [[ -z "$OPENSSL_ROOT" ]]; then
  if [[ "$DRY_RUN" == "true" ]]; then
    OPENSSL_ROOT="/opt/android-openssl"
    echo "[WARN] Android OpenSSL root not auto-detected; dry-run fallback: $OPENSSL_ROOT"
  else
    echo "[FAIL] Android OpenSSL root not detected. Use --openssl-root DIR" >&2
    exit 1
  fi
fi

if ! [[ "$ANDROID_API_LEVEL" =~ ^[0-9]+$ ]]; then
  echo "[FAIL] --api-level must be a positive integer" >&2
  exit 1
fi

NDK_TOOLCHAIN_BIN="$(detect_ndk_toolchain_bin "$NDK_ROOT" || true)"
if [[ "$DRY_RUN" == "false" ]]; then
  if [[ ! -d "$NDK_ROOT" ]]; then
    echo "[FAIL] missing Android NDK root: $NDK_ROOT" >&2
    exit 1
  fi
  if [[ -z "$NDK_TOOLCHAIN_BIN" ]]; then
    echo "[FAIL] missing NDK llvm toolchain bin under: $NDK_ROOT/toolchains/llvm/prebuilt/*/bin" >&2
    exit 1
  fi
  if [[ ! -f "$OPENSSL_ROOT/include/openssl/ssl.h" ]]; then
    echo "[FAIL] missing OpenSSL header: $OPENSSL_ROOT/include/openssl/ssl.h" >&2
    exit 1
  fi
fi

if [[ -z "$NDK_TOOLCHAIN_BIN" ]]; then
  NDK_TOOLCHAIN_BIN="$NDK_ROOT/toolchains/llvm/prebuilt/linux-x86_64/bin"
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

ENV_PREFIX="ANDROID_NDK_ROOT='$NDK_ROOT' OPENSSL_ROOT='$OPENSSL_ROOT' ANDROID_ABI='$ANDROID_ABI' ANDROID_API_LEVEL='$ANDROID_API_LEVEL' PATH='$NDK_TOOLCHAIN_BIN:$PATH'"

run_cmd() {
  local cmd="$1"
  echo "[ANDROID-CHECK] $cmd"
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi
  eval "$cmd"
}

echo "========================================"
echo "fafafa.ssl Android OpenSSL Path Check (Draft)"
echo "========================================"
echo "ndk root: $NDK_ROOT"
echo "openssl root: $OPENSSL_ROOT"
echo "abi: $ANDROID_ABI"
echo "api-level: $ANDROID_API_LEVEL"
echo "modules: $MODULE_SET"
echo "module tests: $WITH_MODULE_TESTS"
echo "phase2 dry-run: $WITH_PHASE2_DRYRUN"
echo "dry-run: $DRY_RUN"

run_cmd "$FPC_EXE -iV"
run_cmd "cd '$PROJECT_ROOT' && $ENV_PREFIX test -d '$NDK_ROOT'"
run_cmd "cd '$PROJECT_ROOT' && $ENV_PREFIX test -f '$OPENSSL_ROOT/include/openssl/ssl.h'"

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

echo "[PASS] android openssl path check draft finished"
