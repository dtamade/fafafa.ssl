#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

OPENSSL_ROOT=""
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
WITH_MODULE_TESTS=true
WITH_PHASE2_DRYRUN=true
VERBOSE=false
DRY_RUN=false

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

shell_join() {
  local parts=()
  local part
  for part in "$@"; do
    parts+=("$(printf '%q' "$part")")
  done
  local IFS=' '
  echo "${parts[*]}"
}

build_display_command() {
  shell_join "$@"
}

build_project_command() {
  echo "cd $(printf '%q' "$PROJECT_ROOT") && $(shell_join "$@")"
}

env_assignments=(
  "OPENSSL_ROOT=$OPENSSL_ROOT"
  "DYLD_LIBRARY_PATH=$OPENSSL_ROOT/lib:${DYLD_LIBRARY_PATH:-}"
  "PKG_CONFIG_PATH=$OPENSSL_ROOT/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
  "PATH=$OPENSSL_ROOT/bin:$PATH"
)

run_cmd() {
  local cmd_desc="$1"
  shift
  echo "[MACOS-CHECK] $cmd_desc"
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi
  "$@"
}

run_project_cmd() {
  local cmd_desc="$1"
  shift
  echo "[MACOS-CHECK] $cmd_desc"
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi
  (
    cd "$PROJECT_ROOT"
    "$@"
  )
}

echo "========================================"
echo "fafafa.ssl macOS OpenSSL Path Check (Draft)"
echo "========================================"
echo "openssl root: $OPENSSL_ROOT"
echo "modules: $MODULE_SET"
echo "module tests: $WITH_MODULE_TESTS"
echo "phase2 dry-run: $WITH_PHASE2_DRYRUN"
echo "dry-run: $DRY_RUN"

fpc_cmd_words=(fpc -iV)
run_cmd "$(build_display_command "${fpc_cmd_words[@]}")" "${fpc_cmd_words[@]}"

openssl_cmd_words=(env "${env_assignments[@]}" openssl version)
run_project_cmd "$(build_project_command "${openssl_cmd_words[@]}")" "${openssl_cmd_words[@]}"

check_libcrypto_words=(test -f "$OPENSSL_ROOT/lib/libcrypto.dylib")
run_project_cmd "$(build_project_command "${check_libcrypto_words[@]}")" "${check_libcrypto_words[@]}"

check_libssl_words=(test -f "$OPENSSL_ROOT/lib/libssl.dylib")
run_project_cmd "$(build_project_command "${check_libssl_words[@]}")" "${check_libssl_words[@]}"

check_ssl_header_words=(test -f "$OPENSSL_ROOT/include/openssl/ssl.h")
run_project_cmd "$(build_project_command "${check_ssl_header_words[@]}")" "${check_ssl_header_words[@]}"

if [[ "$WITH_MODULE_TESTS" == "true" ]]; then
  module_cmd_words=(env "${env_assignments[@]}" bash scripts/run_all_module_tests.sh --modules "$MODULE_SET")
  if [[ "$VERBOSE" == "true" ]]; then
    module_cmd_words+=(--verbose)
  fi
  run_project_cmd "$(build_project_command "${module_cmd_words[@]}")" "${module_cmd_words[@]}"
fi

if [[ "$WITH_PHASE2_DRYRUN" == "true" ]]; then
  phase2_cmd_words=(
    env
    "${env_assignments[@]}"
    bash
    scripts/run_phase2_performance_baseline.sh
    --dry-run
    --iterations 200
    --tls-iterations 50
  )
  run_project_cmd "$(build_project_command "${phase2_cmd_words[@]}")" "${phase2_cmd_words[@]}"
fi

echo "[PASS] macOS openssl path check draft finished"
