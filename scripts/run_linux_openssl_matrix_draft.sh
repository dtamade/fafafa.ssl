#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
WITH_COMPILE=true
WITH_PHASE2_DRYRUN=true
VERBOSE=false
DRY_RUN=false

OPENSSL111_LIB_DIR=""
OPENSSL3_LIB_DIR=""

# 检测候选目录（可按团队环境扩展）
OPENSSL111_CANDIDATES=(
  "/opt/openssl-1.1/lib"
  "/opt/openssl111/lib"
  "/usr/local/openssl-1.1/lib"
  "/usr/local/ssl-1.1/lib"
  "/usr/lib/x86_64-linux-gnu"
)

OPENSSL3_CANDIDATES=(
  "/opt/openssl-3/lib"
  "/usr/local/openssl-3/lib"
  "/usr/local/ssl/lib"
  "/usr/lib/x86_64-linux-gnu"
)

usage() {
  cat <<'USAGE'
Linux OpenSSL 版本矩阵脚本（Draft）

目标：
  为 Linux 提供 OpenSSL 1.1.1 / 3.x 的最小命令矩阵入口。

用法：
  scripts/run_linux_openssl_matrix_draft.sh [options]

选项：
  --openssl111-lib-dir DIR   指定 OpenSSL 1.1.1 库目录（含 libcrypto.so.1.1）
  --openssl3-lib-dir DIR     指定 OpenSSL 3.x 库目录（含 libcrypto.so.3）
  --modules LIST             指定模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --skip-compile             跳过 compile_all_modules
  --skip-phase2-dryrun       跳过 Phase2 baseline dry-run
  --verbose                  模块测试增加 --verbose
  --dry-run                  仅打印将执行的命令
  --help                     显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --openssl111-lib-dir)
      OPENSSL111_LIB_DIR="$2"
      shift 2
      ;;
    --openssl3-lib-dir)
      OPENSSL3_LIB_DIR="$2"
      shift 2
      ;;
    --modules)
      MODULE_SET="$2"
      shift 2
      ;;
    --skip-compile)
      WITH_COMPILE=false
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

find_lib_dir() {
  local target="$1"
  shift
  local dir
  for dir in "$@"; do
    if [[ -f "$dir/$target" ]]; then
      echo "$dir"
      return 0
    fi
  done
  return 1
}

if [[ -z "$OPENSSL111_LIB_DIR" ]]; then
  OPENSSL111_LIB_DIR="$(find_lib_dir "libcrypto.so.1.1" "${OPENSSL111_CANDIDATES[@]}" || true)"
fi

if [[ -z "$OPENSSL3_LIB_DIR" ]]; then
  OPENSSL3_LIB_DIR="$(find_lib_dir "libcrypto.so.3" "${OPENSSL3_CANDIDATES[@]}" || true)"
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

run_cmd() {
  local cmd_desc="$1"
  shift
  echo "    [CMD] $cmd_desc"
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi
  "$@"
}

run_project_cmd() {
  local cmd_desc="$1"
  shift
  echo "    [CMD] $cmd_desc"
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi
  (
    cd "$PROJECT_ROOT"
    "$@"
  )
}

run_profile() {
  local profile="$1"
  local lib_dir="$2"
  local ld_path="${LD_LIBRARY_PATH:-}"
  local profile_env_assignments=()

  echo ""
  echo "==== Profile: $profile ===="

  if [[ -n "$lib_dir" ]]; then
    echo "  lib dir: $lib_dir"
    profile_env_assignments=("LD_LIBRARY_PATH=$lib_dir:${ld_path}")
  else
    echo "  lib dir: (system default)"
  fi

  local openssl_cmd_words=()
  if [[ ${#profile_env_assignments[@]} -gt 0 ]]; then
    openssl_cmd_words=(env "${profile_env_assignments[@]}" openssl version)
  else
    openssl_cmd_words=(openssl version)
  fi
  run_project_cmd "$(build_project_command "${openssl_cmd_words[@]}")" "${openssl_cmd_words[@]}"

  if [[ "$WITH_COMPILE" == "true" ]]; then
    local compile_cmd_words=()
    if [[ ${#profile_env_assignments[@]} -gt 0 ]]; then
      compile_cmd_words=(env "${profile_env_assignments[@]}" python3 scripts/compile_all_modules.py)
    else
      compile_cmd_words=(python3 scripts/compile_all_modules.py)
    fi
    run_project_cmd "$(build_project_command "${compile_cmd_words[@]}")" "${compile_cmd_words[@]}"
  fi

  local module_cmd_words=()
  if [[ ${#profile_env_assignments[@]} -gt 0 ]]; then
    module_cmd_words=(env "${profile_env_assignments[@]}" bash scripts/run_all_module_tests.sh --modules "$MODULE_SET")
  else
    module_cmd_words=(bash scripts/run_all_module_tests.sh --modules "$MODULE_SET")
  fi
  if [[ "$VERBOSE" == "true" ]]; then
    module_cmd_words+=(--verbose)
  fi
  run_project_cmd "$(build_project_command "${module_cmd_words[@]}")" "${module_cmd_words[@]}"

  if [[ "$WITH_PHASE2_DRYRUN" == "true" ]]; then
    local phase2_cmd_words=()
    if [[ ${#profile_env_assignments[@]} -gt 0 ]]; then
      phase2_cmd_words=(
        env
        "${profile_env_assignments[@]}"
        bash
        scripts/run_phase2_performance_baseline.sh
        --dry-run
        --iterations 200
        --tls-iterations 50
      )
    else
      phase2_cmd_words=(
        bash
        scripts/run_phase2_performance_baseline.sh
        --dry-run
        --iterations 200
        --tls-iterations 50
      )
    fi
    run_project_cmd "$(build_project_command "${phase2_cmd_words[@]}")" "${phase2_cmd_words[@]}"
  fi
}

echo "========================================"
echo "fafafa.ssl Linux OpenSSL Matrix (Draft)"
echo "========================================"
echo "modules: $MODULE_SET"
echo "compile: $WITH_COMPILE"
echo "phase2 dry-run: $WITH_PHASE2_DRYRUN"
echo "dry-run: $DRY_RUN"

echo ""
echo "detected openssl 1.1.1 lib dir: ${OPENSSL111_LIB_DIR:-<not found>}"
echo "detected openssl 3.x   lib dir: ${OPENSSL3_LIB_DIR:-<not found>}"

# system profile 总是执行
run_profile "system-default" ""

# openssl3 profile（若可用）
if [[ -n "$OPENSSL3_LIB_DIR" ]]; then
  run_profile "openssl3" "$OPENSSL3_LIB_DIR"
else
  echo "[WARN] openssl3 profile skipped: libcrypto.so.3 not found"
fi

# openssl111 profile（若可用）
if [[ -n "$OPENSSL111_LIB_DIR" ]]; then
  run_profile "openssl111" "$OPENSSL111_LIB_DIR"
else
  echo "[WARN] openssl111 profile skipped: libcrypto.so.1.1 not found"
fi

echo ""
echo "[PASS] linux openssl matrix draft finished"
