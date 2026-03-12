#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"
OUTPUT_FILE=""
MSYS2_ROOT="C:/msys64"
DRY_RUN=false
STRICT=false
WITH_SERVER_HANDSHAKE=true

usage() {
  cat <<'USAGE'
Windows WinSSL 阻塞批次收口脚本（Draft）

目标：
  针对 P1-33~P1-36 在 Windows/Win64 RTL 环境执行实机收口批次，
  产出结构化报告与日志证据。

用法：
  scripts/run_windows_winssl_blocker_batch_draft.sh [options]

选项：
  --run-id ID               指定 run_id
  --reports-dir DIR         报告目录（默认: tmp/wave_b_reports）
  --output FILE             输出报告路径
  --msys2-root DIR          指定 MSYS2 根目录（默认: C:/msys64）
  --skip-server-handshake   跳过 P1-36 服务端握手收口步骤
  --dry-run                 仅打印命令，不执行
  --strict                  任一步骤失败返回非 0
  --help                    显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --reports-dir)
      REPORTS_DIR="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --msys2-root)
      MSYS2_ROOT="$2"
      shift 2
      ;;
    --skip-server-handshake)
      WITH_SERVER_HANDSHAKE=false
      shift
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --strict)
      STRICT=true
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

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/winssl_blocker_batch_${RUN_ID}.md"
fi

mkdir -p "$REPORTS_DIR"
mkdir -p "$(dirname "$OUTPUT_FILE")"

p133_log="$REPORTS_DIR/winssl_p133_cert_loading_${RUN_ID}.log"
p134_log="$REPORTS_DIR/winssl_p134_errors_mapping_${RUN_ID}.log"
p135_log="$REPORTS_DIR/winssl_p135_enterprise_${RUN_ID}.log"
p136_log="$REPORTS_DIR/winssl_p136_server_handshake_${RUN_ID}.log"

run_step() {
  local cmd="$1"
  local log="$2"

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] $cmd" > "$log"
    echo "0"
    return
  fi

  set +e
  eval "$cmd" > "$log" 2>&1
  local ec=$?
  set -e
  echo "$ec"
}

UNAME_RAW="$(uname -s 2>/dev/null || echo unknown)"
UNAME_LC="$(echo "$UNAME_RAW" | tr '[:upper:]' '[:lower:]')"
IS_WINDOWS_RUNTIME=false
case "$UNAME_LC" in
  mingw*|msys*|cygwin*) IS_WINDOWS_RUNTIME=true ;;
esac

if [[ "$IS_WINDOWS_RUNTIME" == "false" ]]; then
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[WARN] non-Windows environment detected: $UNAME_RAW (dry-run only)"
  else
    echo "[FAIL] this script is intended for Windows/Win64 RTL (current: $UNAME_RAW)" >&2
    exit 1
  fi
fi

if [[ "$DRY_RUN" == "false" && ! -d "$MSYS2_ROOT" ]]; then
  echo "[FAIL] missing MSYS2 root: $MSYS2_ROOT" >&2
  exit 1
fi

LAZBUILD_EXE="${LAZBUILD_EXE:-lazbuild}"
if ! command -v "$LAZBUILD_EXE" >/dev/null 2>&1; then
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[WARN] missing lazbuild host in PATH (dry-run placeholder): $LAZBUILD_EXE"
  else
    echo "[FAIL] missing lazbuild host in PATH: $LAZBUILD_EXE" >&2
    exit 1
  fi
fi

FPC_EXE="${FPC_EXE:-fpc}"
if ! command -v "$FPC_EXE" >/dev/null 2>&1; then
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[WARN] missing fpc host in PATH (dry-run placeholder): $FPC_EXE"
  else
    echo "[FAIL] missing fpc host in PATH: $FPC_EXE" >&2
    exit 1
  fi
fi

ENV_PREFIX="MSYS2_ROOT='$MSYS2_ROOT'"

p133_cmd="cd '$PROJECT_ROOT/tests/winssl' && $ENV_PREFIX $LAZBUILD_EXE --cpu=x86_64 --os=win64 test_winssl_certificate_loading.lpi && ./bin/test_winssl_certificate_loading.exe"
p134_cmd="cd '$PROJECT_ROOT/tests/winssl' && $ENV_PREFIX $LAZBUILD_EXE --cpu=x86_64 --os=win64 test_winssl_errors_comprehensive.lpi && ./bin/test_winssl_errors_comprehensive.exe"
p135_cmd="cd '$PROJECT_ROOT/tests/winssl' && $ENV_PREFIX $LAZBUILD_EXE --cpu=x86_64 --os=win64 test_winssl_enterprise.lpi && ./bin/test_winssl_enterprise.exe"
p136_cmd="cd '$PROJECT_ROOT' && $ENV_PREFIX $FPC_EXE -Twin64 -Px86_64 -Fu./src -Fu./tests/framework tests/winssl/test_winssl_server_handshake.pas -otests/winssl/bin/test_winssl_server_handshake.exe && ./tests/winssl/bin/test_winssl_server_handshake.exe"

p133_exit="$(run_step "$p133_cmd" "$p133_log")"
p134_exit="$(run_step "$p134_cmd" "$p134_log")"
p135_exit="$(run_step "$p135_cmd" "$p135_log")"

p136_exit="SKIP"
p136_log_display="<none>"
if [[ "$WITH_SERVER_HANDSHAKE" == "true" ]]; then
  p136_exit="$(run_step "$p136_cmd" "$p136_log")"
  p136_log_display="$p136_log"
fi

overall="PASS"
if [[ "$p133_exit" != "0" || "$p134_exit" != "0" || "$p135_exit" != "0" ]]; then
  overall="FAIL"
fi
if [[ "$p136_exit" != "0" && "$p136_exit" != "SKIP" ]]; then
  overall="FAIL"
fi

{
  echo "# Windows WinSSL Blocker Batch (Draft)"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- overall: **$overall**"
  echo "- environment: $UNAME_RAW"
  echo
  echo "## Step Matrix"
  echo
  echo "| step | task | exit | log |"
  echo "|------|------|------|-----|"
  echo "| WinSSL cert loading | P1-33 | $p133_exit | $p133_log |"
  echo "| WinSSL errors mapping | P1-34 | $p134_exit | $p134_log |"
  echo "| WinSSL enterprise path | P1-35 | $p135_exit | $p135_log |"
  echo "| WinSSL server handshake | P1-36 | $p136_exit | $p136_log_display |"
  echo
  echo "## Commands"
  echo
  echo "- P1-33: \`$p133_cmd\`"
  echo "- P1-34: \`$p134_cmd\`"
  echo "- P1-35: \`$p135_cmd\`"
  echo "- P1-36: \`$p136_cmd\`"
} > "$OUTPUT_FILE"

echo "[INFO] overall=$overall"
echo "[PASS] windows winssl blocker batch draft report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
