#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

PLATFORM="linux"
PROFILE="pr"
GATE_LAYER="L1"
RUN_ID=""
OUTPUT_FILE=""
DRY_RUN=false

usage() {
  cat <<'USAGE'
Gate/Archive 证据模板生成脚本（Draft）

用途：
  生成统一结构的门禁与归档证据 markdown，便于跨平台审阅与归档。

用法：
  scripts/generate_gate_archive_evidence_template_draft.sh [options]

选项：
  --platform NAME     平台（linux|macos|windows，默认: linux）
  --profile NAME      归档配置（pr|nightly|release，默认: pr）
  --gate-layer NAME   主要关注层（L0|L1|L2|L3，默认: L1）
  --run-id ID         运行 ID（默认: yyyyMMdd_HHmmss）
  --output FILE       输出文件路径（默认: docs/test_reports/GATE_ARCHIVE_EVIDENCE_<runid>.md）
  --dry-run           仅打印将生成的信息，不写文件
  --help              显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --platform)
      PLATFORM="$2"
      shift 2
      ;;
    --profile)
      PROFILE="$2"
      shift 2
      ;;
    --gate-layer)
      GATE_LAYER="$2"
      shift 2
      ;;
    --run-id)
      RUN_ID="$2"
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

case "$PLATFORM" in
  linux|macos|windows) ;;
  *)
    echo "[FAIL] unsupported platform: $PLATFORM" >&2
    exit 1
    ;;
esac

case "$PROFILE" in
  pr|nightly|release) ;;
  *)
    echo "[FAIL] unsupported profile: $PROFILE" >&2
    exit 1
    ;;
esac

case "$GATE_LAYER" in
  L0|L1|L2|L3) ;;
  *)
    echo "[FAIL] unsupported gate layer: $GATE_LAYER" >&2
    exit 1
    ;;
esac

if [[ -z "$RUN_ID" ]]; then
  RUN_ID="$(date +"%Y%m%d_%H%M%S")"
fi

resolve_output_path() {
  local path="$1"
  if [[ "$path" == /* ]]; then
    echo "$path"
  else
    echo "$PROJECT_ROOT/$path"
  fi
}

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/GATE_ARCHIVE_EVIDENCE_${RUN_ID}.md"
fi

OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] platform=$PLATFORM profile=$PROFILE gate_layer=$GATE_LAYER"
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Gate & Archive Evidence Report（Draft）

## 1) Metadata

| field | value |
|------|-------|
| report_id | ${RUN_ID} |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| platform | ${PLATFORM} |
| workflow_profile | ${PROFILE} |
| repository | fafafa.ssl |
| run_id | ${RUN_ID} |
| focus_layer | ${GATE_LAYER} |

## 2) Gate Layer Results

| layer | scope | command entry | expected | actual | status |
|------|-------|---------------|----------|--------|--------|
| L0 | 环境预检 | <command> | 依赖可见 | <result> | <pass/fail/skip> |
| L1 | 快速阻断 | <command> | 编译/核心回归通过 | <result> | <pass/fail/skip> |
| L2 | 扩展验证 | <command> | 路径/兼容链路可执行 | <result> | <pass/fail/skip> |
| L3 | 深度验证 | <command> | 矩阵/性能/对照验证 | <result> | <pass/fail/skip> |

## 3) Command Evidence

| # | command | exit code | output report/log |
|---|---------|-----------|-------------------|
| 1 | <command> | <0/non-zero> | <path> |

## 4) Archive Mapping Evidence

| class | profile retention | artifact path | included | notes |
|------|-------------------|---------------|----------|-------|
| core-reports | <days> | <path> | <yes/no> | |
| perf-baseline | <days> | <path> | <yes/no> | |
| docs-evidence | <days> | <path> | <yes/no> | |
| debug-logs | <days> | <path> | <yes/no> | |
| binaries | <days> | <path> | <yes/no> | <optional> |

## 5) Decision

- merge_blocking: <true/false>
- release_blocking: <true/false>
- decision_reason: <one sentence>

## 6) Follow-ups

- <ticket_or_action_1>
- <ticket_or_action_2>

## 7) Attachments

- artifacts/ci/<run_id>/manifest.csv
- artifacts/ci/<run_id>/manifest.md
EOF_REPORT

echo "[PASS] evidence report template generated: $OUTPUT_FILE"
